#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# setwd("C:/Users/User/Documents/rowwisenorm_shiny")

library(shiny)
library(rowwisenorm)  # make available
library(pheatmap)

library(edgeR)  # for VST
library(lumi)  # for VSN
library(preprocessCore)  # for Quantile normalization


# setting upload size to 100 MB max
options(shiny.maxRequestSize=100*1024^2)

# initialize global variables
lowest_level_df <- data.frame()
exp_design <- data.frame()
additional_cols <- data.frame()
lowest_level_norm <- data.frame()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
      h2("Normalize Your Data", align="center",
         style="font-family: Garamond; font-size: 35px; font-weight:350; color:darkblue;"), windowTitle = "Normalize Your Data"
    ),
    # text-shadow: 2px 2px 4px blue;

    # graphic adjustment
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),  # horizontal line thicker
      #tags$script(src = "custom.js")  # not used
    ),

    # color of warnings/ errors inside textOutputs
    tags$head(tags$style("#reading_warning{color: orange;}")),
    tags$head(tags$style("#reading_error{color: red;}")),
    tags$head(tags$style("#normalize_row_warning{color: red;}")),

    # choose method - fluidRow and column() make it occur centered
    fluidRow(
      column(8, align="center", offset=2,
             selectInput(inputId = "method", label = "Method",
                         choices = c("row-wise-normalization" = "row-wise-normalization",
                                     "total-sum" = "total-sum", "VST" = "VST", "VSN" = "VSN",
                                     "quantile-normalization" = "quantile-normalization")),
             textOutput("selected_method"),
             )
    ),
    # hr(),  # horizontal line

    # part the ui in three parts
    tabsetPanel(
      # left tab
      tabPanel("Input and Settings",
               fluidRow(
                 # left
                 column(4,
                        div(
                          h3("Data", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # upload files
                        fileInput(inputId = "data", label = "Upload your data",
                                  accept = c(".csv, .tsv, .txt", "text/*")),
                        textOutput("file_name_data"),

                        fileInput(inputId = "exp_design", label = "Upload the experimental design for your data",
                                  accept = c(".csv, .tsv, .txt", "text/*")),
                        textOutput("file_name_exp_design"),

                        # process button
                        actionButton("process", label = "Process", icon = icon("refresh")),

                       ),
                 # middle
                 column(4,
                        # Preprocessing possible for all methods: - but log2 not for VST allowed (no negative values allowed)
                        div(
                          h3("Preprocessing", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # filtering of reverse, only by site, contaminant
                        selectInput(inputId = "filter", label = "Choose which features should be filtered out",
                                    choices = c("only by site" = "only by site", "reverse" = "reverse",
                                                "contaminant" = "contaminant"), multiple = TRUE),
                        textOutput("selected_filter"),

                        # log2
                        conditionalPanel(
                          condition = "input.method != 'VST' ",
                          checkboxInput(inputId = "log2_t", label = "Logarithmic transformation", value = TRUE),
                          textOutput("log2_transform"),
                        ),

                        # filter_rows
                        checkboxInput(inputId = "filterrows", label = "Filter out rows", value = FALSE),

                        conditionalPanel(
                          condition = "input.filterrows == true",
                          #textInput(inputId = "filterrowsratio", label = "Optionally: Your desired ratio of valid values per row as a decimal between 0 and 1:", placeholder = "Default: 0.5"),
                          sliderInput(inputId = "filterrowsratio", label = "Your desired ratio of valid values per row:", min=0, max=1, value = 0.5, step = 0.01),
                          textOutput("filterrows_note"),
                        ),

                        # sum normalize
                        conditionalPanel(
                          condition = "input.method != 'total-sum' ",
                          checkboxInput(inputId = "sum_norm", label = "Sum normalize", value = FALSE),
                          textOutput("sum_normal"),
                        ),

                        # parameter for sum normalize - same IDs as for total sum since this preprocessing appears for all methods without total sum
                        conditionalPanel(
                          condition = "input.sum_norm & input.method != 'total-sum' ", # important: set off for method total-sum (otherwise, when clicked at a different method it still appears)
                          # refFunc
                          selectInput(inputId = "refFunc_sum", label = "Select the reference Function", choices = c("sum" = "sum", "median" = "median")),
                          textOutput("selected_refFunc_sum_prev"),
                          # norm
                          checkboxInput(inputId = "norm_sum", label = "Normalize the total sum", value = TRUE),
                          # na.rm - this ID only here (needs to work for all methods, the other na_rm is used for total sum and row wise in specific setups)
                          checkboxInput(inputId = "na_rm_sum", label = "Remove NA values inside reference function", value = TRUE),
                        ),

                        # median normalize
                        checkboxInput(inputId = "median_norm", label = "Median normalize", value = FALSE),


                        ### Setups for specific methods:
                        div(
                          h3("Setups", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # active mode - only for row-wise
                        conditionalPanel(
                          condition = "input.method == 'row-wise-normalization' ",
                          checkboxInput(inputId = "active_mode", label = "Manually setting reference channels", value = FALSE),
                          textOutput("selected_mode"),
                        ),

                        # if active is set: input of references - this input is later used for ref parameter
                        conditionalPanel(
                          condition = "input.active_mode == true & input.method == 'row-wise-normalization' ",  # important: set only for row-wise (otherwise, when clicked at a different method it still appears)
                          textInput(inputId = "refs", label = "Please enter the condition names of the references, separated by a comma:")
                        ),

                        textOutput("normalize_row_warning"),  # TODO locate a warning of row-wise here?

                        # na.rm - only for row-wise and total-sum (also specifies na.rm in row-wise function)
                        conditionalPanel(
                          condition = "input.method == 'row-wise-normalization' || input.method == 'total-sum' ",
                          checkboxInput(inputId = "na_rm", label = "Remove NA values inside reference function", value = TRUE),
                          textOutput("selected_na_rm"),
                        ),

                        # refFunc - only for row-wise (because other default)
                        conditionalPanel(
                          condition = "input.method == 'row-wise-normalization' ",
                          selectInput(inputId = "refFunc", label = "Select the reference Function", choices = c("median" = "median", "sum" = "sum")),
                          textOutput("selected_refFunc"),
                        ),


                        # specific parameter for total sum - only for total-sum
                        conditionalPanel(
                          condition = "input.method == 'total-sum' ",
                          # refFunc
                          selectInput(inputId = "refFunc_sum", label = "Select the reference Function", choices = c("sum" = "sum", "median" = "median")),
                          textOutput("selected_refFunc_sum"),
                          # norm
                          checkboxInput(inputId = "norm_sum", label = "Normalize the total sum", value = TRUE),
                          # na.rm - use the same as for row-wise
                          # checkboxInput(inputId = "na_rm_sum", label = "Remove NA values", value = TRUE),
                        ),
                        ),
                 # right
                 column(4,
                        # Notifications (warning and error messages)
                        div(
                          h2("Notifications", style = "font-size: 18px; font-weight:550; color:darkblue"),
                          class = "title-div"
                        ),
                        tags$hr(style="border-color: darkblue;"),  # horizontal line

                        textOutput("reading_error"),  # handle stop() call inside reading
                        textOutput("reading_warning"),  # handle warning()
                       ),
               ),

             ),
      # middle tab
      tabPanel("Download Results",
               fluidRow(
                 # left
                 column(6,
                        # show normalized data
                        div(
                          h3("Values of Normalized Data", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # button for showing data
                        actionButton(inputId = "show_data", label = "Show normalized data"),
                        verbatimTextOutput("data_output"),
                        ),
                 # right
                 column(6,
                        # download
                        div(
                          h3("Write Into File", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # download button for outfile lowest-level
                        downloadButton("download_outfile", "Download Data on Lowest Level"),

                        # download button for outfile with additional columns
                        downloadButton("download_outfile_comp", "Download Data Complete"),

                        # download outfile - manually - could replace div(icon())) with just setting the label
                        checkboxInput(inputId = "writeoutfile", div(icon("star"), "Manually: Download normalized data"), value = FALSE),
                        textOutput("writeout"),

                        conditionalPanel(
                          condition = "input.writeoutfile == true",
                          textInput(inputId = "filename_outfile", label = "Optionally: Your desired file name:"),
                          textInput(inputId = "dir_outfile", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                          selectInput(inputId = "outfile_level", label = "Level of complexity",
                                      choices = c("lowest-level" = "lowest-level", "all-columns" = "all-columns")),
                          actionButton(inputId = "save_outfile", label = "Submit"),
                          verbatimTextOutput("outfile_path")
                        ),

                        div(
                          h3("Download Plots", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # show labels parameter for PCA labels
                        checkboxInput(inputId = "show_labels", label = "Show labels inside PCA plot", value = FALSE),

                        # svg parameter
                        checkboxInput(inputId = "svg", label = "Additionally create SVG files", value = FALSE),


                        # download button for PDF (and svg) raw
                        downloadButton("download_pdf_raw", "Download Plots Raw Data"),

                        # download button for PDF (and svg) normalized
                        downloadButton("download_pdf_norm", "Download Plots Normalized Data"),

                        # download plots raw - manually
                        checkboxInput(inputId = "save_plots_raw", div(icon("star"), "Manually: Save plots for raw data"), value = FALSE),
                        textOutput("saving_plots_raw"),

                        conditionalPanel(
                          condition = "input.save_plots_raw == true",
                          textInput(inputId = "filename_raw", label = "Optionally: Your desired file name:"),
                          textInput(inputId = "dir_raw", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                          actionButton(inputId = "save_pdf_raw", label = "Submit"),
                          verbatimTextOutput("pdf_path_raw")
                        ),

                        # download plots normalized - manually
                        checkboxInput(inputId = "save_plots_norm", div(icon("star"), "Manually: Save plots for normalized data"), value = FALSE),
                        textOutput("saving_plots_norm"),

                        conditionalPanel(
                          condition = "input.save_plots_norm == true",
                          textInput(inputId = "filename_norm", label = "Optionally: Your desired file name:"),
                          textInput(inputId = "dir_norm", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                          actionButton(inputId = "save_pdf_norm", label = "Submit"),
                          verbatimTextOutput("pdf_path_norm")
                        ),

                        )
               ),

             ),
      # right tab
      tabPanel("Show Plots",
               fluidRow(
                 # note: both column widths as 6 instead 4 makes them fill the whole space of page, but plots have not correct height:width ratio then
                 # left - raw
                 column(4,
                        div(
                          h3("Plots of Raw Data", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # show labels parameter for PCA labels
                        checkboxInput(inputId = "show_labels_raw", label = "Show labels inside PCA plot", value = FALSE),

                        # button for showing plots raw data
                        actionButton(inputId = "show_plots_raw", label = "Show plots of raw data"),

                        plotOutput("plot1_raw"),  # set width = "0px", height = "0px" to avoid space to be reserved
                        plotOutput("plot2_raw"),  # ... then would later need at renderPlot again plotOutput
                        plotOutput("plot3_raw"),
                        plotOutput("plot4_raw"),
                        ),
                 # right - normalized
                 column(4,
                        div(
                          h3("Plots of Normalized Data", style = "font-size: 20px; font-weight:750;"),
                          class = "title-div"
                        ),
                        hr(),  # horizontal line

                        # show labels parameter for PCA labels
                        checkboxInput(inputId = "show_labels_norm", label = "Show labels inside PCA plot", value = FALSE),

                        # button for showing plots normalized
                        actionButton(inputId = "show_plots_norm", label = "Show plots of normalized data"),

                        plotOutput("plot1_norm"),
                        plotOutput("plot2_norm"),
                        plotOutput("plot3_norm"),
                        plotOutput("plot4_norm"),
                        ),

               ),

             )
      )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$selected_method <- renderText({
      paste("Your selected method:", input$method)
    })

    # booleans stating if a choice was set - note: hardcoded as it was named above
    boolean_only_by_site <- reactive({
      "only by site" %in% input$filter
    })

    boolean_reverse <- reactive({
      "reverse" %in% input$filter
    })

    boolean_contaminant <- reactive({
      "contaminant" %in% input$filter
    })

    # reading in of uploaded files - using booleans of picked filters
    readin <- reactive({
      # clear for every new call
      output$reading_warning <- renderText({
        NULL
      })
      output$reading_error <- renderText({
        NULL
      })
      req(input$data)
      req(input$exp_design)
      tryCatch({
        return_list <- rowwisenorm::read_files(data = input$data$datapath, design = input$exp_design$datapath,
                                               rm_only_by_site = boolean_only_by_site(), rm_reverse = boolean_reverse(),
                                               rm_contaminant = boolean_contaminant())

        return(return_list)
      }, warning = function(w) {  # print first warning
        output$reading_warning <- renderText({
          paste(w$message, " - Attention: Some other feature may also not have been found.")
        })  # TODO maybe loop over warnings
        return(return_list)
      }, error = function(e) {  # when reading sanity check fails
        output$reading_error <- renderText({
          paste(e$message, " - Please upload the correct files before continuing.")
        })
        return(NULL)  # in case sanity checks fail, return NULL -> is.null in the following always checked
      })

    })


    # print note when filter rows is set
    output$filterrows_note <- renderText({
      if (input$filterrows){
        return("This is an exclusive lower bound.")
      }
      else {
        return(NULL)
      }
    })


    # helper function preprocessing
    preprocess <- function(lowest_level_df, do_log=F, do_filter=F, do_sum=F, do_median=F){
      # preprocessing: when log2 is set
      if(do_log){
        lowest_level_df <- rowwisenorm::log2_transform(lowest_level_df = lowest_level_df)
      }

      # preprocessing: when filter rows is set
      if(do_filter){
        # slider input is automatically a numeric between 0 and 1 - no warnings possible
        lowest_level_df <- rowwisenorm::filter_rows(lowest_level_df, input$filterrowsratio)
      }

      # preprocessing: when sum normalize is set
      if(do_sum){
        lowest_level_df <- rowwisenorm::sum_normalize(lowest_level_df, refFunc = input$refFunc_sum,
                                                      norm = input$norm_sum, na.rm = input$na_rm_sum)
      }

      # preprocessing: when median normalize is set
      if(do_median){
        lowest_level_df <- rowwisenorm::median_normalize(lowest_level_df)
      }
      return(lowest_level_df)
    }

    # BUTTON PROCESS: when clicked, new read in, new preprocess, new check method and do normalization
    # save lowest level df, the other return list elements and the lowest level norm all in variables
    # defined outside of reactives/ ... -> set them new every time when clicked, use them everywhere else
    # clean everything when button clicked

    # important: assign values for global variables with <<- and not <-
    observeEvent(input$process, {
      # set empty
      lowest_level_df <<- data.frame()
      exp_design <<- data.frame()
      additional_cols <<- data.frame()
      lowest_level_norm <<- data.frame()

      # TODO set other fields empty
      output$data_output <- renderPrint({ NULL })  # show data field

      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <<- return_list[["lowest_level_df"]]
        exp_design <<- return_list[["exp_design"]]
        additional_cols <<- return_list[["additional_cols"]]

        print(head(lowest_level_df))
        print("button clicked")
        print(head(lowest_level_norm))
        print("button clicked before norm")
        # pre-processing (included in each normalization) and normalization
        if(input$method == "row-wise-normalization"){
          lowest_level_norm <<- normalize_rowwise(lowest_level_df, exp_design)
        }
        else if(input$method == "total-sum"){
          lowest_level_norm <<- normalize_totalsum(lowest_level_df)
        }
        else if(input$method == "VST"){
          lowest_level_norm <<- normalize_vst()
        }
        else if(input$method == "VSN"){
          lowest_level_norm <<- normalize_vsn()
        }
        else if(input$method == "quantile-normalization"){
          lowest_level_norm <<- normalize_quantile()
        }

        print(head(lowest_level_norm))
        print("button clicked norm")
      }
    })


    # NORMALIZATION row-wise
    normalize_rowwise <- function(lowest_level_df, exp_design){
      # clear for every new call
      output$normalize_row_warning <- renderText({ NULL })

      # preprocessing
      lowest_level_df_pre <- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        if(input$active_mode){  # when active was set, the input for refs is taken as ref parameter
          input_string <- input$refs
          input_vector <- NULL
          if (!is.null(input_string) && input_string != "") {
            input_vector <- unlist(strsplit(input_string, ",\\s*"))
            input_vector <- trimws(input_vector)
          }
          lowest_level_norm <- rowwisenorm::normalize_row(lowest_level_df = lowest_level_df_pre,
                                                          exp_design = exp_design, ref = input_vector,
                                                          na.rm = input$na_rm, refFunc = input$refFunc)
        }
        else {  # no refs are set manually, automatic mode
          lowest_level_norm <- rowwisenorm::normalize_row(lowest_level_df = lowest_level_df_pre,
                                                          exp_design = exp_design,
                                                          na.rm = input$na_rm, refFunc = input$refFunc)
        }
        return(lowest_level_norm)
      }, warning = function(w) {  # print warning
        output$normalize_row_warning <- renderText({
          paste(w$message)
        })
        return(lowest_level_norm)  # still return the empty data frame that normalize_row returns in case of warning
      })

    }

    # NORMALIZATION total sum
    normalize_totalsum <- function(lowest_level_df){

      # preprocessing - no sum normalize
      lowest_level_df_pre <- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = F, do_median = input$median_norm)
      lowest_level_norm <- rowwisenorm::sum_normalize(lowest_level_df_pre, refFunc = input$refFunc_sum,
                                                      norm = input$norm_sum, na.rm = input$na_rm)
      return(lowest_level_norm)

    }

    # NORMALIZATION VST
    normalize_vst <- reactive({
      vst_normalized_data <- data.frame()

      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <- return_list[["lowest_level_df"]]

        # preprocessing - no log2 (no negatives allowed)
        lowest_level_df <- preprocess(lowest_level_df, do_log = F, do_filter = input$filterrows,
                                      do_sum = input$sum_norm, do_median = input$median_norm)

        lowest_level_df_comp <- lowest_level_df[complete.cases(lowest_level_df), ]  # without missing values
        lowest_level_df_matrix <- as.matrix(lowest_level_df_comp[! colnames(lowest_level_df_comp) %in% "row.number"])  # convert to matrix and exclude ID column
        # calculation
        dge_list <- DGEList(counts = lowest_level_df_matrix)  # allows no negative counts (do not log2)
        vst_normalized_data <- calcNormFactors(dge_list)
        vst_normalized_data <- cpm(vst_normalized_data)
        # back convert and set column names back
        vst_normalized_data <- as.data.frame(vst_normalized_data)
        colnames(vst_normalized_data) <- colnames(lowest_level_df_comp[! colnames(lowest_level_df_comp) %in% "row.number"])
        vst_normalized_data <- cbind("row.number" = lowest_level_df_comp$row.number, vst_normalized_data)  # back append ID column

        return(vst_normalized_data)
      }
    })

    # NORMALIZATION VSN
    normalize_vsn <- reactive({
      vsn_normalized_data <- data.frame()

      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <- return_list[["lowest_level_df"]]

        # preprocessing
        lowest_level_df <- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                      do_sum = input$sum_norm, do_median = input$median_norm)

        lowest_level_df_matrix <- as.matrix(lowest_level_df[! colnames(lowest_level_df) %in% "row.number"])  # convert to matrix and exclude ID column
        lowest_level_df_matrix[is.nan(lowest_level_df_matrix)] <- NA  # safety check that no NaN but only NA present
        # Perform VSN normalization
        vsn_normalized_data <- normalizeVSN(lowest_level_df_matrix)
        # back convert and set column names back
        vsn_normalized_data <- as.data.frame(vsn_normalized_data)
        colnames(vsn_normalized_data) <- colnames(lowest_level_df[! colnames(lowest_level_df) %in% "row.number"])
        vsn_normalized_data <- cbind("row.number" = lowest_level_df$row.number, vsn_normalized_data)  # back append ID column

        return(vsn_normalized_data)
      }
    })

    # NORMALIZATION Quantile
    normalize_quantile <- reactive({
      quantile_normalized <- data.frame()

      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <- return_list[["lowest_level_df"]]

        # preprocessing
        lowest_level_df <- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                      do_sum = input$sum_norm, do_median = input$median_norm)

        lowest_level_df_matrix <- as.matrix(lowest_level_df[! colnames(lowest_level_df) %in% "row.number"])  # convert to matrix and exclude ID column
        quantile_normalized <- preprocessCore::normalize.quantiles(lowest_level_df_matrix)
        # back convert and set column names back
        quantile_normalized <- as.data.frame(quantile_normalized)
        colnames(quantile_normalized) <- colnames(lowest_level_df[! colnames(lowest_level_df) %in% "row.number"])
        quantile_normalized <- cbind("row.number" = lowest_level_df$row.number, quantile_normalized)  # back append ID column

        return(quantile_normalized)
      }
    })


    # show data - EXECUTION of normalization
    observeEvent(input$show_data, {

      output$data_output <- renderPrint({
        cat("Head of the DataFrame:\n")
        print(head(lowest_level_norm))
      })

    })


    # save PDF manually
    observeEvent(input$save_pdf_raw, {
      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <- return_list[["lowest_level_df"]]
        exp_design <- return_list[["exp_design"]]

        # show labels parameter
        if (input$show_labels) show_lab <- T else show_lab <- F

        # svg parameter
        if (input$svg) make_svg <- T else make_svg <- F

        rowwisenorm::plot_results(lowest_level_df = lowest_level_df, exp_design = exp_design,
                                  main = input$filename_raw, output_dir = input$dir_raw,
                                  show_labels = show_lab, svg = make_svg)

        # output message stating where the file was saved
        if (input$dir_raw != "")  dir_path <- input$dir_raw else dir_path <- "current working directory"
        if (input$filename_raw != "") file_name <- paste(input$filename_raw, ".pdf", sep = "") else file_name <- "results.pdf"  # note: hard coded as stated in plot_results
        output$pdf_path_raw <- renderText({
          paste("PDF saved to ", dir_path, " with file name ", file_name)
        })
      }

    })

    observeEvent(input$save_pdf_norm, {
      return_list <- readin()
      if (! is.null(return_list)){
        exp_design <- return_list[["exp_design"]]

        if(input$method == "row-wise-normalization"){
          lowest_level_norm <- normalize_rowwise()
        }
        else if(input$method == "total-sum"){
          lowest_level_norm <- normalize_totalsum()
        }
        else if(input$method == "VST"){
          lowest_level_norm <- normalize_vst()
        }
        else if(input$method == "VSN"){
          lowest_level_norm <- normalize_vsn()
        }
        else if(input$method == "quantile-normalization"){
          lowest_level_norm <- normalize_quantile()
        }

        # show labels parameter
        if (input$show_labels) show_lab <- T else show_lab <- F

        # svg parameter
        if (input$svg) make_svg <- T else make_svg <- F

        rowwisenorm::plot_results(lowest_level_df = lowest_level_norm, exp_design = exp_design,
                                  main = input$filename_norm, output_dir = input$dir_norm,
                                  show_labels = show_lab, svg = make_svg)

        # output message stating where the file was saved
        if (input$dir_norm != "")  dir_path <- input$dir_norm else dir_path <- "current working directory"
        if (input$filename_norm != "") file_name <- paste(input$filename_norm, ".pdf", sep = "") else file_name <- "results.pdf"  # note: hard coded as stated in plot_results
        output$pdf_path_norm <- renderText({
          paste("PDF saved to ", dir_path, " with file name ", file_name)
        })
      }

    })

    # download PDFs
    output$download_pdf_raw <- downloadHandler(
      filename = function(){
        if (input$svg){
          "results.zip"
        }
        else {
          "results.pdf"
        }
      },
      content = function(file) {
        return_list <- readin()
        if (! is.null(return_list)){
          lowest_level_df <- return_list[["lowest_level_df"]]
          exp_design <- return_list[["exp_design"]]

          # show labels parameter
          if (input$show_labels) show_lab <- T else show_lab <- F

          # svg parameter
          if (input$svg) make_svg <- T else make_svg <- F

          # Save file with a progress indicator (only to get progress message, also works to just generate and save)
          withProgress(
              message = 'Generating file(s)...',
              detail = 'This may take a moment...',
              value = 0, {

                original_working_directory <- getwd()

                # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
                mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
                dir.create(mytemp)

                # Generate the PDF and SVG files in the temporary directory
                rowwisenorm::plot_results(lowest_level_df, exp_design, output_dir = mytemp, show_labels = show_lab, svg = make_svg)
                Sys.sleep(0.5)

                setwd(mytemp)

                if (make_svg){
                  # make zip of the files inside temporary directory
                  zip_file <- "results.zip"
                  zip(zip_file, files = c("results.pdf", "results01.svg", "results02.svg", "results03.svg", "results04.svg"))

                  # Move the ZIP archive to the chosen location
                  file.rename(zip_file, file)
                }
                else {
                  file.rename("results.pdf", file)
                }

                setwd(original_working_directory)  # set back to original working directory

                # remove the temporary directory
                unlink(mytemp, recursive = TRUE)
              }
          )

        }
      }
    )

    output$download_pdf_norm <- downloadHandler(
      filename = function(){
        if (input$svg){
          "results.zip"
        }
        else {
          "results.pdf"
        }
      },
      content = function(file) {
        return_list <- readin()
        if (! is.null(return_list)){
          exp_design <- return_list[["exp_design"]]

          if(input$method == "row-wise-normalization"){
            lowest_level_norm <- normalize_rowwise()
          }
          else if(input$method == "total-sum"){
            lowest_level_norm <- normalize_totalsum()
          }
          else if(input$method == "VST"){
            lowest_level_norm <- normalize_vst()
          }
          else if(input$method == "VSN"){
            lowest_level_norm <- normalize_vsn()
          }
          else if(input$method == "quantile-normalization"){
            lowest_level_norm <- normalize_quantile()
          }

          # show labels parameter
          if (input$show_labels) show_lab <- T else show_lab <- F

          # svg parameter
          if (input$svg) make_svg <- T else make_svg <- F

          # Save file with a progress indicator (only to get progress message, also works to just generate and save)
          withProgress(
            message = 'Generating file(s)...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the PDF and SVG files in the temporary directory
              rowwisenorm::plot_results(lowest_level_norm, exp_design, output_dir = mytemp, show_labels = show_lab, svg = make_svg)
              Sys.sleep(0.5)

              setwd(mytemp)

              if (make_svg){
                # make zip of the files inside temporary directory
                zip_file <- "results.zip"
                zip(zip_file, files = c("results.pdf", "results01.svg", "results02.svg", "results03.svg", "results04.svg"))

                # Move the ZIP archive to the chosen location
                file.rename(zip_file, file)
              }
              else {
                file.rename("results.pdf", file)
              }

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )

        }
      }
    )

    # show plots
    observeEvent(input$show_plots_raw, {
      return_list <- readin()
      if (! is.null(return_list)){
        lowest_level_df <- return_list[["lowest_level_df"]]
        exp_design <- return_list[["exp_design"]]

        output$plot1_raw <- renderPlot({
          rowwisenorm::plot_correlations(lowest_level_df)
        })
        output$plot2_raw <- renderPlot({
          rowwisenorm::plot_heatmap(lowest_level_df)
        })
        output$plot3_raw <- renderPlot({
          rowwisenorm::pcaPlot(lowest_level_df)
        })
        output$plot4_raw <- renderPlot({
          # show labels parameter
          if (input$show_labels_raw) show_lab <- T else show_lab <- F
          rowwisenorm::pcaPlot2(lowest_level_df, exp_design, show_labels = show_lab)
        })
        updateTabsetPanel(session, "plots_tabset_raw", selected = "Plots Raw Data")
      }
    })

    observeEvent(input$show_plots_norm, {
      return_list <- readin()
      if (! is.null(return_list)){
        exp_design <- return_list[["exp_design"]]

        if(input$method == "row-wise-normalization"){
          lowest_level_norm <- normalize_rowwise()
        }
        else if(input$method == "total-sum"){
          lowest_level_norm <- normalize_totalsum()
        }
        else if(input$method == "VST"){
          lowest_level_norm <- normalize_vst()
        }
        else if(input$method == "VSN"){
          lowest_level_norm <- normalize_vsn()
        }
        else if(input$method == "quantile-normalization"){
          lowest_level_norm <- normalize_quantile()
        }

        output$plot1_norm <- renderPlot({
          rowwisenorm::plot_correlations(lowest_level_norm)
        })
        # (*) handling button for new window, use javascript file custom.js or use shinyjs
        # observeEvent(input$openPlotWindow, {
        #   #session$sendCustomMessage(type = "openNewWindow", message = "plot1_norm")
        #   shinyjs::runjs("var win = window.open('', '_blank'); win.document.write($('#plot1_norm').html());")
        #
        # })
        output$plot2_norm <- renderPlot({
          rowwisenorm::plot_heatmap(lowest_level_norm)
        })
        output$plot3_norm <- renderPlot({
          rowwisenorm::pcaPlot(lowest_level_norm)
        })
        output$plot4_norm <- renderPlot({
          # show labels parameter
          if (input$show_labels_norm) show_lab <- T else show_lab <- F
          rowwisenorm::pcaPlot2(lowest_level_norm, exp_design, show_labels = show_lab)
        })

        updateTabsetPanel(session, "plots_tabset_norm", selected = "Plots Normalized Data")
      }

    })


    # save outfile manually
    observeEvent(input$save_outfile, {
      return_list <- readin()
      if (! is.null(return_list)){
        if(input$method == "row-wise-normalization"){
          lowest_level_norm <- normalize_rowwise()
        }
        else if(input$method == "total-sum"){
          lowest_level_norm <- normalize_totalsum()
        }
        else if(input$method == "VST"){
          lowest_level_norm <- normalize_vst()
        }
        else if(input$method == "VSN"){
          lowest_level_norm <- normalize_vsn()
        }
        else if(input$method == "quantile-normalization"){
          lowest_level_norm <- normalize_quantile()
        }

        if(input$outfile_level == "lowest-level"){
          rowwisenorm::write_outfile(lowest_level_df = lowest_level_norm,
                                     filename = input$filename_outfile, output_dir = input$dir_outfile)
          # output message
          if (input$dir_outfile != "")  dir_path <- input$dir_outfile else dir_path <- "current working directory"
          if (input$filename_outfile != "") file_name <- paste(input$filename_outfile, ".csv", sep = "") else file_name <- "output.csv"  # note: hard coded as stated in write_outfile
        }
        else if(input$outfile_level == "all-columns"){
          return_list <- readin()
          if (! is.null(return_list)){
            additional_cols <- return_list[["additional_cols"]]
          }
          rowwisenorm::write_outfile(lowest_level_df = lowest_level_norm, additional_cols = additional_cols,
                                     filename = input$filename_outfile, output_dir = input$dir_outfile)
          # output message
          if (input$dir_outfile != "")  dir_path <- input$dir_outfile else dir_path <- "current working directory"
          if (input$filename_outfile != "") file_name <- paste(input$filename_outfile, ".csv", sep = "") else file_name <- "output_complete.csv"  # note: hard coded as stated in write_outfile
        }
        output$outfile_path <- renderText({
          paste("Output saved to ", dir_path, " with file name ", file_name)
        })
      }

    })

    # download outfile lowest level
    output$download_outfile <- downloadHandler(
      filename = function(){
        "output.csv"
      },
      content = function(file) {
        return_list <- readin()
        if (! is.null(return_list)){
          if(input$method == "row-wise-normalization"){
            lowest_level_norm <- normalize_rowwise()
          }
          else if(input$method == "total-sum"){
            lowest_level_norm <- normalize_totalsum()
          }
          else if(input$method == "VST"){
            lowest_level_norm <- normalize_vst()
          }
          else if(input$method == "VSN"){
            lowest_level_norm <- normalize_vsn()
          }
          else if(input$method == "quantile-normalization"){
            lowest_level_norm <- normalize_quantile()
          }

          # Save file with a progress indicator
          withProgress(
            message = 'Generating file...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the file
              rowwisenorm::write_outfile(lowest_level_norm, output_dir = mytemp)
              Sys.sleep(0.5)  # Simulate some work for the progress bar

              setwd(mytemp)

              # Move the generated file to the specified location
              file.rename("output.csv", file)

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )
        }
      }
    )

    # download outfile with additional columns
    output$download_outfile_comp <- downloadHandler(
      filename = function(){
        "output_complete.csv"
      },
      content = function(file) {
        return_list <- readin()
        if (! is.null(return_list)){
          additional_cols <- return_list[["additional_cols"]]

          if(input$method == "row-wise-normalization"){
            lowest_level_norm <- normalize_rowwise()
          }
          else if(input$method == "total-sum"){
            lowest_level_norm <- normalize_totalsum()
          }
          else if(input$method == "VST"){
            lowest_level_norm <- normalize_vst()
          }
          else if(input$method == "VSN"){
            lowest_level_norm <- normalize_vsn()
          }
          else if(input$method == "quantile-normalization"){
            lowest_level_norm <- normalize_quantile()
          }

          # Save file with a progress indicator
          withProgress(
            message = 'Generating file...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the file
              rowwisenorm::write_outfile(lowest_level_norm, additional_cols = additional_cols)
              Sys.sleep(0.5)  # Simulate some work for the progress bar

              setwd(mytemp)

              # Move the generated file to the specified location
              file.rename("output_complete.csv", file)

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )
        }
      }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
