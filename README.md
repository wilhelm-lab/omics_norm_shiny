# omics_norm_shiny
R Shiny Application for Omics Data Normalization 

**Description:**
This is an R Shiny Application which allows omics data to be uploaded and normalized. The application offers some pre-processing steps as well as the ability to view and download the normalized data. More than that, the results can also be displayed in the form of plots. The plots can also be downloaded as a PDF file and SVG files.

The available normalization methods include row-wise normalization, total sum normalization, VST, VSN, quantile normalization, ComBat, and M-ComBat.

**Important for Linux:**
Add the directory where ```zip``` executable is located to the system's ```PATH``` variable.
Alternatively: Specify the path to ```zip``` executable in the environment variable ```R_ZIPCMD```. 
This can be done by the following line of code in R: 
```
Sys.setenv("R_ZIPCMD" = "path/to/zip")
```

**Usage:**

Terminal:
```
Rscript app.R 
```

Optional: The parameter 'local' can be added to obtain additional features inside the application.

Terminal:
```
Rscript app.R local 
```

**Important:** This is only recommended in case the application is used locally.

**More detailed description:**

The application is divided into three tabs.
1. First, there is a tab that includes the upload of data and the settings. In this place, the data can be processed, and the normalization can be performed. 
2. Following this, there is a tab allowing the user to have a preview of the normalized values, download the normalized data, and download plots of the raw data as well as plots of the normalized data. 
3. The third tab can be used to directly view the plots inside the shiny app. This symbolizes an easy way to determine whether the normalization helped reduce batch effects in comparison with the raw data.
