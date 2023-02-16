## Self-Service Instructions

SI backstops have made the COP supporting visualizations available via email the week of February 13, 2023. These visuals are static and largely focused on all of PEPFAR for a given country. Teams may want to see the data for just USAID or may want to adjust the output visuals in different ways. We have laid out some instructions to help guide the way if you are using R. If you have any questions along the way, feel free to reach out to use at [si.coreanalytics@usaid.gov](si.coreanalytics@usaid.gov)

#### Pre Work

To get started, you'll need to install the packages used for the plots.

First, start with packages on CRAN

```
  install.packages("tidyverse")
  install.packages("glue")
  install.packages("scales")
  install.packages("extrafont")
  install.packages("patchwork")
  install.packages("ggtext")
  install.packages("googlesheets4")
  install.packages("ggrepel")
  install.packages("janitor")
  install.packages("here")
  install.packages("assertr")
  
```

Next up, you'll need a number of packages the SI team has developed that need to be installed from GitHub.

```
  install.packages("remotes")
  remotes::install_github("mindthegap")
  remotes::install_github("gagglr")
  remotes::install_github("cascade", ref = "dev") # Use dev version
  remotes::install_github("selfdestructin5")
```
You will also want to install the font we use, Source Sans Pro. Instructions for that can be found on the [`glitr` vignette page](https://usaid-oha-si.github.io/glitr/articles/what-the-f_nt.html).

And last you should store your email locally for use with the Google Drive API.

```
glamr::set_email("rshah@usaid.gov")
```

#### Store the Data

We use a number of [different data sources](Documents/Data_Sources.md) for this project, both PEPFAR and external sources. In order to completely recreate what is found here, you will need to download the following files.

  - PSNUxIM MSD (from Genie or Panorama)
  - Financial Structured Dataset (from Panorama)
  - NAT_SUBNAT MSD (from Panorama)
  - HRH Structured Dataset (complete [this form](https://drive.google.com/file/d/1iAyEAXDv545yoKQngBABXebaQYZu_1Cr/view?usp=sharing) and send to Jason Roffenbender for access to the non-redacted, clean, adjusted dataset)
  
The other sources - SID, UNAIDS data, and HIV Policy Lab - are stored on Google Drive and just require you store your USAID email address as noted above. 
  
If you only care about certain visuals, check the source information in the caption of the original plot and just make sure those sources are available.

These local data source should ideally be stored in a central location. You can designate where that is using the code below. If the files are in different locations, you will need to replace all of the `si_path()` refernces in [`91_setup.R`](Scripts/91_setup.R) with the correct, full path to the data.

```
glamr::set_paths(folderpath_msd = "~/Documents/Data")
```

#### Data Import and Munging

Now that the data have been stored, you are ready to start working with the data. Rather than having each plot import and munge data across more than 15 plots, this work is done in a prep-script called [`91_setup.R`](Scripts/91_setup.R). You can run this script in its entirety to load and filter/adjust all the requisite datasets. For datasets that are not available, the script will load a blank dataset.


#### Re-generating the Visuals

For this project, we use an [Rmarkdown file](Scripts/country_report.Rmd) to knit together all the various plots into one html output. In order to run anything in this file, you will need to have gone through the last step of loading and munging the data by running  [`91_setup.R`](Scripts/91_setup.R).

After the setup script is run, open up [`country_report.Rmd`](Scripts/country_report.Rmd) and change the parameters (`params`) at the top to match what you want, e.g. replacing `cntry: "Malawi"` with `cntry: Kenya`. The `agency` for the original report was `PEPFAR`, but this can be changed to reflect `USAID`. 

**Re-creating the whole report** - At the very top of of the script editor, there is a button that reads "Knit". If you click on this, it will run through all the code in this and output an html file (you will need to open this in an internet browser).

**Re-running one-off visuals** - If you just want to re-create one visual, start by hitting the green "play" button on line 16, which essentially is loading the parameters you have set in lines 10-12. Next, navigate to any of the question sections you want to recreate and do the same thing - high the green play button on the right hand side to generate the plot. This will create a preview output in your IDE. To export the file, in your console, use `si_save("~/COP23/Images/epi-control.png")` to save the png output as a high resolution (330 dpi) image the dimensions of a Google Slide (10x5.625).

**Adjusting the plot**
If you want more freedom in adjusting the file output, you can CTRL + Click on a function to see what the actual code is behind the munging and plot. From here, you can adjust as necessary to get the data and visual to what you want. These are function, so you will need to highlight the whole function and hit run to save it in your environment (replacing the original function).

