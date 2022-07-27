
   # devtools::install_github("ropenscilabs/rnaturalearth")
   # devtools::install_github("ropenscilabs/rnaturalearthdata")
   # install.packages("rnaturalearthhires",
   #                repos = "http://packages.ropensci.org",
   #                type = "source")
#"tidygeocoder"
#devtools::install_github("laresbernardo/lares")

# List of packages for session
.packages = c("devtools","stringi","data.table","tidyverse","readxl","rlang","ggspatial","rgdal",
              "stringr","sf","raster","Redmonder","lubridate","ggs","grid",
              "gridExtra","mgcv","fitdistrplus", "geoR", "mgcViz","itsadug","rnaturalearth",
              "rnaturalearthdata","cowplot","lares","car","broom","gratia","formula.tools","vegan", 
              "factoextra","ggfortify","corrplot","paran", "patchwork","ggforce","AICcmodavg", "spData")

#devtools::install_github("vr-vr/itsadug", build_vignettes=TRUE)
#library(itsadug)

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)
