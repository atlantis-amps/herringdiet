
#   devtools::install_github("ropenscilabs/rnaturalearth")
#   devtools::install_github("ropenscilabs/rnaturalearthdata")
#   install.packages("rnaturalearthhires",
#                  repos = "http://packages.ropensci.org",
#                  type = "source")
#"tidygeocoder"

# List of packages for session
.packages = c("devtools","stringi","data.table","tidyverse","readxl","rlang","ggspatial","rgdal",
              "stringr","sf","raster","Redmonder","lubridate","ggs",
              "gridExtra","mgcv","fitdistrplus", "geoR")

devtools::install_github("vr-vr/itsadug", build_vignettes=TRUE)
library(itsadug)

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)
