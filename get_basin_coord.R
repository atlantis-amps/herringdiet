#' @title Assign coordinates to basins, also checks coordinates are in Puget Sound
#' @description  Function to intersect coordinates to basin shapefile
#' @details INPUT: 1) coordinates for basin, 2) herring data
#' @details OUTPUT: 1) herring data with basin field
#' @details Basin file from Washington Department of Fish and Wildlife. Puget Sound Basins (WDFW). ERMA Northwest. Retrieved November 3, 2020, from https://erma.noaa.gov/northwest/erma.html#/x=- 123.44039&y=48.39419&z=8&layers=16+7531
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


get_basin_coord <- function(herring.locations, boundary.file){
  
herring.data <- read_csv(herring.locations) 

herring.coords <- herring.data

coordinates(herring.coords) <- c("longitude","latitude")

target.crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(herring.coords)  <- target.crs

boundary.shape <- readOGR(paste0("~/herringdiet/shapefiles/",boundary.file))
boundary.shape.crs <- spTransform(boundary.shape, target.crs)

coord.intersect <- raster::intersect(herring.coords, boundary.shape.crs)

coord.data <- coord.intersect@data %>% 
  tbl_df() %>% 
  dplyr::select(-WEB_GEOMET, -CNT_NAME, -SUM_AREA) %>% 
  rename(basin = NAME)

return(coord.data)

}