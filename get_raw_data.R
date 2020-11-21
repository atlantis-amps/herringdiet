#' @title Combine clean raw diet data 
#' @description  Raw diet files as CSV files with ready to import data
#' @details INPUT: 1) diet data for Puget Sound
#' @details OUTPUT: 1) data files
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


get_raw_data <- function(thisfile){
  
  this.data <- read_csv(thisfile) %>% 
    filter(atlantis_prey_group =="HE") %>% 
    mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
           lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
    mutate(latitude = if_else(lat_d>0,(lat_d + lat_m/60 + lat_s/3600),lat_d)) %>% 
    mutate(longitude = if_else(lon_d>0,(lon_d + lon_m/60 + lon_s/3600),lon_d)) %>% 
    dplyr::select(-starts_with("lat_"),-starts_with("lon_")) %>% 
    mutate(longitude = if_else(longitude>1,longitude*-1,longitude)) %>% 
    mutate(latitude = if_else(latitude<1,latitude*-1,latitude))
  
  return(this.data)
  
}