#' @title Geocode locations
#' @description  Read sampling locations without long lat
#' @details INPUT: 1) list of locations
#' @details https://cran.r-project.org/web/packages/tidygeocoder/vignettes/tidygeocoder.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
 


get_location <- function(eachlocation) {
  
  print(eachlocation)
  
  location.frame <- geo(address = eachlocation, method = 'osm', 
                        lat = latitude, long = longitude)
  
  print(location.frame)
  
  return(location.frame)
  
}




