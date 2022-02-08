#' @title Combine clean raw diet data 
#' @description  Raw diet files as CSV files with ready to import data
#' @details INPUT: 1) diet data for Puget Sound
#' @details OUTPUT: 1) data files
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


get_raw_data <- function(thisfile){
  
  print(thisfile)
  
    this.data <- fread(thisfile) %>% 
      as_tibble %>% 
      filter(atlantis_prey_group =="HE")
  
  return(this.data)
  
}