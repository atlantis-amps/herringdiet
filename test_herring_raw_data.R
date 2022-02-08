#' @title Select raw diet data files with Herring as prey
#' @description  Read file and test if herring are prey
#' @details INPUT: 1) diet data for Puget Sound
#' @details OUTPUT: 1) Clean file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


test_herring_raw_data <- function(this.file){
  
  this.file.name <- this.file %>% 
  str_split("/") %>% 
  unlist %>% 
  .[6]

print(this.file.name)

this.data <- read_csv(this.file) %>% 
  filter(atlantis_prey_group =="HE") 

if(nrow(this.data)>0){
  
  print(paste(this.file.name, "has herring as prey"))
  dir.create("herring_raw_data")
  file.copy(from=this.file,to="/home/atlantis/herringdiet/herring_raw_data/",recursive = FALSE, 
            copy.mode = TRUE)
  
}

}