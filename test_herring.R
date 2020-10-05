#' @title Select diet files with Herring as prey
#' @description  Read file and test if herring are prey
#' @details INPUT: 1) diet data for Puget Sound
#' @details OUTPUT: 1) Clean file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


test_herring <- function(this.file){
  
  this.file.name <- this.file %>% 
  str_split("/") %>% 
  unlist %>% 
  .[6]

print(this.file.name)

this.data <- read_xlsx(this.file) %>% 
  filter(atlantis_prey_group =="HE") 

if(nrow(this.data)>0){
  
  print(paste(this.file.name, "has herring as prey"))
  dir.create("herring_data")
  file.copy(from=this.file,to="/home/atlantis/herringdiet/herring_data/",recursive = FALSE, 
            copy.mode = TRUE)
  
}

}