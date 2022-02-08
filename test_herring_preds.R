#' @title Select data for herring predators
#' @description  Select diet files that include herring predators but do not include herring as prey
#' @details INPUT: 1) diet data for Puget Sound
#' @details OUTPUT: 1) diet data for Puget Sound
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


test_herring_preds <- function(this.file,herring.predators){
  
  this.file.name <- this.file %>% 
  str_split("/") %>% 
  unlist %>% 
  .[6]

print(this.file.name)

this.data <- read_xlsx(this.file) 

this.data.he <- this.data %>% 
  filter(atlantis_prey_group == "HE")
  
if(nrow(this.data.he)==0){
  
this.data.pred <- this.data %>% 
    filter(atlantis_pred_group %in% herring.predators) 
  
if(nrow(this.data.pred)>0){
  
  this.data.mod <- this.data %>% 
    mutate(variable=NA_character_,predator_size=NA,size_units=NA_character_,sample_source=NA_character_)
  
  write_csv(this.data.mod, paste0("/home/atlantis/herringdiet/herring_pred_data/",gsub(".xlsx",".csv",this.file.name)))

  print(paste(this.file.name, "has herring predators"))
  
}
}

}