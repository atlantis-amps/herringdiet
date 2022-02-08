#' @title Clean pred files
#' @description  Clean files that have herring predators but do not report herring
#' @details INPUT: 1) list of files 
#' @details OUTPUT: 1) Clean files
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



clean_herringpreddata <- function(this.file){
  
  print(this.file)
  
  
  this.file.name <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[6]
  
  this.data <- fread(this.file) %>% 
    as_tibble
  
  print(names(this.data))
  this.data.he <- this.data[1,] %>% 
    mutate(atlantis_prey_group = "HE", taxa_prey = "Herring",prey="Herring")
  
  this.data.rev <-this.data %>% 
    distinct(data,index,predator_stage,atlantis_pred_group,taxa_predator,predator,latitude,longitude,location,year,author,publication_yr, variable, predator_size, size_units, sample_source) %>% 
    mutate(predator_size = as.numeric(predator_size)) %>% 
    mutate(atlantis_prey_group = "HE", taxa_prey = "Herring",prey="Herring", value=0) %>% 
    dplyr::rename(predator_taxa = taxa_predator, proportion=value) %>% 
    dplyr::select(-predator,-index)
  
   return(this.data.rev)
  
  }
