#' @title Select diet files with Herring as prey
#' @description  Read file and test if herring are prey
#' @details INPUT: 1) diet data for Puget Sound, 2) predator names, 3) list of files and references
#' @details OUTPUT: 1) Clean table 2) File with references, 3) Directory with subset of data files
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



clean_herringdata <- function(this.file, diet.names, predator.list){
  
  print(this.file)
  
  print("Read list of references")  
  
  diet.file.names <- read_csv(diet.names) %>% 
    dplyr::rename(file_name = FILE, reference = CITA)
  
  print(head(diet.file.names))
  
  print("read species list")
  
  predator.names <- read_csv(predator.list)
  
  print(head(predator.names))
  
  this.file.name <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[6]
  
  this.data <- read_xlsx(this.file) %>% 
    setNames(c("atlantis_pred_group","predator_stage","taxa_predator","predator","atlantis_prey_group",
               "taxa_prey","prey","data","value","proportion","index","lat_d","lat_m","lat_s","lon_d",
               "lon_m","lon_s","location","year","author","publication_yr","variable","predator_size",
               "size_units","sample_source")) %>% 
    filter(atlantis_prey_group =="HE") %>% 
    mutate(file_name  = this.file.name, full_file_name = this.file) %>% 
    mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
           lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
    mutate(year = as.numeric(year), publication_yr = as.numeric(publication_yr)) %>% 
    mutate(predator_size = as.numeric(predator_size))
  
  
  subset.names <- c("Chondrichthyes","Chordata","Cetacea","Mammalia")
  
  print("Clean data table")
  
  herring.data <- this.data %>% 
    left_join(predator.names, by="taxa_predator") %>% 
    dplyr::select(-taxa_predator,-predator) %>% 
    dplyr::rename(predator_name = new_name, predator_taxa = scientific_name) %>% 
    filter(!predator_name %in% subset.names) %>% 
    mutate(location=if_else(location=="southern British Columbia","Southern British Columbia",
                            if_else(location=="South San Juan Channel","Southern San Juan Channel",
                                    if_else(location=="inside Active Pass","Active Pass",
                                            if_else(location=="Padilla Bay, Washington","Padilla Bay",
                                                    if_else(location=="Protection island","Protection Island",location))))))
  
    
    #calculate decimal degrees and fix incorrect coordinates
  herring.coord.data <- herring.data %>% 
    mutate(lat_d = if_else(is.na(lat_d),0,lat_d)) %>%
    mutate(lat_m = if_else(is.na(lat_m),0,lat_m)) %>%
    mutate(lat_s = if_else(is.na(lat_s),0,lat_s)) %>%
    mutate(latitude = if_else(lat_d>0,(lat_d + lat_m/60 + lat_s/3600),lat_d)) %>% 
    mutate(lon_d = if_else(is.na(lon_d),0,lon_d)) %>%
    mutate(lon_m = if_else(is.na(lon_m),0,lon_m)) %>%
    mutate(lon_s = if_else(is.na(lon_s),0,lon_s)) %>%
    mutate(longitude = if_else(lon_d>0,(lon_d + lon_m/60 + lon_s/3600),lon_d)) %>% 
    dplyr::select(-starts_with("lat_"),-starts_with("lon_")) %>% 
    mutate(longitude = if_else(longitude>1,longitude*-1,longitude)) %>% 
    mutate(latitude = if_else(latitude<1,latitude*-1,latitude))
  
  return(herring.coord.data)
  
  }
