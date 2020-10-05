#' @title Select diet files with Herring as prey
#' @description  Read file and test if herring are prey
#' @details INPUT: 1) diet data for Puget Sound, 2) predator names, 3) list of files and references
#' @details OUTPUT: 1) Clean table 2) File with references, 3) Directory with subset of data files
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



clean_herringdata <- function(herring.data, diet.names, predator.list){
  
  print(head(herring.data))
  
  print("Read list of references")  
  
  diet.file.names <- read_csv(diet.names) %>% 
    dplyr::rename(file_name = FILE, reference = CITA)
  
  print(head(diet.file.names))
  
  print("read species list")
  
  predator.names <- read_csv(predator.list)
  
  print(head(predator.names))
  
  subset.names <- c("Chondrichthyes","Chordata","Cetacea","Mammalia")
  subset.location <- c("Tomales Bay","Washington Coast","Willapa Bay","Moon Island","Grays Harbor")
  
  print("Clean data table")
  
  herring.name.data <- herring.data %>% 
    left_join(predator.names, by="taxa_predator") %>% 
    dplyr::select(-taxa_predator,-predator) %>% 
    dplyr::rename(predator_name = new_name, predator_taxa = scientific_name) %>% 
    filter(!predator_name %in% subset.names) %>%  # these appear to only be 14 data points  
    mutate(full_file_name = file_name) %>% 
    mutate(location=str_to_title(location)) %>% 
    filter(!location %in% subset.location) %>% 
    mutate(location=if_else(location=="Tatoosh Islan","Tatoosh Island",
                            if_else(location=="Tooten","Totten Inlet",
                                    if_else(location=="San Juan Island","San Juan Islands",
                                            if_else(location=="Strait Of Juan De Fuca",  "Strait Of San Juan De Fuca", 
                                                    if_else(location=="Hood Canal, South Puget Sound", "Hood Canal", 
                                                            if_else(location=="Ne Vancouver Island","NE Vancouver Island",
                                                                    if_else(location=="Padilla Bay, Washington","Padilla Bay", 
                                                                            if_else(location=="Qc Island","QC Island",
                                                                                    if_else(location=="Hammersley","Hammersley Inlet",
                                                                                            if_else(location=="Eld","Eld Inlet",
                                                                                            if_else(location=="Skookum","Skookum Inlet",location))))))))))))
  #calculate decimal degrees and fix incorrect coordinates
  herring.coord.data <- herring.name.data %>% 
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
  
  
  print("Save reference table")
  
  herring.coord.data %>% 
    mutate(file_name=gsub(".xlsx","",file_name)) %>% 
    mutate(file_name=gsub("/home/atlantis/herringdiet/all_diet_data/","",file_name)) %>% 
    left_join(diet.file.names, by="file_name") %>% 
    distinct(reference) %>% 
    filter(!is.na(reference)) %>% 
    write_csv("herring_references.csv")
  
  #using all predators
  #selected groups are 
  #pred.groups <- c("HSL","PIN","SAL","SB","FMM","CI","CO","CU","DOG","PIS","ROC","MRO","SMD","SP")
  
  herring.pubs <- herring.coord.data %>%
    # filter(atlantis_pred_group %in% pred.groups) %>% 
    distinct(full_file_name) %>% 
    pull(full_file_name)
  
  dir.create("herring_data")
  file.copy(from=herring.pubs,to="/home/atlantis/herringdiet/herring_data/",recursive = FALSE, 
            copy.mode = TRUE)
  
  return(herring.coord.data)
  
}
