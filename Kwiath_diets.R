#' @title Code to clean and organize Kwiaht data
#' @details Data of salmon gut contents from San Juan Islands
#' @details Sent by Russel Barsh
#' @details Reads in values, summarizes and assigns atlantis groups
#' @author Hem Nalini Morzaria Luna, email\{hmorzarialuna@gmail.com} Nov 2020
#' @keywords Puget Sound, Atlantis


atlantis_fg <- read_csv("atlantis_fg.csv") %>% 
  mutate(atlantis_name=tolower(atlantis_name), atlantis_name=gsub(" ","_",atlantis_name))

#first sheet is prey groups
atlantis.kw.sp <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=1) %>% 
  rename(variable=acronym)

#second sheet is locations
atlantis.kw.loc <- read_csv("Kwiaht_locations.csv")


salmon.categories <- c("Chum?", "Sock?", "Coho" , "Coho?" ,"Jack or Steel", "Cutthroat")


clean_data <- function(kwiaht.data, atlantis.kw.loc){
  
  atlantis.kw.data.clean <- kwiaht.data %>% 
    gather(key=variable,value=value, -Other, -index, -Location, -FL, -Date, -atlantis_pred_group) %>% 
    mutate(variable=gsub(" min#","",variable)) %>% 
    mutate(variable=gsub("#","",variable)) %>% 
    mutate(variable = if_else(Other %in% salmon.categories, Other, variable)) %>% 
    mutate(variable =gsub("\\?","",variable), prop = if_else(value>0,1,0)) %>% 
    left_join(atlantis.kw.sp, by="variable") %>% 
    group_by(atlantis_pred_group,Location, Date) %>% 
    mutate(loc_date_index = group_indices()) %>% 
    ungroup %>% 
    group_by(loc_date_index, index) %>% 
    mutate(id = row_number()) %>% 
    ungroup
  
  return(atlantis.kw.data.clean)
  
}

calc_freq <- function(thisindex, atlantis.kw.data.clean){
  
  kw.data.freq <- atlantis.kw.data.clean %>%  
    filter(loc_date_index == thisindex) %>% 
    arrange(index,atlantis_name) %>% 
    group_by(index) %>% 
    mutate(fish_index = group_indices()) %>% 
    ungroup %>% 
    mutate(num_fish = max(fish_index)) %>% 
    group_by(atlantis_pred_group,Location, Date, loc_date_index, atlantis_name, atlantis_group, Species) %>% 
    summarise(sum_prop = sum(prop), mean_FL= mean(FL), num_fish=max(num_fish)) %>% 
    ungroup %>% 
    mutate(value = sum_prop / num_fish) 
  
  return(kw.data.freq)
}


#Data for 2017
atlantis.kw.data.ch1 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=3) %>% 
  mutate(atlantis_pred_group = if_else(Marked=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Marked, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl"))
   
atlantis.kw.data.clean <- clean_data(atlantis.kw.data.ch1, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat <- lapply(index.id, calc_freq, atlantis.kw.data.clean) %>% 
  bind_rows()

freq.kwhiat.2017 <- freq.kwhiat %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 

  
  

#Data for 2016
atlantis.kw.data.ch2 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=4) %>% 
  mutate(atlantis_pred_group = if_else(Marked=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Marked, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.))

atlantis.kw.data.clean.2 <- clean_data(atlantis.kw.data.ch2, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.2 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.2 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.2) %>% 
  bind_rows()

freq.kwhiat.2016 <- freq.kwhiat.2 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value,
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 



#Data for 2015

atlantis.kw.data.ch3 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=5) %>% 
  mutate(atlantis_pred_group = if_else(Marked=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Marked, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.))

atlantis.kw.data.clean.3 <- clean_data(atlantis.kw.data.ch3, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.3 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.3 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.3) %>% 
  bind_rows()

freq.kwhiat.2015 <- freq.kwhiat.3 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 


#Data for 2014

atlantis.kw.data.ch4 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=6) %>% 
  mutate(atlantis_pred_group = "CI") %>% 
  dplyr::select(-Vial, -`#Lice`, -Volume, -`Other...36`, -`Other2...37`,-`Other2...66`,-Other3) %>% 
dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.)) %>% 
  dplyr::rename(Other = `Other...6`)

atlantis.kw.data.clean.4 <- clean_data(atlantis.kw.data.ch4, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.4 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.4 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.4) %>% 
  bind_rows()

freq.kwhiat.2014 <- freq.kwhiat.4 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 


#Data for 2013

atlantis.kw.data.ch5 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=7) %>% 
mutate(atlantis_pred_group = if_else(Mark=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.))

atlantis.kw.data.clean.5 <- clean_data(atlantis.kw.data.ch5, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.5 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.5 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.5) %>% 
  bind_rows()

freq.kwhiat.2013 <- freq.kwhiat.5 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 

#Data for 2012
atlantis.kw.data.ch6 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=8) %>% 
mutate(atlantis_pred_group = if_else(Mark=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.))

atlantis.kw.data.clean.6 <- clean_data(atlantis.kw.data.ch6, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.6 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.6 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.6) %>% 
  bind_rows()

freq.kwhiat.2012 <- freq.kwhiat.6 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 


#Data for 2011

atlantis.kw.data.ch7 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=9) %>% 
 mutate(atlantis_pred_group = if_else(Mark=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
dplyr::select(-contains("avL"), -contains("avFL"),-contains("totFL"), -contains(" L")) %>% 
  mutate(index = 1:nrow(.)) %>% 
  dplyr::rename(Location=Site)

atlantis.kw.data.clean.7 <- clean_data(atlantis.kw.data.ch7, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.7 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.7 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.7) %>% 
  bind_rows()

freq.kwhiat.2011 <- freq.kwhiat.7 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 



#Data for 2010

atlantis.kw.data.ch8 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=10) %>% 
  mutate(atlantis_pred_group = if_else(Mark=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl"), -contains("avL"), -contains("avFL"),-contains("totFL"), -contains(" L")) %>% 
  mutate(index = 1:nrow(.)) %>% 
  dplyr::rename(Location=Site)

atlantis.kw.data.clean.8 <- clean_data(atlantis.kw.data.ch8, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.8 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.8 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.8) %>% 
  bind_rows()

freq.kwhiat.2010 <- freq.kwhiat.8 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value, 
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile") 


#Data for 2009

atlantis.kw.data.ch9 <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Kwiaht gut contents data_reviewed.xlsx"), sheet=11) %>% 
  mutate(atlantis_pred_group = if_else(Mark=="Y","CIH","CI")) %>% 
  dplyr::select(-Vial, -Mark, -`#Lice`,-`Gut volume`,-Other2, -Other3, -`Fin#`) %>% 
  dplyr::select(-contains("mm"),-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index = 1:nrow(.)) %>% 
  dplyr::rename(Location = Site)

atlantis.kw.data.clean.9 <- clean_data(atlantis.kw.data.ch9, atlantis.kw.loc)

index.id <- atlantis.kw.data.clean.9 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.kwhiat.9 <- lapply(index.id, calc_freq, atlantis.kw.data.clean.9) %>% 
  bind_rows()

freq.kwhiat.2009 <- freq.kwhiat.9 %>% 
  mutate(year=year(Date)) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  dplyr::rename(data = loc_date_index, predator_size=mean_FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species, prey=atlantis_name) %>% 
  mutate(predator_name="Chinook", size_units="mm", author = "Kwiaht","publication_yr" = 2018, proportion = value,
         variable="volume", sample_source="stomachs", predator_taxa="Oncorhynchus tshawytscha", predator_stage="juvenile")


#combine all years
#eliminates other categories, things like leaf and rock

atlantis.kw.freq <- bind_rows(freq.kwhiat.2009,freq.kwhiat.2010,freq.kwhiat.2011,freq.kwhiat.2012,freq.kwhiat.2013,
                              freq.kwhiat.2014,freq.kwhiat.2015,freq.kwhiat.2016,freq.kwhiat.2017) %>% 
    mutate(file_name = "diet_matrix_kwiaht.csv", full_file_name = "/home/atlantis/herringdiet/raw_diet_data_rev/diet_matrix_kwiaht.csv") %>% 
  rename(taxa_predator = predator_taxa, predator = predator_name) %>% 
  dplyr::select(-Date, -data, -sum_prop, -value, -file_name, -full_file_name) %>% 
  mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
         lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
  mutate(latitude = if_else(lat_d>0,(lat_d + lat_m/60 + lat_s/3600),lat_d)) %>% 
  mutate(longitude = if_else(lon_d>0,(lon_d + lon_m/60 + lon_s/3600),lon_d)) %>% 
  dplyr::select(-starts_with("lat_"),-starts_with("lon_")) %>% 
  mutate(longitude = if_else(longitude>1,longitude*-1,longitude)) %>% 
  mutate(latitude = if_else(latitude<1,latitude*-1,latitude))
  


save.path <- "~/herringdiet/raw_diet_data_rev"
write_csv(atlantis.kw.freq,paste0(save.path,"/","diet_matrix_kwiaht_rev.csv"))

  
  