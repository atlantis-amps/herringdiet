#' @title Code to clean and organize Reum data
#' @details Data of fish gut contents
#' @details Sent by John Reum
#' @details Reads in values, summarizes and assigns atlantis groups
#' @author Hem Nalini Morzaria Luna, email\{hmorzarialuna@gmail.com} Nov 2020
#' @keywords Puget Sound, Atlantis


atlantis_fg <- read_csv("atlantis_fg.csv") %>% 
  mutate(atlantis_name=tolower(atlantis_name), atlantis_name=gsub(" ","_",atlantis_name))


atlantis.diet.groups <- read_csv("atlantis_diets_prey_pred_groups.csv") %>% 
  dplyr::rename(prey=common_name, taxa_prey = species)

atlantis.groups <- atlantis.diet.groups %>% 
  dplyr::select(taxa_prey, atlantis_group)

#first sheet is prey groups
atlantis.sp <- read_xlsx(paste0("~/herringdiet/raw_diet_data/", "Reum_diet_summary.xlsx"), sheet=1) %>% 
  pivot_longer(cols=Amphipoda:Zoea,names_to = "prey", values_to = "value") %>% 
  mutate(taxa_prey = tolower(prey), year=year(date),taxa_prey=trimws(taxa_prey)) %>% 
  mutate(taxa_prey=tolower(prey),taxa_prey=trimws(taxa_prey), predator_stage = tolower(predator_stage)) %>% 
  dplyr::select(-date, -StomachID)
  

atlantis.sp.prey <- atlantis.diet.groups %>% 
  dplyr::select(prey, atlantis_group) %>%
  dplyr::rename(taxa_prey = prey) %>% 
  bind_rows(atlantis.groups) %>% 
  mutate(taxa_prey = tolower(taxa_prey), taxa_prey=trimws(taxa_prey)) %>% 
  distinct(taxa_prey, atlantis_group) %>% 
   right_join(atlantis.sp, by="taxa_prey") %>% 
  dplyr::rename(atlantis_prey_group=atlantis_group)

atlantis.data.clean <- atlantis.sp.prey %>% 
  group_by(`Trawl ID`,predator,predator_stage) %>% 
  mutate(loc_date_index = group_indices()) %>% 
  ungroup %>% 
  group_by(loc_date_index, index) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  group_by(latitude,longitude) %>% 
  mutate(loc_index = group_indices()) %>% 
  ungroup %>% 
  mutate(prop = value / 100)


calc_freq <- function(thisindex, this.data, locations.table){
  
  data.freq <- this.data %>%  
    filter(loc_date_index == thisindex) %>% 
    arrange(index,prey) %>% 
    group_by(index) %>% 
    mutate(fish_index = group_indices()) %>% 
    ungroup %>% 
    mutate(num_fish = max(fish_index)) %>% 
    group_by(atlantis_pred_group,predator_stage,taxa_predator, predator, loc_index, year, loc_date_index, atlantis_prey_group,taxa_prey,prey) %>% 
    summarise(sum_prop = sum(prop), predator_size= mean(predator_size), num_fish=max(num_fish)) %>% 
    ungroup %>% 
    mutate(proportion = sum_prop / num_fish) %>% 
    left_join(locations.table, by=c("loc_index")) %>% 
    dplyr::select(-sum_prop) 
  
  
  return(data.freq)
}

index.id <- atlantis.data.clean %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

locations.data <- atlantis.data.clean %>% 
  distinct(loc_index, latitude, longitude,author,publication_yr,size_units,sample_source, variable)


freq.data <- lapply(index.id, calc_freq, this.data=atlantis.data.clean, locations.table=locations.data) %>% 
  bind_rows()


save.path <- "~/herringdiet/raw_diet_data_rev"
write_csv(freq.data,paste0(save.path,"/","diet_matrix_reum_rev.csv"))


