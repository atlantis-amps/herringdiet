#' @title Code to clean and organize raw data
#' @details Data of fish diet 
#' @details Reads in values, summarizes and assigns atlantis groups
#' @author Hem Nalini Morzaria Luna, email\{hmorzarialuna@gmail.com} Nov 2020
#' @keywords herring, Atlantis

raw.data.dir <- "~/herringdiet/raw_diet_data"
raw.data.dir.rev <- "~/herringdiet/raw_diet_data_rev"

setwd(raw.data.dir)

clean_data <- function(this.data.file){
  
  clean.data <- read_csv(this.data.file) %>% 
    group_by(atlantis_pred_group,location, year) %>% 
    mutate(loc_date_index = group_indices()) %>% 
    arrange(loc_date_index) %>% 
    ungroup %>% 
    group_by(loc_date_index, index) %>% 
    mutate(id = row_number(), prop = if_else(value>0,1,0)) %>% 
    ungroup
  
  return(clean.data)
}

calc_freq <- function(thisindex, this.data, locations.table){
  
  data.freq <- this.data %>%  
    filter(loc_date_index == thisindex) %>% 
    arrange(index,prey) %>% 
    group_by(index) %>% 
    mutate(fish_index = group_indices()) %>% 
    ungroup %>% 
    mutate(num_fish = max(fish_index)) %>% 
    group_by(atlantis_pred_group,predator_stage,taxa_predator, predator, location, year, loc_date_index, atlantis_prey_group,taxa_prey,prey) %>% 
    summarise(sum_prop = sum(prop), predator_size= mean(predator_size), num_fish=max(num_fish)) %>% 
    ungroup %>% 
    mutate(value = sum_prop / num_fish) %>% 
    left_join(locations.table, by="location")
  
  return(data.freq)
}

beaudreau.data <- clean_data(this.data.file = "diet_matrix_beaudreau_2004.csv")

locations.beaudreau <- beaudreau.data %>% 
  distinct(location,lat_d,lat_m,lat_s,lon_d,lon_s,author,publication_yr,size_units,sample_source)

index.id <- beaudreau.data %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.beaudreau <- lapply(index.id, calc_freq, this.data=beaudreau.data, locations.table=locations.beaudreau) %>% 
  bind_rows()

write_csv(freq.beaudreau, paste0(raw.data.dir.rev,"/","diet_matrix_beaudreau_2004_rev.csv"))


##
metrock.data <- clean_data(this.data.file = "diet_matrix_metrock_2001.csv")

locations.metrock <- metrock.data %>% 
  distinct(location,lat_d,lat_m,lat_s,lon_d,lon_s,author,publication_yr,size_units,sample_source)

index.id <- metrock.data %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.metrock <- lapply(index.id, calc_freq, this.data=metrock.data, locations.table=locations.metrock) %>% 
  bind_rows()

write_csv(freq.metrock, paste0(raw.data.dir.rev,"/","diet_matrix_metrock_2001_rev.csv"))

##

nelson.data <- clean_data(this.data.file = "diet_matrix_nelson_2016.csv")

locations.nelson <- nelson.data %>% 
  distinct(location,lat_d,lat_m,lat_s,lon_d,lon_s,author,publication_yr,size_units,sample_source)

index.id <- nelson.data %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.nelson <- lapply(index.id, calc_freq, this.data=nelson.data, locations.table=locations.nelson) %>% 
  bind_rows()

write_csv(freq.nelson, paste0(raw.data.dir.rev,"/","diet_matrix_nelson_2016_rev.csv"))

#

nelson.data2 <- clean_data(this.data.file = "diet_matrix_nelson_2017_2018.csv")

locations.nelson2 <- nelson.data2 %>% 
  distinct(location,lat_d,lat_m,lat_s,lon_d,lon_s,author,publication_yr,size_units,sample_source)

index.nelson2 <- nelson.data2 %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.nelson2 <- lapply(index.nelson2, calc_freq, this.data=nelson.data2, locations.table=locations.nelson2) %>% 
  bind_rows()

write_csv(freq.nelson2, paste0(raw.data.dir.rev,"/","diet_matrix_nelson_2017_2018_rev.csv"))


#

duguid.data <- clean_data(this.data.file = "diet_matrix_duguid_2016.csv")

locations.duguid <- duguid.data %>% 
  distinct(location,lat_d,lat_m,lat_s,lon_d,lon_s,author,publication_yr,size_units,sample_source)

index.id <- duguid.data %>% 
  distinct(loc_date_index) %>% 
  pull(loc_date_index)

freq.duguid <- lapply(index.id, calc_freq, this.data=duguid.data, locations.table=locations.nelson) %>% 
  bind_rows()

write_csv(freq.duguid, paste0(raw.data.dir.rev,"/","diet_matrix_duguid_2016.csv"))