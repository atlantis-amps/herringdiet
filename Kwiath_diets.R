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
atlantis.kw.loc <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=2)


salmon.categories <- c("Chum?", "Sock?", "Coho" , "Coho?" ,"Jack or Steel", "Cutthroat")

#Data for 2017
atlantis.kw.data.ch1 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=3) %>% 
  dplyr::select(-Vial, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  dplyr::select(-contains("totL"), -contains("totl"),-contains("Totl")) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Marked, -Other, -index, -Location, -FL, -Date) %>% 
  arrange(index) %>% 
  mutate(variable=gsub(" min#","",variable)) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable = if_else(Other %in% salmon.categories, Other, variable)) %>% 
  mutate(variable =gsub("\\?","",variable), prop = if_else(value>0,1,0)) %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  group_by(Location, Date) %>% 
  mutate(loc_date_index = group_indices()) %>% 
  ungroup %>% 
  group_by(Location, Date, Species, atlantis_name, atlantis_group) %>% 
  summarise(sum_prop = sum(prop),mean_FL= mean(FL), index=max(loc_date_index), count_samples = n()) %>% 
  ungroup() %>% 
  View()
  
  mutate(predator="Chinook",year=2017, size_units="mm", proportion=value, author = "Kwiaht","publication_yr" = 2018,data = index,
         variable="volume", sample_source="stomachs",taxa_predator="Oncorhynchus tshawytscha", atlantis_pred_group = "CI",predator_stage="juvenile") %>% 
  dplyr::select(-Other) %>% 
  left_join(atlantis.kw.loc, by="Location") %>% 
  rename(predator_size=FL, location = Location, atlantis_prey_group = atlantis_group, taxa_prey=Species,prey=atlantis_name) %>% 
  dplyr::select("atlantis_pred_group","predator_stage","taxa_predator","predator","atlantis_prey_group","taxa_prey","prey",
                "data","value",	"proportion","index","lat_d","lat_m","lat_s","lon_d","lon_m","lon_s",
                "location","year","author","publication_yr","variable","predator_size","size_units","sample_source")

#filter(is.na(atlantis_name)) #use this to see all categories are accounted for 

atlantis.kw.data.oth1 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=2) %>% 
  dplyr::select(-Date, -Vial, -Marked, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2017") # %>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 

atlantis.kw.2017 <- atlantis.kw.data.ch1 %>% bind_rows(atlantis.kw.data.oth1)

#Data for 2016
atlantis.kw.data.ch2 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=3) %>% 
  dplyr::select(-Date, -Vial, -Marked, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2016") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 

atlantis.kw.data.oth2 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=3) %>% 
  dplyr::select(-Date, -Vial, -Marked, -`#Lice`,FL, Other, -Volume, -Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2016")

atlantis.kw.2016 <- atlantis.kw.data.ch2 %>% bind_rows(atlantis.kw.data.oth2)


#Data for 2015
atlantis.kw.data.ch3 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=4) %>% 
  dplyr::select(-Date, -Vial, -Marked, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2015") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 



atlantis.kw.data.oth3 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=4) %>% 
  dplyr::select(-Date, -Vial, -Marked, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2015") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 

atlantis.kw.2015 <- atlantis.kw.data.ch3 %>% bind_rows(atlantis.kw.data.oth3)



#Data for 2014

atlantis.kw.data.ch4 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=5) %>% 
  dplyr::select(-Date, -Vial, -`#Lice`,FL, Other, -Volume,-Other2__1, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(FL=as.numeric(FL)) %>% 
  mutate(year="2014") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for


atlantis.kw.data.oth4 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=5) %>% 
  dplyr::select(-Date, -Vial, -`#Lice`,FL, Other, -Volume,-Other2__1, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2014") %>% 
  mutate(FL=as.numeric(FL))
  #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 

atlantis.kw.2014 <- atlantis.kw.data.ch4 %>% bind_rows(atlantis.kw.data.oth4)

#Data for 2013

atlantis.kw.data.ch5 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=6) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2013")  %>% 
  mutate(FL=gsub("x","NA",FL), FL=as.numeric(FL)) %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for


atlantis.kw.data.oth5 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=6) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2013")  %>% 
  mutate(FL=gsub("x","NA",FL), FL=as.numeric(FL)) %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.2013 <- atlantis.kw.data.ch5 %>% bind_rows(atlantis.kw.data.oth5)

#Data for 2012
atlantis.kw.data.ch6 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=7) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2012") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for


atlantis.kw.data.oth6 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=7) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -Volume,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Location, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2012") #%>% filter(is.na(atlantis_name)) #use this to see all categories are accounted for 


atlantis.kw.2012 <- atlantis.kw.data.ch6 %>% bind_rows(atlantis.kw.data.oth6)

#Data for 2011

atlantis.kw.data.ch7 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=8) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2011")  %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.data.oth7 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=8) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2011")  %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.2011 <- atlantis.kw.data.ch7 %>% bind_rows(atlantis.kw.data.oth7)


#Data for 2010

atlantis.kw.data.ch8 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=9) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl("FLL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(!grepl("MidgeL", variable)) %>%
  filter(!grepl("mm", variable)) %>%
  filter(!grepl("Fin", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2010")  %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.data.oth8 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=9) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl("FLL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(!grepl("MidgeL", variable)) %>%
  filter(!grepl("mm", variable)) %>%
  filter(!grepl("Fin", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2010")  %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.2010 <- atlantis.kw.data.ch8 %>% bind_rows(atlantis.kw.data.oth8)

#Data for 2009

atlantis.kw.data.ch9 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=10) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl("FLL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(!grepl("MidgeL", variable)) %>%
  filter(!grepl("mm", variable)) %>%
  filter(!grepl("Fin", variable)) %>% 
  filter(!Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator="Chinook") %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2009")  %>%
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for

atlantis.kw.data.oth9 <- read_xlsx("Kwiaht gut contents data_reviewed.xlsx", sheet=10) %>% 
  dplyr::select(-Date, -Vial, -Mark, -`#Lice`,FL, Other, -`Gut volume`,-Other2, -Other3) %>% 
  mutate(index=1:nrow(.)) %>% 
  gather(key=variable,value=value, -Site, -Other, -index, -FL) %>% 
  filter(!grepl("totL", variable)) %>% 
  filter(!grepl("totl", variable)) %>% 
  filter(!grepl("Totl", variable)) %>%
  filter(!grepl("Total", variable)) %>%
  filter(!grepl("total", variable)) %>%
  filter(!grepl("avL", variable)) %>%
  filter(!grepl("FL", variable)) %>%
  filter(!grepl("FLL", variable)) %>%
  filter(!grepl(" L", variable)) %>%
  filter(!grepl("MidgeL", variable)) %>%
  filter(!grepl("mm", variable)) %>%
  filter(!grepl("Fin", variable)) %>% 
  filter(Other %in% salmon.categories) %>% 
  mutate(variable=gsub("#","",variable)) %>% 
  mutate(variable=gsub(" min","",variable))  %>% 
  left_join(atlantis.kw.sp, by="variable") %>% 
  mutate(predator=Other, predator=gsub("\\?","",predator)) %>% 
  dplyr::select(-Other) %>% 
  mutate(year="2009") %>% 
  mutate(value=as.numeric(value))# %>%  filter(is.na(atlantis_name)) #use this to see all categories are accounted for


atlantis.kw.2009 <- atlantis.kw.data.ch9 %>% bind_rows(atlantis.kw.data.oth9)

#combine all years
#eliminates other categories, things like leaf and rock

atlantis.kw.index <-bind_rows(atlantis.kw.2009,atlantis.kw.2010,atlantis.kw.2011,atlantis.kw.2012,atlantis.kw.2013,
                                                  atlantis.kw.2014,atlantis.kw.2015,atlantis.kw.2016,atlantis.kw.2017) %>% 
  arrange(year,index) %>% 
  mutate(year=as.numeric(year),index=as.numeric(index)) %>% 
  rename(atlantis_prey_group=atlantis_group, prey_taxa=Species) %>% 
  mutate(index_2 = group_indices_(., .dots = c('year','index'))) %>% 
  mutate(atlantis_predator_group=if_else(predator=="Chinook","CI",
                                         if_else(predator=="Coho","CO",
                                                 if_else(predator=="Chum","CU",
                                                         if_else(predator=="Sock"|predator=="Cutthroat","SAL","NA")))),
         source="Kwiath_San_Juan",
         predator_stage = if_else(FL>300, "adult","juvenile"), 
         predator_stage = if_else(is.na(FL), "adult",predator_stage),
         Marked = if_else(is.na(Marked),"NO",Marked)) %>% 
  mutate(atlantis_predator_group=if_else(Marked=="Y" & predator=="Chinook", "CI1_CI2", atlantis_predator_group)) %>% 
  dplyr::select(source,atlantis_predator_group,predator_stage,predator,atlantis_prey_group,prey_taxa,value,index_2) %>% 
  rename(index=index_2) %>% 
  mutate(value = if_else(is.na(value),0,value)) %>% 
  filter(atlantis_prey_group!="Other") 

atlantis.kw.prop <- atlantis.kw.index  %>% 
  group_by(source,atlantis_predator_group,predator_stage,predator,index) %>% 
  summarise(sum_tot=sum(value)) %>% 
  left_join(atlantis.kw.index, by=c("source","atlantis_predator_group","predator_stage","predator","index")) %>% 
  mutate(proportion = value/sum_tot) %>% 
  mutate(proportion = if_else(is.nan(proportion),0,proportion)) %>% 
  dplyr::select(-value)

#fish is allocated to identified fish groups
#FPS (Sand Lance),HE (Herring),SMD (Small demersal),FVS (Lingcod),POP (Perch), FDF (Flatfish)


atlantis.kw.fish <- atlantis.kw.prop %>% 
  filter(atlantis_prey_group == "FISH") %>% 
  group_by(index) %>% 
  summarise(sum_tot_fish = sum(proportion)) 


atlantis.kw.fish.prop <- atlantis.kw.prop %>% 
  filter(atlantis_prey_group!="FISH") %>% #eliminate fish
  #select only fish groups
  filter(atlantis_prey_group=="FDF"|atlantis_prey_group=="FPS"|atlantis_prey_group=="POP"|atlantis_prey_group=="FVS"|atlantis_prey_group=="SMD"|atlantis_prey_group=="HE") %>% 
  filter(proportion!=0) %>% 
  mutate(index= as.character(index)) %>% 
  group_by(index) %>% 
  summarise(fish_groups=n()) %>% 
  ungroup %>% 
  mutate(index= as.numeric(index)) %>% 
  left_join(atlantis.kw.fish, by="index") %>% 
  mutate(fish_prop = sum_tot_fish / fish_groups)


fish.groups <- c("FDF", "FPS", "POP", "FVS","SMD","HE")

atlantis.kw.fish.final <- atlantis.kw.prop %>% 
  filter(atlantis_prey_group!="FISH") %>% #eliminate fish
  #select only fish groups
  filter(atlantis_prey_group %in% fish.groups) %>% 
  filter(proportion!=0) %>% 
  left_join(atlantis.kw.fish.prop, by="index") %>% 
  mutate(proportion = proportion + fish_prop) %>% 
  dplyr::select(-fish_prop,-sum_tot_fish, -fish_groups)

atlantis.kw.final <- atlantis.kw.prop %>% 
  filter(atlantis_prey_group!="FISH") %>% #eliminate fish
  #select only fish groups
  filter(!atlantis_prey_group %in% fish.groups) %>% 
  bind_rows(atlantis.kw.fish.final) %>% 
  mutate(value=proportion, data= index) %>% 
  select(source,atlantis_predator_group,predator_stage,predator,atlantis_prey_group,prey_taxa,data,value,proportion,index) %>% 
  rename(atlantis_pred_group = atlantis_predator_group, prey = prey_taxa)

setwd(save_path)
write_csv(atlantis.kw.final,"diet_matrix_kwiaht.csv")
setwd(work_path)


  
  