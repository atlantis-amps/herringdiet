#functions for herring data analysis


get_herringdata <- function(this.file){
  
  this.file.name <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[6]
  
  print(this.file.name)
  
  this.data <- read_xlsx(this.file) %>% 
    filter(atlantis_prey_group =="HE") %>% 
    mutate(file_name  = this.file) %>% 
    mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
           lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
    mutate(year = as.numeric(year), publication_yr = as.numeric(publication_yr))
  
  print(names(this.data))
  
  test.name <- grep("Daily_Consumption",names(this.data),value=TRUE)
  
  if(rlang::is_empty(test.name)==FALSE) stop(print(paste(this.file.name,"wrong col names")))
  
  if(class(this.data$data)=="numeric") stop(print(paste("data is numeric in file",this.file.name))) 
  if(nrow(this.data)!=0) print("Herring prey present")
  
  return(this.data)
  
}