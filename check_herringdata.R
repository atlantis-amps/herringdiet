check_herringdata <- function(this.file, data.path){
  
  print(this.file)
  
  this.file.name <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[6]
  
  print(this.file.name)
  
  this.data <- read_xlsx(path=paste0(data.path,this.file)) 
  
  data.names <- names(this.data)
  
  test.names <- grepl("data",data.names)
  
  if(any(test.names)==TRUE) {
    
    data.set <- this.data %>% 
      mutate(file_name  = this.file) %>% 
      mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
             lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
      mutate(year = as.numeric(year), publication_yr = as.numeric(publication_yr)) %>% 
      group_by(data,location,year,author,publication_yr,file_name) %>% 
      summarise(prop_sum = sum(value))
    
    
    
  } else {
    
    print(paste(this.file," has no data column"))
    
    data.set <- this.data %>% 
      mutate(file_name  = this.file, data = "all_data") %>% 
      mutate(lat_d= as.numeric(lat_d),lat_m= as.numeric(lat_m),lat_s= as.numeric(lat_s),
             lon_d= as.numeric(lon_d),lon_m= as.numeric(lon_m),lon_s= as.numeric(lon_s)) %>% 
      mutate(year = as.numeric(year), publication_yr = as.numeric(publication_yr)) %>% 
      group_by(data,location,year,author,publication_yr,file_name) %>% 
      summarise(prop_sum = sum(value))
    
      }
   
  return(data.set)
  
}