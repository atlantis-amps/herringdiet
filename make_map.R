#' @title Generate area map
#' @description  Functions to plot shapefile
#' @details INPUT: 1) coordinates for desired area
#' @details OUTPUT: 1) Map of area
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @details Can get coordinates for desired map from https://boundingbox.klokantech.com/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



make_map <- function(boundary.file, file.name,scale.factor, bar.position, min.long, max.long, min.lat, max.lat, herring.locations) {

  # my.box <- rgeos::bbox2SP(n = 50, s = 46.75, w = -125, e = -121.75,
  #                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  # 
  # my.box.sf <- st_as_sf(my.box)
  
  target.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
 #read Puget Sound boundary polygons
  
  print("read Puget Sound boundary polygons")
  boundary.shape <- st_read(paste0("~/herringdiet/shapefiles/",boundary.file))
  boundary.shape.crs <- st_transform(boundary.shape, target.crs)
  
  #read world map
  print("read world map")
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
 
  #read point data
  print("read point data")
  
  herring.data <- read_csv(herring.locations) %>% 
    distinct(latitude,longitude)
  
  coordinates(herring.data) <- c("longitude","latitude")
  
  proj4string(herring.data)  <- target.crs

  herring.data.sf <- st_as_sf(herring.data)
  
  #eliminate data points outside Puget Sound basins
  print("eliminate data points outside Puget Sound basins")
  coord.intersect <- raster::intersect(herring.data, boundary.shape.crs)
  
  coord.intersect.sf <- st_as_sf(coord.intersect)
  
  print("set colors")
  col.pal <- c(redmonder.pal(8,"qMSOMed"),redmonder.pal(8,"qMSOPap")[8])
  
  print("generate map")
  model.map <- ggplot(data=world) +
    geom_sf()+
    geom_sf(data = boundary.shape.crs, aes(fill=NAME)) +
    geom_sf(data = coord.intersect.sf, alpha = .75, colour = "black", size = 0.5) +
    coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = TRUE)+
    scale_fill_manual(values = col.pal, name="Sub-basin") + 
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "br", width_hint = 0.2,
                     pad_x = unit(0.1, "cm"), pad_y = unit(0.25, "cm")) +
    xlab("Lon")+
    ylab("Lat")+
    theme_minimal()
  
  
  save_plot(file.name, model.map, ncol=1, base_width = 7, base_height = 4)
  
  return("done")
  
}
