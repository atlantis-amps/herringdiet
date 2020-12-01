#' @title Generate area map
#' @description  Functions to plot shapefile
#' @details INPUT: 1) coordinates for desired area
#' @details OUTPUT: 1) Map of area
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @details Can get coordinates for desired map from https://boundingbox.klokantech.com/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



make_map <- function(shape.file, boundary.file, file.name,scale.factor, bar.position, min.long, max.long, min.lat, max.lat, herring.locations) {

  herring.data <- read_csv(herring.locations) %>% 
    distinct(latitude,longitude)
  
  
  my_box <- rgeos::bbox2SP(n = 50, s = 46.75, w = -125, e = -121.75,
                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  target.crs <- projection(my_box)
  
  boundary.shape <- readOGR(paste0("~/herringdiet/shapefiles/",boundary.file))
  boundary.shape.crs <- spTransform(boundary.shape, target.crs)
  
  
 # model.shape <- readOGR(paste0("~/herringdiet/shapefiles/",shape.file))
 # model.shape.crs <- spTransform(model.shape, target.crs)
 # ps.shape <- intersect(model.shape.crs, my_box)
  
  # Reformat shape for mapping purposes
  
  #model.shape.df <- broom::tidy(ps.shape)
 
  boundary.basins <- boundary.shape@data %>% 
    dplyr::select(-WEB_GEOMET) %>% 
    mutate(id = as.character(0:(nrow(.)-1)))
  
  # Reformat shape for mapping purposes
  boundary.shape.df <- broom::tidy(boundary.shape.crs) %>% 
    left_join(boundary.basins, by="id")
  
   box.df <- broom::tidy(my_box) 
  
  coordinates(herring.data) <- c("longitude","latitude")
  
  target.crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  proj4string(herring.data)  <- target.crs
  
  
  coord.intersect <- raster::intersect(herring.data, boundary.shape.crs)
  
  boundary.coords <- coord.intersect@coords %>%
    as_tibble()
  
  col.pal <- c(redmonder.pal(8,"qMSOMed"),redmonder.pal(8,"qMSOPap")[8])
  
  model.map <- ggplot() +
    geom_polygon(data = box.df, aes(x = long, y = lat, group=group), fill="gray86") +
    #geom_path(data = model.shape.df, aes(x = long, y = lat, group=group)) +
    geom_polygon(data = boundary.shape.df, aes(x = long, y = lat, group=group, fill = NAME)) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    scale_fill_manual(values = col.pal, name="Basin") + 
    geom_point(aes(x = longitude, y = latitude), data = boundary.coords, alpha = .75, colour = "black", size = 0.5) +
    annotation_scale(location = "bl", width_hint = 0.2,
                     pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in")) +
    xlab("Lon")+
    ylab("Lat")+
    theme_minimal()
    #annotate(geom="text", x=-122.75, y=49.75, label="Puget Sound",
    #         color="black", size = 9)
  
  # world <- ne_countries(scale = "large", returnclass = "sf")
  # class(world)
  # 
  # world_points<- st_centroid(world)
  # world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  # 
  # 
  # model.map <- ggplot(data = world) +
  #   geom_sf() +
  #   geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
  #             colour = "gray33") +
  #   annotation_north_arrow(location = "br", which_north = "true", 
  #                          pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
  #                          style = north_arrow_fancy_orienteering) +
  #   #geom_text(data= world_points,aes(x=X, y=Y, label=name),
  #   #          color = "darkgrey", check_overlap = FALSE) +
  #   geom_point(aes(x = longitude, y = latitude), data = herring.data, alpha = .75, colour = "black") +
  #   coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
  #   annotation_scale(location = "bl", width_hint = 0.2) +
  #   xlab("Lon")+
  #   ylab("Lat")+
  #   theme_bw()+
  #   annotate(geom="text", x=-102, y=24, label="Puget Sound",
  #            color="black")
  
  ggsave(file.name, model.map, dpi = 400)
  
  return(model.map)
  
}
