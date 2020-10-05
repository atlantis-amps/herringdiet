#' @title Generate area map
#' @description  Functions to plot shapefile
#' @details INPUT: 1) coordinates for desired area
#' @details OUTPUT: 1) Map of area
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @details Can get coordinates for desired map from https://boundingbox.klokantech.com/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


make_map <- function(shape.file, file.name,scale.factor, bar.position, min.long, max.long, min.lat, max.lat, location.table, herring.dietdata) {

  herring.diet.data <- read_csv(herring.dietdata)
  
  model.shape <- readOGR(shape.file)
  
  # Reformat shape for mapping purposes
  model.shape.df <- broom::tidy(model.shape)
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
  
  world_points<- st_centroid(world)
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  
  model.map <- ggplot(data = world) +
    geom_sf() +
    geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
              colour = "gray33") +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering) +
    #geom_text(data= world_points,aes(x=X, y=Y, label=name),
    #          color = "darkgrey", check_overlap = FALSE) +
    geom_point(aes(x = longitude, y = latitude, size = year), data = herring.diet.data, alpha = .5, colour = "red") +
    coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
    annotation_scale(location = "bl", width_hint = 0.2) +
    xlab("Lon")+
    ylab("Lat")+
    theme_bw()+
    annotate(geom="text", x=-102, y=24, label="Puget Sound",
             color="black")
  
  ggsave(file.name, model.map, width = 16,height = 12, units="cm", dpi = 400)
  
  return(model.map)
  
}
