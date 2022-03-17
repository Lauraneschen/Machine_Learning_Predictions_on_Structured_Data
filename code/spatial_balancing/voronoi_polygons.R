# load required packages
library(tidyverse)
library(sf)
library(vroom)
library(dismo)
library(rworldmap)
library(rgeos)
library(lwgeom)
library(comprehenr)
library(viridis)
library(gridExtra)


### goal:
# create a spatially balanced data set by sampling from spatially unbalanced data
# in this script, the locations are assigned sampling probabilities
# sampling probability for specific location is inversely related to data density

### steps:
# 1) transform location data into equal-area projection
# 2) create voronoi polygons
# 3) cut voronoi polygons by measurement area
# 4) calculate area of voronoi polygons and assign sampling probabilities


# load location data
full_routes <- vroom("data/Full_routes.csv")
long_lat_routes <- unique(full_routes[c("Longitude", "Latitude", "route_id")])
long_lat <- long_lat_routes[,1:2]
df_areas <- long_lat_routes


### functions
# transform points from WGS84 to Albers Equal Area Conic projection
eq_area_proj <- function(long_lat){
  long_lat_df <- as.data.frame(long_lat)
  points <- st_multipoint(matrix(c(long_lat_df[,1], long_lat_df[,2]), ncol = 2))
  sf_points <- st_sfc(points)
  st_crs(sf_points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  points_Albers <- st_transform(sf_points, "EPSG:5070")
  return(points_Albers)
}

# create voronoi polygons
voronoi_polygons <- function(points){
  # convert to multipoint and then combine
  combined <- st_combine(st_geometry(points))
  voronoi <- st_voronoi(combined)
  voronoi <- st_collection_extract(voronoi)
  return(voronoi[unlist(st_intersects(points, voronoi))])
}


### main script
# transform location data into equal area projection, then create voronoi polygons
point_Albers <- eq_area_proj(long_lat)
coords <- st_coordinates(point_Albers)[,1:2]
coords_sf <- st_as_sf(data.frame(x = coords[,1], y = coords[,2]), coords = 1:2)
vor_polygons <- voronoi_polygons(coords_sf)


# outter boundary: USA and Canada (select from worldmap)
# used to cut the voronoi polygons by that boundary
worldmap <- rnaturalearth::ne_download(scale = 110,
                                       type = "countries",
                                       destdir = tempdir(),
                                       load = TRUE,
                                       returnclass = "sf")

ind <- which(worldmap$NAME == "United States of America" | worldmap$NAME == "Canada")
wordmap_trans <- st_transform(worldmap, st_crs("EPSG:5070"))

# CRS: Conus Albers
North_America <- st_union(wordmap_trans[ind,])

# CRS: WGS84
North_America_WGS84 <- st_transform(North_America, crs = 4326)


# intersect voronoi polygons with the measurement area (cut at outer boundary)
intersections <- list()
df_areas$area <- NA
st_crs(vor_polygons) <- st_crs("EPSG:5070")

# assign areas (takes a while)
for(i in 1:length(vor_polygons)){
  
  if(as.numeric(st_area(vor_polygons[i])) > 0){
    # intersect the "valid" area (North America) with voronoi polygon
    area_cut <- st_intersection(North_America, st_make_valid(vor_polygons[i]))
    intersections[[i]] <- area_cut
    
    # then calculate area of voronoi polygon
    if(length(area_cut) > 0){
      df_areas$area[i] <- st_area(area_cut)
    }
    
    else{
      df_areas$area[i] <- 0
    }
  }
  
  else{
    area_cut <- NULL
    intersections[[i]] <- area_cut
    df_areas$area[i] <- 0
  }
  
}


# combine all corrected voronoi polygons in one sf object (I didn't find a more efficient way)
all <- intersections[[1]]

for(i in 2:length(intersections)){
  all <- st_combine(c(all, intersections[[i]]))
}


# backtransform to WGS84
all_WGS84 <- st_transform(all, crs = 4326)

df_areas_sf <- df_areas %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# plot of locations and their belonging polygons
plot_voronoi <- ggplot() +
  geom_sf(data = st_cast(all_WGS84, to = "POLYGON"), size = 0.01) +
  geom_sf(data = df_areas_sf, size = 0.001, col = "red3") +
  theme(legend.position = "none") +
  coord_sf(expand = F)


# plot of sampling probabilities (represented by point size)
plot_sampl_prob <- ggplot() +
  geom_sf(data = North_America_WGS84) +
  geom_sf(data = df_areas_sf, aes(size = area), col = "red3") +
  scale_size_continuous(range = c(0.1, 3)) +
  theme(legend.position = "none") +
  coord_sf(expand = F)


# plot of isolines of data density (smoothed)
plot_density <- ggplot() + 
  geom_sf(data = North_America_WGS84) +
  coord_sf(expand = F) + 
  stat_density_2d_filled(data = long_lat, aes(x = Longitude, y = Latitude), alpha = 0.6, bins = 20) +
  theme(legend.position = 'none') +
  xlim(st_bbox(North_America_WGS84)[1], st_bbox(North_America_WGS84)[3]) +
  ylim(st_bbox(North_America_WGS84)[2], st_bbox(North_America_WGS84)[4]) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


df_pol <- to_df(for(i in 1:length(intersections)) intersections[i])
del <- c()

for(i in 1:length(intersections)){
  if(length(intersections[[i]]) == 0)
    del <- append(del, i)
}


# plots of polygons, colored by their size
plot_voronoi_2 <- ggplot() +
  geom_sf(data = st_transform(df_pol$geometry, crs = 4326), aes(fill = log(df_areas$area[-del])), col = NA) +
  scale_fill_viridis(begin = 1, end = 0, option = "magma", guide = guide_colourbar(reverse = TRUE), "log(area)") +
  theme(legend.position = 'none') +
  scale_size_identity() +
  coord_sf(expand = F)


# figure of all plots
grid.arrange(plot_density, plot_voronoi, plot_voronoi_2, plot_sampl_prob,
             nrow = 2)


# save table
write.csv(df_areas, "data/areas.csv")


