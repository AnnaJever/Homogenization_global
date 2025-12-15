# Visualization - maps ####

# information for figures building: size (350 x 350)

# packages ----
library(ggplot2)
library(gridExtra)
library(rstatix)
library(sf)
library(rnaturalearth)
library(terra)
library(ggpubr)
library(ggrepel)
library(patchwork)
library(ggalt)





# load data ----
load("./Data/R_products/similarity_islands.RData")





# location of islands ----

# location of all study islands
islands_geo <- st_read("./Data/environmental_data/islands_pollen.kml")
str(islands_geo)

islands_geo_sv <- vect(islands_geo) # convert to spatial vector

# shape of the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin!= "Antarctica") %>%
  st_transform(crs = "+proj=moll")


coords_moll <- readme %>%
  st_as_sf(coords = c("Long_isl_center", "Lat_isl_center"), crs = "EPSG:4326") %>%
  st_transform(crs = "+proj=moll")


readme$Long_moll <- st_coordinates(coords_moll)[,1]
readme$Lat_moll <- st_coordinates(coords_moll)[,2]

# plot islands globally
island_location <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean)), size = 3, shape = 16)+
  theme_bw()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c(paste("Atlantic Ocean,", paste("n =", nrow(readme %>% filter(ocean == "atlantic")))),
                                paste("Indian Ocean,", paste("n =", nrow(readme %>% filter(ocean == "indian")))),
                                paste("Mediterranean Sea,", paste("n =", nrow(readme %>% filter(ocean == "mediterranean")))),
                                paste("Pacific Ocean,", paste("n =", nrow(readme %>% filter(ocean == "pacific"))))),
                     values = c("#FFC100", "black", "#CC998D", "#32746D"))+
  guides(color=guide_legend("Oceanic basin"))

island_location

ggsave(
  "island_location.jpg",
  plot = island_location,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 2000,
  height = 900,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)




#### Atlantic map
world_latlong <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin!= "Antarctica")

island_location_atl <- ggplot(data = world_latlong) +
  geom_sf(colour = "lightgrey", fill="lightgrey")+
  coord_sf(xlim = c(-90, 15), ylim = c(75, -60), expand = FALSE) +
  geom_encircle(data = readme[which(readme$ocean == "atlantic"),],
                aes(x = Long_isl_center, y = Lat_isl_center, fill = biogeo_unit),
                alpha = 0.5, colour = NA, spread = 0.01, s_shape=0.5, expand=0.02)+
  scale_color_manual(name = "Biogeographic unit",
                     values = c("#FFF9C4FF", "#FBC02DFF", "red", "darkred", "chocolate4"),
                     aesthetics = c("color", "fill"), labels = c("Caribbean Islands", "Gulf of Guinea Islands", "Macaronesia", "North Atlantic", "South Atlantic"))+
  geom_point(data = readme[which(readme$ocean == "atlantic"),],
             aes(x = Long_isl_center, y = Lat_isl_center),
             colour = "black", size = 1, shape = 16)+
  geom_text_repel(data = readme[which(readme$ocean == "atlantic"),],
                  aes(x = Long_isl_center, y = Lat_isl_center, label = island), 
                  fontface = "bold")+
  ylab("Longitude") +
  xlab("Latitude") +
  theme_pubr()

island_location_atl

ggsave(
  "islands_Atlantic.svg",
  plot = island_location_atl,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 2000,
  height = 3000,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)



#### Mediterranean map
island_location_med <- ggplot(data = world_latlong) +
  geom_sf(colour = "lightgrey", fill="lightgrey")+
  coord_sf(xlim = c(0, 15), ylim = c(45, 35), expand = FALSE) +
  geom_encircle(data = readme[which(readme$ocean == "mediterranean"),],
                aes(x = Long_isl_center, y = Lat_isl_center, fill = biogeo_unit),
                alpha = 0.5, colour = NA, spread = 0.01, s_shape=0.5, expand=0.02)+
  scale_color_manual(name = "Biogeographic unit",
                     values = c("#CC998D", "salmon", "#6D575C"),
                     aesthetics = c("color", "fill")) +
  geom_point(data = readme[which(readme$ocean == "mediterranean"),],
             aes(x = Long_isl_center, y = Lat_isl_center), colour = "black", size = 3, alpha = 2/3, shape = 16)+
  ylab("Longitude")+
  xlab("Latitude")+
  labs(fill = "Island group")+
  geom_text_repel(data = readme[which(readme$ocean == "mediterranean"),],
                  aes(x = Long_isl_center, y = Lat_isl_center, label = island), 
                  fontface = "bold")+
  theme_pubr()


island_location_med

ggsave(
  "islands_Mediterranean.svg",
  plot = island_location_med,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)




#### Indian Ocean 
island_location_ind <- ggplot(data = world_latlong) +
  geom_sf(colour = "lightgrey", fill="lightgrey")+
  coord_sf(xlim = c(30, 65), ylim = c(0, -35), expand = FALSE) +
  geom_encircle(data = readme[which(readme$ocean == "indian"),],
                aes(x = Long_isl_center, y = Lat_isl_center, fill = biogeo_unit),
                alpha = 0.5, colour = NA, spread = 0.01, s_shape=0.5, expand=0.02)+
  scale_color_manual(name = "Biogeographic unit",
                     values = c("darkslategrey"),
                     aesthetics = c("color", "fill"),
                     labels = c("Indian Ocean")) +
  geom_point(data = readme[which(readme$ocean == "indian"),],
             aes(x = Long_isl_center, y = Lat_isl_center), colour = "black", size = 3, alpha = 2/3, shape = 16)+
  ylab("Longitude")+
  xlab("Latitude")+
  labs(fill = "Island group")+
  geom_text_repel(data = readme[which(readme$ocean == "indian"),],
                  aes(x = Long_isl_center, y = Lat_isl_center, label = island), 
                  fontface = "bold")+
  theme_pubr()

island_location_ind

ggsave(
  "islands_Indian.svg",
  plot = island_location_ind,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)





#### Pacific
worldMap <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()
target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")
offset <- 180 - 133


polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)


# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldMap %>% st_difference(polygon)

# Transform and crop
world3 <- world2 %>% st_transform(crs = target_crs)

world4 <- st_crop(
  x = world3, 
  y = st_as_sfc(
    st_bbox(c(xmin= 120, ymin = -40, xmax = 300, ymax = 30), crs = 4326)
  ) %>% st_transform(target_crs)
)

coords_eq <- readme %>%
  st_as_sf(coords = c("Long_isl_center", "Lat_isl_center"), crs = "EPSG:4326") %>%
  st_transform(crs = target_crs)

readme$Long_eq <- st_coordinates(coords_eq)[,1]
readme$Lat_eq <- st_coordinates(coords_eq)[,2]

island_location_pac1 <- ggplot(data = world4) +
  geom_sf(colour = "lightgrey", fill="lightgrey")+
  geom_encircle(data = readme[which(readme$ocean == "pacific"),], aes(x = Long_eq, y = Lat_eq, fill = biogeo_unit),
                alpha = 0.5, colour = NA, spread = 0.01, s_shape=0.5, expand=0.02)+
  scale_color_manual(name = "Biogeographic unit",
                     values = c("#32746D", "#0B3935", "steelblue4", "#66C2BB", "darkgreen", "#204725"),
                     aesthetics = c("color", "fill"))+
  geom_point(data = readme[which(readme$ocean == "pacific"),],
             aes(x = Long_eq, y = Lat_eq),
             colour = "black", size = 1, shape = 16)+
  geom_text_repel(data = readme[which(readme$ocean == "pacific"),],
                  aes(x = Long_eq, y = Lat_eq, label = island), 
                  fontface = "bold")+
  ylab("Longitude")+
  xlab("Latitude")+
  theme_pubr()


island_location_pac1

ggsave(
  "islands_Pacific.svg",
  plot = island_location_pac1,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 3000,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)





# environmental data ----
env_area <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = Island_size), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Island area (km²)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


env_iso <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = isolation), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Isolation from continent (km)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


env_elev <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = elev), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Elevation (m)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))

env_tri <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = tri), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Terrain Ruggedness Index (TRI)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


# convert settlements with the value ">5000" to "5000"
str(readme)
readme$settlement_BP[which(readme$settlement_BP == ">5000")] <- 500
readme$settlement_BP <- as.numeric(readme$settlement_BP)

env_sett <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = settlement_BP), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Onset of settlement (cal. yrs BP)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


env_temp <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = avg_temp), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Mean annual temperature (°C)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


env_precip <- ggplot(data = world) +
  geom_sf(colour = "grey", fill="grey")+
  geom_point(data = readme, aes(x = Long_moll, y = Lat_moll, colour = factor(ocean), size = sum_prec), alpha = 2/3, shape = 16)+
  theme_pubr()+
  ylab("Longitude")+
  xlab("Latitude")+
  scale_color_manual(labels = c("Atlantic", "Indian Ocean", "Mediterranean Sea", "Pacific"), values = c("#FFC100", "darkslategrey", "#CC998D", "#32746D"))+
  guides(color="none", size = guide_legend(""))+
  ggtitle("Annual precipitation (mm)")+
  scale_x_continuous(breaks = c(-180, 0, 180),
                     labels = c("180°", "0°", "180°"))


env_descriptive <- ggarrange(env_area, env_iso, env_elev, env_tri, env_temp, env_precip, env_sett, get_legend(island_location), labels = c("A", "B", "C", "S", "E", "F", "G"), ncol = 2, nrow = 4)

env_descriptive

ggsave(
  "env_descriptive.svg",
  plot = env_descriptive,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 3000,
  height = 3200,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)
