# Calculate and prepare environmental data ####





# packages ----
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(geodata)
library(geosphere)





# get data ----

## prepared pollen data ----
# load previously calculated similarity indices for all site comparisons
load("./Data/R_products/similarity_islands.RData")




## polygones islands ----
# location of all study islands
islands_geo <- st_read("./Data/environmental_data/islands_pollen.kml")
str(islands_geo)
islands_geo_sv <- vect(islands_geo) # convert to spatial vector





## temperature ----
# get mean annual temperature data (based on monthly average) for islands
worldclim_global(var = "tavg",
                 res = 0.5, # minutes of a degree
                 path  = "./Data/environmental_data/worldclim_geodata/temperature/")

# read in temperature data (average from 1970-2000)
tavg_01 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_01.tif")
tavg_02 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_02.tif")
tavg_03 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_03.tif")
tavg_04 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_04.tif")
tavg_05 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_05.tif")
tavg_06 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_06.tif")
tavg_07 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_07.tif")
tavg_08 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_08.tif")
tavg_09 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_09.tif")
tavg_10 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_10.tif")
tavg_11 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_11.tif")
tavg_12 <- rast("./Data/environmental_data/worldclim_geodata/temperature/wc2.1_30s/wc2.1_30s_tavg_12.tif")

# extract values per island into df
tavg_01_by_island <- terra::extract(tavg_01, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_02_by_island <- terra::extract(tavg_02, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_03_by_island <- terra::extract(tavg_03, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_04_by_island <- terra::extract(tavg_04, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_05_by_island <- terra::extract(tavg_05, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_06_by_island <- terra::extract(tavg_06, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_07_by_island <- terra::extract(tavg_07, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_08_by_island <- terra::extract(tavg_08, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_09_by_island <- terra::extract(tavg_09, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_10_by_island <- terra::extract(tavg_10, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_11_by_island <- terra::extract(tavg_11, islands_geo_sv, touches = TRUE, small = TRUE)
tavg_12_by_island <- terra::extract(tavg_12, islands_geo_sv, touches = TRUE, small = TRUE)

# group and average values per island
mean_tavg_01 <- 
  tavg_01_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_01 = mean(wc2.1_30s_tavg_01, na.rm = TRUE))

mean_tavg_02 <- 
  tavg_02_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_02 = mean(wc2.1_30s_tavg_02, na.rm = TRUE))

mean_tavg_03 <- 
  tavg_03_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_03 = mean(wc2.1_30s_tavg_03, na.rm = TRUE))

mean_tavg_04 <- 
  tavg_04_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_04 = mean(wc2.1_30s_tavg_04, na.rm = TRUE))

mean_tavg_05 <- 
  tavg_05_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_05 = mean(wc2.1_30s_tavg_05, na.rm = TRUE))

mean_tavg_06 <- 
  tavg_06_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_06 = mean(wc2.1_30s_tavg_06, na.rm = TRUE))

mean_tavg_07 <- 
  tavg_07_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_07 = mean(wc2.1_30s_tavg_07, na.rm = TRUE))

mean_tavg_08 <- 
  tavg_08_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_08 = mean(wc2.1_30s_tavg_08, na.rm = TRUE))

mean_tavg_09 <- 
  tavg_09_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_09 = mean(wc2.1_30s_tavg_09, na.rm = TRUE))

mean_tavg_10 <- 
  tavg_10_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_10 = mean(wc2.1_30s_tavg_10, na.rm = TRUE))

mean_tavg_11 <- 
  tavg_11_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_11 = mean(wc2.1_30s_tavg_11, na.rm = TRUE))

mean_tavg_12 <- 
  tavg_12_by_island %>% 
  group_by(ID) %>% 
  summarize(tavg_12 = mean(wc2.1_30s_tavg_12, na.rm = TRUE))

mean_tavg_all <- cbind(mean_tavg_01, mean_tavg_02[,2], mean_tavg_03[,2], mean_tavg_04[,2], mean_tavg_05[,2], mean_tavg_06[,2],
                       mean_tavg_07[,2], mean_tavg_08[,2], mean_tavg_09[,2], mean_tavg_10[,2], mean_tavg_11[,2], mean_tavg_12[,2])

# get annual mean
for(i in 1:nrow(mean_tavg_all)){
  mean_tavg_all$tavg_mean[i] <- sum(mean_tavg_all[i,2:13])/12
}

mean_tavg_all$island <- islands_geo_sv$Name





## precipitation ----
worldclim_global(var = "prec",
                 res = 0.5, # minutes of a degree
                 path  = "./Data/environmental_data/worldclim_geodata/precipitation/")

# read in precipitation data (average from 1970-2000)
prec_01 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_01.tif")
prec_02 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_02.tif")
prec_03 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_03.tif")
prec_04 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_04.tif")
prec_05 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_05.tif")
prec_06 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_06.tif")
prec_07 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_07.tif")
prec_08 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_08.tif")
prec_09 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_09.tif")
prec_10 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_10.tif")
prec_11 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_11.tif")
prec_12 <- rast("./Data/environmental_data/worldclim_geodata/precipitation/wc2.1_30s/wc2.1_30s_prec_12.tif")

# extract values into df
prec_01_by_island <- terra::extract(prec_01, islands_geo_sv, touches = TRUE, small = TRUE)
prec_02_by_island <- terra::extract(prec_02, islands_geo_sv, touches = TRUE, small = TRUE)
prec_03_by_island <- terra::extract(prec_03, islands_geo_sv, touches = TRUE, small = TRUE)
prec_04_by_island <- terra::extract(prec_04, islands_geo_sv, touches = TRUE, small = TRUE)
prec_05_by_island <- terra::extract(prec_05, islands_geo_sv, touches = TRUE, small = TRUE)
prec_06_by_island <- terra::extract(prec_06, islands_geo_sv, touches = TRUE, small = TRUE)
prec_07_by_island <- terra::extract(prec_07, islands_geo_sv, touches = TRUE, small = TRUE)
prec_08_by_island <- terra::extract(prec_08, islands_geo_sv, touches = TRUE, small = TRUE)
prec_09_by_island <- terra::extract(prec_09, islands_geo_sv, touches = TRUE, small = TRUE)
prec_10_by_island <- terra::extract(prec_10, islands_geo_sv, touches = TRUE, small = TRUE)
prec_11_by_island <- terra::extract(prec_11, islands_geo_sv, touches = TRUE, small = TRUE)
prec_12_by_island <- terra::extract(prec_12, islands_geo_sv, touches = TRUE, small = TRUE)

# group and average values per island
mean_prec_01 <- 
  prec_01_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_01 = mean(wc2.1_30s_prec_01, na.rm = TRUE))

mean_prec_02 <- 
  prec_02_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_02 = mean(wc2.1_30s_prec_02, na.rm = TRUE))

mean_prec_03 <- 
  prec_03_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_03 = mean(wc2.1_30s_prec_03, na.rm = TRUE))

mean_prec_04 <- 
  prec_04_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_04 = mean(wc2.1_30s_prec_04, na.rm = TRUE))

mean_prec_05 <- 
  prec_05_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_05 = mean(wc2.1_30s_prec_05, na.rm = TRUE))

mean_prec_06 <- 
  prec_06_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_06 = mean(wc2.1_30s_prec_06, na.rm = TRUE))

mean_prec_07 <- 
  prec_07_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_07 = mean(wc2.1_30s_prec_07, na.rm = TRUE))

mean_prec_08 <- 
  prec_08_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_08 = mean(wc2.1_30s_prec_08, na.rm = TRUE))

mean_prec_09 <- 
  prec_09_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_09 = mean(wc2.1_30s_prec_09, na.rm = TRUE))

mean_prec_10 <- 
  prec_10_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_10 = mean(wc2.1_30s_prec_10, na.rm = TRUE))

mean_prec_11 <- 
  prec_11_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_11 = mean(wc2.1_30s_prec_11, na.rm = TRUE))

mean_prec_12 <- 
  prec_12_by_island %>% 
  group_by(ID) %>% 
  summarize(prec_12 = mean(wc2.1_30s_prec_12, na.rm = TRUE))

mean_prec_all <- cbind(mean_prec_01, mean_prec_02[,2], mean_prec_03[,2], mean_prec_04[,2], mean_prec_05[,2], mean_prec_06[,2],
                       mean_prec_07[,2], mean_prec_08[,2], mean_prec_09[,2], mean_prec_10[,2], mean_prec_11[,2], mean_prec_12[,2])

# get total annual precipitation
for(i in 1:nrow(mean_prec_all)){
  mean_prec_all$prec_sum[i] <- sum(mean_prec_all[i,2:13])
}

mean_prec_all$island <- islands_geo_sv$Name




## elevation ---- 

elevation_global(res = 0.5,
                 path = "./Data/environmental_data/worldclim_geodata/elevation/")

elev <- rast("./Data/environmental_data/worldclim_geodata/elevation/wc2.1_30s/wc2.1_30s_elev.tif")
elev_by_island <- terra::extract(elev, islands_geo_sv, touches = TRUE, small = TRUE)


max_elev <- elev_by_island %>%
  group_by(ID) %>%
  na.omit("elev") %>%
  summarise(elev = max(wc2.1_30s_elev, na.rm=TRUE))

max_elev <- rbind(max_elev, data.frame(ID = 3, elev = NA))
max_elev <- max_elev[order(max_elev$ID),]

max_elev$island <- islands_geo_sv$Name





## Terraine Ruggendness Index ----

# source: https://www.earthenv.org/topography

tri <- rast("./Data/environmental_data/worldclim_geodata/terrain_ruggedness_index/tri_1KMmn_GMTEDmd.tif")
tri_by_island <- terra::extract(tri, islands_geo_sv, touches = TRUE, small = TRUE)

tri_df <- tri_by_island %>%
  group_by(ID) %>%
  summarise(tri = max(tri_1KMmn_GMTEDmd, na.rm=TRUE))

tri_df$island <- islands_geo_sv$Name




## distance to continent ----

# read in continental dat
cont <- st_read("./Data/continent_shp/USGSEsri_WCMC_continents.shp")

isolation <- st_distance(islands_geo, cont, by_element = FALSE)
isolation <- as.data.frame(isolation)
rownames(isolation) <- islands_geo$Name

isolation$V1 <- str_sub(isolation$V1, end = -4)
isolation$V2 <- str_sub(isolation$V2, end = -4)
isolation$V3 <- str_sub(isolation$V3, end = -4)
isolation$V4 <- str_sub(isolation$V4, end = -4)
isolation$V5 <- str_sub(isolation$V5, end = -4)
isolation$V6 <- str_sub(isolation$V6, end = -4)

isolation <- mutate_all(isolation, function(x) as.numeric(as.character(x)))
str(isolation)

isolation$min <- apply(isolation, 1, FUN = min)
isolation <- isolation/1000

isolation$island <- islands_geo_sv$Name



#  summaries ----
# summarize per island env data in readme table

readme$avg_temp <- NA
readme$sum_prec <- NA
readme$elev <- NA
readme$tri <- NA
readme$isolation <- NA

for(i in 1:nrow(readme)){
  if(readme$island[i] %in% mean_tavg_all$island){
  readme$avg_temp[i]      <- mean_tavg_all$tavg_mean[which(mean_tavg_all$island == readme$island[i])]
  readme$sum_prec[i]      <- mean_prec_all$prec_sum[which(mean_prec_all$island == readme$island[i])]
  readme$elev[i]          <- max_elev$elev[which(max_elev$island == readme$island[i])]
  readme$tri[i]           <- tri_df$tri[which(tri_df$island == readme$island[i])]
  readme$isolation[i]     <- isolation$min[which(isolation$island == readme$island[i])]
  }
}





# site comparison ----

# distance between sites
readme$Lat_isl_center <- as.numeric(readme$Lat_isl_center)
readme$Long_isl_center <- as.numeric(readme$Long_isl_center)

dat$distance <- NA
dat$delta_area <- NA
dat$delta_tri <- NA
dat$delta_isolation <- NA
dat$delta_elevation <- NA
dat$delta_temperature <- NA
dat$delta_precipitation <- NA

# calculate environmental parameters for all site comparisons
for(i in 1:nrow(readme)){
  for(j in 1:nrow(readme)){
    
        # distance between sites
         dist <- distm(c(readme$Long_isl_center[i], readme$Lat_isl_center[i]), c(readme$Long_isl_center[j], readme$Lat_isl_center[j]), fun = distGeo)

         dat$distance[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                      readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- dist/1000

         # delta island area
         size <- abs(readme$Island_size[i] - readme$Island_size[j])

         dat$delta_area[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                           readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- size

         # delta isolation from mainland
         iso <- abs(readme$isolation[i] - readme$isolation[j])

         dat$delta_isolation[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                           readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- iso
         
         # delta elevation
         elevation <- abs(readme$elev[i] - readme$elev[j])
         
         dat$delta_elevation[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                           readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- elevation
         
         # delta tri
         trindex <- abs(readme$tri[i] - readme$tri[j])
         
         dat$delta_tri[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                                readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- trindex
         
         # delta temperature
         temperature <- abs(readme$avg_temp[i] - readme$avg_temp[j])
         
         dat$delta_temperature[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                           readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- temperature
         
         # delta precipitation
         precipitation <- abs(readme$sum_prec[i] - readme$sum_prec[j])
         
         dat$delta_precipitation[which(grepl(paste0("(", readme$site[i], ".*", readme$site[j], "|",
                                           readme$site[j], ".*", readme$site[i], ")"), dat$name, ignore.case=TRUE) == TRUE)] <- precipitation
        
    }
}




# delta settlement time
dat$diff_settlement <- NA


for(i in 1:nrow(dat)){
  if(!is.na(readme$settlement_BP[which(readme$site == dat$site1[i])]) == TRUE && !is.na(readme$settlement_BP[which(readme$site == dat$site2[i])]) == TRUE){
  # both islands settled
 
  if(dat$times[i] < readme$settlement_BP[which(readme$site == dat$site1[i])] & dat$times[i] < readme$settlement_BP[which(readme$site == dat$site2[i])]){
   dat$diff_settlement[i] <- "both" 
  }
  
  # no island settled
  if(dat$times[i] > readme$settlement_BP[which(readme$site == dat$site1[i])] & dat$times[i] > readme$settlement_BP[which(readme$site == dat$site2[i])]){
    dat$diff_settlement[i] <- "none" 
  }
  
  # one island settled
    if(dat$times[i] < readme$settlement_BP[which(readme$site == dat$site1[i])] & dat$times[i] > readme$settlement_BP[which(readme$site == dat$site2[i])]){
    dat$diff_settlement[i] <- "one" 
  }
  
  if(dat$times[i] > readme$settlement_BP[which(readme$site == dat$site1[i])] & dat$times[i] < readme$settlement_BP[which(readme$site == dat$site2[i])]){
    dat$diff_settlement[i] <- "one" 
  }
  }
}




#### transfer information to different datasets ----

dat_global$distance <- NA
dat_global$delta_area <- NA
dat_global$delta_tri <- NA
dat_global$delta_isolation <- NA
dat_global$delta_elevation <- NA
dat_global$delta_temperature <- NA
dat_global$delta_precipitation <- NA
dat_global$diff_settlement <- NA

for(i in 1:nrow(dat_global)){
  dat_global$distance[i]<- dat$distance[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_area[i]<- dat$delta_area[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_tri[i]<- dat$delta_tri[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_isolation[i]<- dat$delta_isolation[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_elevation[i]<- dat$delta_elevation[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_temperature[i]<- dat$delta_temperature[which(dat$name == dat_global$name[i])][1]
  dat_global$delta_precipitation[i]<- dat$delta_precipitation[which(dat$name == dat_global$name[i])][1]
  dat_global$diff_settlement[i]<- dat$diff_settlement[which(dat$name == dat_global$name[i])][1]
}

dat_island_group$distance <- NA
dat_island_group$delta_area <- NA
dat_island_group$delta_tri <- NA
dat_island_group$delta_isolation <- NA
dat_island_group$delta_elevation <- NA
dat_island_group$delta_temperature <- NA
dat_island_group$delta_precipitation <- NA
dat_island_group$diff_settlement <- NA

for(i in 1:nrow(dat_island_group)){
  dat_island_group$distance[i]<- dat$distance[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_area[i]<- dat$delta_area[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_tri[i]<- dat$delta_tri[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_isolation[i]<- dat$delta_isolation[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_elevation[i]<- dat$delta_elevation[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_temperature[i]<- dat$delta_temperature[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$delta_precipitation[i]<- dat$delta_precipitation[which(dat$name == dat_island_group$name[i])][1]
  dat_island_group$diff_settlement[i]<- dat$diff_settlement[which(dat$name == dat_island_group$name[i])][1]
}

dat_archipelago$distance <- NA
dat_archipelago$delta_area <- NA
dat_archipelago$delta_tri <- NA
dat_archipelago$delta_isolation <- NA
dat_archipelago$delta_elevation <- NA
dat_archipelago$delta_temperature <- NA
dat_archipelago$delta_precipitation <- NA
dat_archipelago$diff_settlement <- NA

for(i in 1:nrow(dat_archipelago)){
  dat_archipelago$distance[i]<- dat$distance[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_area[i]<- dat$delta_area[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_tri[i]<- dat$delta_tri[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_isolation[i]<- dat$delta_isolation[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_elevation[i]<- dat$delta_elevation[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_temperature[i]<- dat$delta_temperature[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$delta_precipitation[i]<- dat$delta_precipitation[which(dat$name == dat_archipelago$name[i])][1]
  dat_archipelago$diff_settlement[i]<- dat$diff_settlement[which(dat$name == dat_archipelago$name[i])][1]
}


dat_biogeo$distance <- NA
dat_biogeo$delta_area <- NA
dat_biogeo$delta_tri <- NA
dat_biogeo$delta_isolation <- NA
dat_biogeo$delta_elevation <- NA
dat_biogeo$delta_temperature <- NA
dat_biogeo$delta_precipitation <- NA
dat_biogeo$diff_settlement <- NA

for(i in 1:nrow(dat_biogeo)){
  dat_biogeo$distance[i]<- dat$distance[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_area[i]<- dat$delta_area[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_tri[i]<- dat$delta_tri[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_isolation[i]<- dat$delta_isolation[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_elevation[i]<- dat$delta_elevation[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_temperature[i]<- dat$delta_temperature[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$delta_precipitation[i]<- dat$delta_precipitation[which(dat$name == dat_biogeo$name[i])][1]
  dat_biogeo$diff_settlement[i]<- dat$diff_settlement[which(dat$name == dat_biogeo$name[i])][1]
}






# save data ----
# These include environmental variables
save(readme, # metadata
     list_all, # all data
     list_cons, list_cons_bin, # str 1, unbinned and binned
     list_bold, list_bold_bin, # str 2, unbinned and binned
     
     dat, dat_con, # island-island comparison for str 1 and 2, for all three indices
     dat_global, dat_global_con, # dataframe with all island-island comparisons and geographic categorization
     dat_island_group, # data classified by island group
     dat_archipelago, # data classified by archipelago
     dat_biogeo, # data classified by biogeographic region
     
     master_bold_df, master_cons_df, # alls islands, all pollen data, all time steps
     stand_master, # taxonomic standardization master table
     island_group_stats, # summary values per island group
     file = "./Data/R_products/similarity_islands.RData")
