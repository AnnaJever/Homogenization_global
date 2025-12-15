# Beta-diversity ####

# Aim: Calculate beta-diversity between all islands and time steps
# categorize diversity comparisons into oceans and archipelagos





# load packages ----
library(tidyverse)
library(reshape)
library(vegan)





# get data ----

## taxa ----
# get standardized taxa (most recent version)
stand_master <-readxl::read_xlsx("./Data/revised_taxon_lists/AllPollenTaxaPOWO_revised_2025_07_25.xlsx", sheet = 1)

## island data ----
# get island meta data
readme <- readxl::read_xlsx("./Data/2025_07_16_island_characteristics.xlsx")

## pollen data ----
# get pollen data
list_all <- list()

for(i in 1:length(list.files("./Data/pollen_data_clean/"))){
  list_all[[i]]       <- read.csv(paste0("./Data/pollen_data_clean/",list.files("./Data/pollen_data_clean/")[i]), check.names = FALSE)[,-c(1)]
  names(list_all)[i]  <- c(paste(readme$site[readme$file == list.files("./Data/pollen_data_clean/")[i]]))
  list_all[[i]]$site  <- readme$site[which(readme$file == list.files("./Data/pollen_data_clean/")[i])] # add site
}





### addition to pollen data ----

# replace all NA in pollen data with 0
list_all <- lapply(list_all, function(d) {d[is.na(d)] <- 0; d})

# get maximum and minimum ages of all cores, as well as number of samples
readme$max_age    <- NA
readme$min_age    <- NA
readme$sample_n   <- NA

for(i in 1:nrow(readme)){
  readme$max_age[i]  <- round(max(as.numeric(as.data.frame(list_all[readme$site[i]])[,grep("Cal_yrs_BP", names(as.data.frame(list_all[readme$site[i]])), value = TRUE)])), digits = 0)
  readme$min_age[i]  <- round(min(as.numeric(as.data.frame(list_all[readme$site[i]])[,grep("Cal_yrs_BP", names(as.data.frame(list_all[readme$site[i]])), value = TRUE)])), digits = 0)
  readme$sample_n[i] <- length(as.data.frame(list_all[readme$site[i]])[,grep("Cal_yrs_BP", names(as.data.frame(list_all[readme$site[i]])), value = TRUE)])
}





# site selection ----

# select only one site per island
island_summary <- readme %>% group_by(island) %>% summarise(n()) # groups data by island

island_summary$island[which(island_summary$`n()` >= 2)] # which islands have more than one core?
# for the islands Corsica, Eleuthera, Grande Terre, Iceland, Jan Mayen, Menorca and Viti Levu more than one pollen core is available
# select cores with greater length and more sample points and covering the entire sample period

# Corsica
readme[which(readme$island == "Corsica"), c("site", "max_age", "min_age", "sample_n")] # select Bastani, delete Saint Florent

# Eleuthera
readme[which(readme$island == "Eleuthera"), c("site", "max_age", "min_age", "sample_n")] # select Shad Pond, delete Duck Pond Blue Hole

# Grande Terre
readme[which(readme$island == "Grande Terre"), c("site", "max_age", "min_age", "sample_n")] # select St. Louis Lac, delete Plum Swamp

# Iceland
readme[which(readme$island == "Iceland"), c("site", "max_age", "min_age", "sample_n")] # select site Iceland, delete Kalmanstjörn Lake

# Jan Mayen
readme[which(readme$island == "Jan Mayen"), c("site", "max_age", "min_age", "sample_n")] # select Sjuhollendarbukta, delete Kvalrossbukta

# Menorca
readme[which(readme$island == "Menorca"), c("site", "max_age", "min_age", "sample_n")] # select Hort Timoner, delete Algendar, Cala'n Porter and Cala Galdana

# Viti Levu
readme[which(readme$island == "Viti Levu"), c("site", "max_age", "min_age", "sample_n")] # select Volivoli, delete Bonatoa Bog

# remove selected sites from meta file (readme) and pollen data file (list_all)
readme <- readme[-which(readme$site %in% c(
                  "Saint Florent",                    # Corsica
                  "Duck Pond Blue Hole",              # Eleuthera
                  "Plum Swamp",                       # Grande Terre
                  "Kalmanstjörn Lake",                # Iceland
                  "Kvalrossbukta",                    # Jan Mayen
                  "Algendar",                         # Menorca
                  "Cala'n Porter",                    # Menorca
                  "Cala Galdana",                     # Menorca
                  "Bonatoa Bog"                       # Viti Levu
                  )),]

list_all <- list_all[-which(names(list_all) %in% c(
                  "Saint Florent",                    # Corsica
                  "Duck Pond Blue Hole",              # Eleuthera
                  "Plum Swamp",                       # Grande Terre
                  "Kalmanstjörn Lake",                # Iceland
                  "Kvalrossbukta",                    # Jan Mayen
                  "Algendar",                         # Menorca
                  "Cala'n Porter",                    # Menorca
                  "Cala Galdana",                     # Menorca
                  "Bonatoa Bog"                       # Viti Levu
                  ))]





# data standardization ----

# pollen counts are standardized by calculating percentage data

# uncertainty in pollen taxonomy is embraced by taking amgiguity in pollen grain identification into account  
# they are hereafter called standardization 1 (bold approach) and 2 (conservative approach) of pollen taxa

# data is put into times bins between now and 5000 cal. years BP within the same loop

## standardization 1 (bold approach) ----
list_bold <- list_all
list_bold_bin <- list_all

for(i in 1:length(list_bold)){
  
  # pollen count standardization
  meta <- list_bold[[i]][,c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- list_bold[[i]][,!names(list_bold[[i]]) %in% c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- mutate_all(sdt, function(x) as.numeric(as.character(x)))
  sdt <- sdt / rowSums(sdt) * 100 # get percentage data
  
  # taxonomic standardization
  names(sdt) <- stand_master$`alltaxa_version2_(bold approach)`[match(names(sdt), stand_master$alltaxa)]
  sdt <- sdt[,which(!is.na(colnames(sdt)))]
  sdt <-as.data.frame(do.call(cbind, by(t(sdt),INDICES=names(sdt),FUN=colSums)))
  list_bold[[i]] <- cbind(meta, sdt)
  
  # time limit
  list_bold[[i]] <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP < 5000),] # only data until -5000
  
  # binning of data
  list_bold_bin[[i]] <- aggregate(sdt, by=list(cut(meta$Cal_yrs_BP,seq(-500,5000,500))), mean)
  list_bold_bin[[i]]$site <- unique(meta$site)
}

master_bold_df <- Reduce(function(x, y) merge(x, y, all=TRUE), c(unique(list_bold_bin))) %>% replace(is.na(.),0) # from list to df


## Standardization 2 (conservative approach) ----
list_cons <- list_all
list_cons_bin <- list_all

for(i in 1:length(list_cons)){
  
  # pollen count standardization
  meta <- list_cons[[i]][,c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- list_cons[[i]][,!names(list_cons[[i]]) %in% c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- mutate_all(sdt, function(x) as.numeric(as.character(x)))
  sdt <- sdt / rowSums(sdt) * 100 # get percentage data
  
  # taxonomic standardization
  names(sdt) <- stand_master$`alltaxa_version3_(conservative approach)`[match(names(sdt), stand_master$alltaxa)]
  sdt <- sdt[,which(!is.na(colnames(sdt)))]
  sdt <-as.data.frame(do.call(cbind, by(t(sdt),INDICES=names(sdt),FUN=colSums)))
  list_cons[[i]] <- cbind(meta, sdt)
  
  # time limit
  list_cons[[i]] <- list_cons[[i]][which(list_cons[[i]]$Cal_yrs_BP < 5000),] # only data until -5000
  
  # binning of data
  list_cons_bin[[i]] <- aggregate(sdt, by=list(cut(meta$Cal_yrs_BP,seq(-500,5000,500))), mean)
  list_cons_bin[[i]]$site <- unique(meta$site)
}

master_cons_df <- Reduce(function(x, y) merge(x, y, all=TRUE), c(unique(list_cons_bin))) %>% replace(is.na(.),0) # from list to data frame





# beta diversity calculation ----

# these calculations are done twice, once for the bold (standardization 1) and the conservative approach (standardization 2)

# Build unique pairs of sites to compare each with each
pairs<-expand.grid(unique(master_bold_df$site),unique(master_bold_df$site))

# delete those pairs that compare the same with the same site
# delete those pairs where A is paired with B and B is paired with a to avoid doubling

pairs$delete <- NA

for(i in 1:nrow(pairs)){
  for (j in 1:nrow(pairs)){
    if(is.na(pairs$delete[i])){
      if(pairs$Var1[i] == pairs$Var2[j] & pairs$Var2[i] == pairs$Var1[j]){
        pairs$delete[j] <- "delete"
      }
    }    
  }
}

pairs <- pairs[-which(pairs$delete == "delete"),]


pairs_con<-expand.grid(unique(master_cons_df$site),unique(master_cons_df$site))

# delete those pairs that compare the same with the same site
# delete those pairs where A is paired with B and B is paired with a to avoid doubling

pairs_con$delete <- NA

for(i in 1:nrow(pairs_con)){
  for (j in 1:nrow(pairs_con)){
    if(is.na(pairs_con$delete[i])){
      if(pairs_con$Var1[i] == pairs_con$Var2[j] & pairs_con$Var2[i] == pairs_con$Var1[j]){
        pairs_con$delete[j] <- "delete"
      }
    }    
  }
}

pairs_con <- pairs_con[-which(pairs_con$delete == "delete"),]



comp_b <-list()
comp_s <-list()
comp_j <-list()

comp_con_b <-list()
comp_con_s <-list()
comp_con_j <-list()

beta_index_bray <- "bray" # = sorensen if binary = TRUE
beta_index_jaccard <- "jaccard" # set binary = TRUE


# calculate beta diversity index for all sites pairs, all three beta diversity indices and both standardization approaches

for(i in 1:nrow(pairs)){
    
  a<-master_bold_df[master_bold_df$site ==pairs[i,1],]
  b<-master_bold_df[master_bold_df$site ==pairs[i,2],]	
  times<-intersect(a$Group.1, b$Group.1)
  
  # bray
  sims_b<-times
  sims_b[]<-NA
  for(d in times){
    sims_b[times==d]<-1-vegdist(rbind(as.numeric(a[a$Group.1==d, 3:ncol(master_bold_df)]),
                                      as.numeric(b[b$Group.1==d,3:ncol(master_bold_df)])),
                              method=paste(beta_index_bray))}
  
  comp_b[[i]]<-data.frame(times,sims_b)
  
  # sorensen
  sims_s<-times
  sims_s[]<-NA
  for(d in times){
    sims_s[times==d]<-1-vegdist(rbind(as.numeric(a[a$Group.1==d, 3:ncol(master_bold_df)]),
                                      as.numeric(b[b$Group.1==d,3:ncol(master_bold_df)])),
                                method=paste(beta_index_bray), binary = TRUE)}
  
  comp_s[[i]]<-data.frame(times,sims_s)
  
  # jaccard
  sims_j<-times
  sims_j[]<-NA
  for(d in times){
    sims_j[times==d]<-1-vegdist(rbind(as.numeric(a[a$Group.1==d, 3:ncol(master_bold_df)]),
                                      as.numeric(b[b$Group.1==d,3:ncol(master_bold_df)])),
                                method=paste(beta_index_jaccard), binary = TRUE)}
  
  comp_j[[i]]<-data.frame(times,sims_j)
}



for(i in 1:nrow(pairs_con)){
  a_con<-master_cons_df[master_cons_df$site ==pairs_con[i,1],]
  b_con<-master_cons_df[master_cons_df$site ==pairs_con[i,2],]	
  times<-intersect(a_con$Group.1, b_con$Group.1)
  
  # bray
  sims_b<-times
  sims_b[]<-NA
  for(d in times){
    sims_b[times==d]<-1-vegdist(rbind(as.numeric(a_con[a_con$Group.1==d,3:ncol(master_cons_df)]),
                                    as.numeric(b_con[b_con$Group.1==d,3:ncol(master_cons_df)])),
                              method=paste(beta_index_bray))}
  comp_con_b[[i]]<-data.frame(times,sims_b)
  
  # sorensen
  sims_s<-times
  sims_s[]<-NA
  for(d in times){
    sims_s[times==d]<-1-vegdist(rbind(as.numeric(a_con[a_con$Group.1==d,3:ncol(master_cons_df)])
                                    ,as.numeric(b_con[b_con$Group.1==d,3:ncol(master_cons_df)])),
                              method=paste(beta_index_bray), binary = TRUE)}
  comp_con_s[[i]]<-data.frame(times,sims_s)
  
  # jaccard
  sims_j<-times
  sims_j[]<-NA
  for(d in times){
    sims_j[times==d]<-1-vegdist(as.numeric(rbind(a_con[a_con$Group.1==d,3:ncol(master_cons_df)])
                                    ,as.numeric(b_con[b_con$Group.1==d,3:ncol(master_cons_df)])),
                              method=paste(beta_index_jaccard), binary = TRUE)}
  comp_con_j[[i]]<-data.frame(times,sims_j)
}


# data transformation into table
names(comp_b)<-paste(pairs[,1],pairs[,2])
names(comp_j)<-paste(pairs[,1],pairs[,2])
names(comp_s)<-paste(pairs[,1],pairs[,2])

dat_b<-do.call("rbind", comp_b)
dat_j<-do.call("rbind", comp_j)
dat_s<-do.call("rbind", comp_s)
dat <- cbind(dat_b, dat_j[,2], dat_s[,2])
colnames(dat) <- c("times", "sims_b", "sims_j", "sims_s")
dat$name<-unlist(lapply(strsplit(rownames(dat_b),split="[.]"),function(x){x[[1]]}))


names(comp_con_b)<-paste(pairs_con[,1],pairs_con[,2])
names(comp_con_j)<-paste(pairs_con[,1],pairs_con[,2])
names(comp_con_s)<-paste(pairs_con[,1],pairs_con[,2])

dat_con_b<-do.call("rbind", comp_con_b)
dat_con_j<-do.call("rbind", comp_con_j)
dat_con_s<-do.call("rbind", comp_con_s)
dat_con <- cbind(dat_con_b, dat_con_j[,2], dat_con_s[,2])
colnames(dat_con) <- c("times", "sims_b", "sims_j", "sims_s")
dat_con$name<-unlist(lapply(strsplit(rownames(dat_con),split="[.]"),function(x){x[[1]]}))


# get site one and two names
dat$site1 <- NA
dat$site2 <- NA

for(i in 1:nrow(readme)){
  for(j in 1:nrow(readme)){
    if(i != j){
      temp <- dat[which(grepl(readme$site[i], dat$name)),]
      temp <- temp[which(grepl(readme$site[j], temp$name)),]
      dat$site1[which(dat$name %in% temp$name)] <- readme$site[i] 
      dat$site2[which(dat$name %in% temp$name)] <- readme$site[j]
    }
  }
}

dat_con$site1 <- NA
dat_con$site2 <- NA

for(i in 1:nrow(readme)){
  for(j in 1:nrow(readme)){
    if(i != j){
      temp <- dat_con[which(grepl(readme$site[i], dat_con$name)),]
      temp <- temp[which(grepl(readme$site[j], temp$name)),]
      dat_con$site1[which(dat_con$name %in% temp$name)] <- readme$site[i] 
      dat_con$site2[which(dat_con$name %in% temp$name)] <- readme$site[j]
    }
  }
}



# standardize time bins
unique(dat$times)
dat$times[which(dat$times == "(-500,0]")] <- 0
dat$times[which(dat$times == "(0,500]")] <- 500
dat$times[which(dat$times == "(500,1e+03]")] <- 1000
dat$times[which(dat$times == "(1e+03,1.5e+03]")] <- 1500
dat$times[which(dat$times == "(1.5e+03,2e+03]")] <- 2000
dat$times[which(dat$times == "(2e+03,2.5e+03]")] <- 2500
dat$times[which(dat$times == "(2.5e+03,3e+03]")] <- 3000
dat$times[which(dat$times == "(3e+03,3.5e+03]")] <- 3500
dat$times[which(dat$times == "(3.5e+03,4e+03]")] <- 4000
dat$times[which(dat$times == "(4e+03,4.5e+03]")] <- 4500
dat$times[which(dat$times == "(4.5e+03,5e+03]")] <- 5000
str(dat)

unique(dat_con$times)
dat_con$times[which(dat_con$times == "(-500,0]")] <- 0
dat_con$times[which(dat_con$times == "(0,500]")] <- 500
dat_con$times[which(dat_con$times == "(500,1e+03]")] <- 1000
dat_con$times[which(dat_con$times == "(1e+03,1.5e+03]")] <- 1500
dat_con$times[which(dat_con$times == "(1.5e+03,2e+03]")] <- 2000
dat_con$times[which(dat_con$times == "(2e+03,2.5e+03]")] <- 2500
dat_con$times[which(dat_con$times == "(2.5e+03,3e+03]")] <- 3000
dat_con$times[which(dat_con$times == "(3e+03,3.5e+03]")] <- 3500
dat_con$times[which(dat_con$times == "(3.5e+03,4e+03]")] <- 4000
dat_con$times[which(dat_con$times == "(4e+03,4.5e+03]")] <- 4500
dat_con$times[which(dat_con$times == "(4.5e+03,5e+03]")] <- 5000
str(dat)

# similarity and times data as numeric
dat <- dat %>% mutate_at(c("sims_b", "sims_j", "sims_s", "times"), as.numeric)
dat_con <- dat_con %>% mutate_at(c("sims_b", "sims_j", "sims_s", "times"), as.numeric)




# data classification ----

## classify by ocean ----
atlantic <- readme$site[which(readme$ocean == "atlantic")]
indian_ocean <- readme$site[which(readme$ocean == "indian")]
pacific <- readme$site[which(readme$ocean == "pacific")]
mediterranean <- readme$site[which(readme$ocean == "mediterranean")]


# only Atlantic, Pacific etc     
dat_atl <- dat[rowSums(sapply(atlantic, grepl, dat$name)) > 1, ,drop = FALSE]
dat_ind <- dat[rowSums(sapply(indian_ocean, grepl, dat$name)) > 1, ,drop = FALSE]
dat_pac <- dat[rowSums(sapply(pacific, grepl, dat$name)) > 1, ,drop = FALSE]
dat_med <- dat[rowSums(sapply(mediterranean, grepl, dat$name)) > 1, ,drop = FALSE]

dat_atl$ocean <- "Atlantic"
dat_ind$ocean <- "Indian ocean"
dat_pac$ocean <- "Pacific"
dat_med$ocean <- "Mediterranean"

df_list <- list(dat_atl, dat_pac, dat_med, dat_ind)
dat_global <- df_list %>% reduce(full_join) # merge all data frames in list


dat_atl_con <- dat_con[rowSums(sapply(atlantic, grepl, dat_con$name)) > 1, ,drop = FALSE]
dat_ind_con <- dat_con[rowSums(sapply(indian_ocean, grepl, dat_con$name)) > 1, ,drop = FALSE]
dat_pac_con <- dat_con[rowSums(sapply(pacific, grepl, dat_con$name)) > 1, ,drop = FALSE]
dat_med_con <- dat[rowSums(sapply(mediterranean, grepl, dat_con$name)) > 1, ,drop = FALSE]

dat_atl_con$ocean <- "Atlantic"
dat_ind_con$ocean <- "Indian ocean"
dat_pac_con$ocean <- "Pacific"
dat_med_con$ocean <- "Mediterranean"

df_list_con <- list(dat_atl_con, dat_pac_con, dat_med_con, dat_ind_con)
dat_global_con <- df_list_con %>% reduce(full_join) # merge all data frames in list



## classify by island group ----

# add data about island groups
readme_island_group <- readme[which(!is.na(readme$island_group1)),]

dat_island_group <- data.frame(times <- NA, sims_b <- NA, sims_j <- NA, sims_s <- NA, name <- NA, site1 <- NA, site2 <- NA, zuordnung_island_group <- NA)
colnames(dat_island_group) <- c("times", "sims_b", "sims_j", "sims_s", "name", "site1", "site2", "zuordnung_island_group")

for(i in 1:nrow(readme_island_group)){
  temp1 <- dat[which(grepl(readme_island_group$site[i], dat$name)),]
  for(j in 1:nrow(readme_island_group)){
    temp2 <- temp1[which(grepl(readme_island_group$site[j], temp1$name)),]
    
    if(i != j && nrow(temp2)>= 1 && readme_island_group$island_group1[which(readme_island_group$site == readme_island_group$site[i])] == readme_island_group$island_group1[which(readme_island_group$site == readme_island_group$site[j])]){ 
      
      temp2$zuordnung_island_group <- readme_island_group$island_group1[which(readme_island_group$site == readme_island_group$site[i])]
      
      dat_island_group <- rbind(dat_island_group, temp2)
    }
  }
  
  rm(temp1)
  rm(temp2)
}

dat_island_group <- dat_island_group[which(complete.cases(dat_island_group)),]




## classify by archipelago ----

# add data about archipelagos
readme_archipelago <- readme[which(!is.na(readme$archipelago)),]

dat_archipelago <- data.frame(times <- NA, sims_b <- NA, sims_j <- NA, sims_s <- NA, name <- NA, site1 <- NA, site2 <- NA, zuordnung_archipel <- NA)
colnames(dat_archipelago) <- c("times", "sims_b", "sims_j", "sims_s", "name", "site1", "site2", "zuordnung_archipel")

for(i in 1:nrow(readme_archipelago)){
  temp1 <- dat[which(grepl(readme_archipelago$site[i], dat$name)),]
  for(j in 1:nrow(readme_archipelago)){
    temp2 <- temp1[which(grepl(readme_archipelago$site[j], temp1$name)),]
    
    if(i != j && nrow(temp2)>= 1 && readme_archipelago$archipelago[which(readme_archipelago$site == readme_archipelago$site[i])] == readme_archipelago$archipelago[which(readme_archipelago$site == readme_archipelago$site[j])]){ 
      
      temp2$zuordnung_archipel <- readme_archipelago$archipelago[which(readme_archipelago$site == readme_archipelago$site[i])]
      
      dat_archipelago <- rbind(dat_archipelago, temp2)
    }
  }
  
  rm(temp1)
  rm(temp2)
}

dat_archipelago <- dat_archipelago[which(complete.cases(dat_archipelago)),]






## classify by biogeographic region ----

# add data about biogeographic regions
readme_biogeo <- readme[which(!is.na(readme$biogeo_unit)),]

dat_biogeo <- data.frame(times <- NA, sims_b <- NA, sims_j <- NA, sims_s <- NA, name <- NA, site1 <- NA, site2 <- NA, zuordnung_biogeo <- NA)
colnames(dat_biogeo) <- c("times", "sims_b", "sims_j", "sims_s", "name", "site1", "site2", "zuordnung_biogeo")

for(i in 1:nrow(readme_biogeo)){
  temp1 <- dat[which(grepl(readme_biogeo$site[i], dat$name)),]
  for(j in 1:nrow(readme_biogeo)){
    temp2 <- temp1[which(grepl(readme_biogeo$site[j], temp1$name)),]
    
    if(i != j && nrow(temp2)>= 1 && readme_biogeo$biogeo_unit[which(readme_biogeo$site == readme_biogeo$site[i])] == readme_biogeo$biogeo_unit[which(readme_biogeo$site == readme_biogeo$site[j])]){ 
      
      temp2$zuordnung_biogeo <- readme_biogeo$biogeo_unit[which(readme_biogeo$site == readme_biogeo$site[i])]
      
      dat_biogeo <- rbind(dat_biogeo, temp2)
    }
  }
  
  rm(temp1)
  rm(temp2)
}

dat_biogeo <- dat_biogeo[which(complete.cases(dat_biogeo)),]





# island stats ----

# for each island group mean similarity value for last (sim_b_present) and for the first (sim_b_past) 500 years is valculated

island_group_stats <- data.frame(island_group = unique(dat_biogeo$zuordnung_biogeo))
island_group_stats$ocean <- c("Atlantic", "Pacific", "Pacific", "Atlantic", "Mediterranean", "Atlantic", "Atlantic", "Pacific", "Pacific", "Mediterranean", "Indian",  "Pacific")


for(i in 1:nrow(island_group_stats)){
  
  # Bray Curtis
  island_group_stats$sim_b_present[i] <- mean(dat_biogeo$sims_b[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                              dat_biogeo$times %in% c(0, 500, 1000))])
  island_group_stats$sim_b_past[i] <- mean(dat_biogeo$sims_b[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                           dat_biogeo$times %in% c(4000, 4500, 5000))])
  island_group_stats$simb_b_diff[i] <- island_group_stats$sim_b_past[i] - island_group_stats$sim_b_present[i]
  
  # Sorensen
  island_group_stats$sim_s_present[i] <- mean(dat_biogeo$sims_s[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                              dat_biogeo$times %in% c(0, 500, 1000))])
  island_group_stats$sim_s_past[i] <- mean(dat_biogeo$sims_s[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                           dat_biogeo$times %in% c(4000, 4500, 5000))])
  island_group_stats$simb_s_diff[i] <- island_group_stats$sim_s_past[i] - island_group_stats$sim_s_present[i]
  
  # Jaccard
  island_group_stats$sim_j_present[i] <- mean(dat_biogeo$sims_j[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                              dat_biogeo$times %in% c(0, 500, 1000))])
  island_group_stats$sim_j_past[i] <- mean(dat_biogeo$sims_j[which(dat_biogeo$zuordnung_biogeo == island_group_stats$island_group[i] &
                                                                           dat_biogeo$times %in% c(4000, 4500, 5000))])
  island_group_stats$simb_j_diff[i] <- island_group_stats$sim_j_past[i] - island_group_stats$sim_j_present[i]
  
  island_group_stats$mean_lat[i] <- mean(abs(as.numeric(readme$Lat_isl_center[which(readme$biogeo_unit == island_group_stats$island_group[i])])))
}

island_group_stats <- island_group_stats[order(island_group_stats$mean_lat),]

# Past similarities values missing for New Zealand Region and Galápagos as those pollen cores only date back 2000 years.




# save data ----
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
