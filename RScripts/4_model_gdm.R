# Generalized Dissimilarity Modeling #####





# packages ----
library(gdm)
library(tidyverse)
library(viridis)
library(ggpubr)




# load data ----
load("./Data/R_products/similarity_islands.RData")





# prepare data for gdm ----

# standardize all data
for(i in 1:length(list_bold)){
  meta <- list_bold[[i]][,c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- list_bold[[i]][!names(list_bold[[i]]) %in% c("Cal_yrs_BP", "max", "min", "site")]
  sdt <- sdt / rowSums(sdt) * 100 # get percentage data

  names(sdt) <- stand_master$`alltaxa_version3_(conservative approach)`[match(names(sdt), stand_master$alltaxa)]
  sdt<-as.data.frame(do.call(cbind, by(t(sdt),INDICES=names(sdt),FUN=colSums)))
  list_bold[[i]] <- cbind(meta, sdt)
  list_bold[[i]] <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP < 5000),] # only data until -5000
}


# get dataframes per timestep

# for new run, please remove the following
# rm("dat_prep.base", "dat_prep.0", "dat_prep.1", "dat_prep.2", "dat_prep.3", "dat_prep.4", "dat_prep.5", "dat_prep.6", "dat_prep.7", "dat_prep.8", "dat_prep.9")

# times < 0
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP < 0),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.base") == TRUE){
      dat_prep.base <- rbind(dat_prep.base, sub)
    }else{
      dat_prep.base <- sub
    }
  }
}

# times 1 - 500 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(1:500)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.0") == TRUE){
      dat_prep.0 <- rbind(dat_prep.0, sub)
    }else{
      dat_prep.0 <- sub
    }
  }
}

# times 501 - 1000
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(501:1000)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA

    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.1") == TRUE){
      dat_prep.1 <- rbind(dat_prep.1, sub)
    }else{
      dat_prep.1 <- sub
    }
  }
}

# times 1001 - 1500 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(1001:1500)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.2") == TRUE){
      dat_prep.2 <- rbind(dat_prep.2, sub)
    }else{
      dat_prep.2 <- sub
    }
  }
}

# times 1501-2000 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(1501:2000)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.3") == TRUE){
      dat_prep.3 <- rbind(dat_prep.3, sub)
    }else{
      dat_prep.3 <- sub
    }
  }
}

# times 2001 - 2500 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(2001:2500)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.4") == TRUE){
      dat_prep.4 <- rbind(dat_prep.4, sub)
    }else{
      dat_prep.4 <- sub
    }
  }
}

# times 2501 - 3000
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(2501:3000)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.5") == TRUE){
      dat_prep.5 <- rbind(dat_prep.5, sub)
    }else{
      dat_prep.5 <- sub
    }
  }
}


# times 3001-3500 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(3001:3500)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.6") == TRUE){
      dat_prep.6 <- rbind(dat_prep.6, sub)
    }else{
      dat_prep.6 <- sub
    }
  }
}

# times 3501-4000 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(3501:4000)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.7") == TRUE){
      dat_prep.7 <- rbind(dat_prep.7, sub)
    }else{
      dat_prep.7 <- sub
    }
  }
}

# times 4001-4500
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(4001:4500)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.8") == TRUE){
      dat_prep.8 <- rbind(dat_prep.8, sub)
    }else{
      dat_prep.8 <- sub
    }
  }
}

# times 4501-5000 
for(i in 1:length(list_bold)){
  
  sub <- list_bold[[i]][which(list_bold[[i]]$Cal_yrs_BP %in% c(4501:5000)),]
  if(nrow(sub) >= 1){
    sub <- sub[1,]
    sub <- sub[-which(colnames(sub) %in% c("Cal_yrs_BP", "max", "min", "site"))]
    sub <- as.data.frame(cbind(colnames(sub), t(sub)))
    sub$site <- names(list_bold)[i]
    rownames(sub) <- NULL
    colnames(sub) <- c("species", "abundance", "site")
    
    sub$Long <- NA
    sub$Lat <- NA
    sub$area <- NA
    sub$isolation <- NA
    sub$tri <- NA
    sub$precipitation <- NA
    sub$temperature <- NA
    
    sub$Long          <- readme$Long_isl_center[which(readme$site == names(list_bold)[i])]
    sub$Lat           <- readme$Lat_isl_center[which(readme$site == names(list_bold)[i])]
    sub$area          <- readme$Island_size[which(readme$site == names(list_bold)[i])]
    sub$isolation     <- readme$isolation[which(readme$site == names(list_bold)[i])]
    sub$tri           <- readme$tri[which(readme$site == names(list_bold)[i])]
    sub$precipitation <- readme$sum_prec[which(readme$site == names(list_bold)[i])]
    sub$temperature   <- readme$avg_temp[which(readme$site == names(list_bold)[i])]
    
    if(exists("dat_prep.9") == TRUE){
      dat_prep.9 <- rbind(dat_prep.9, sub)
    }else{
      dat_prep.9 <- sub
    }
  }
}


## formatting ----
dat_prep.base$abundance <- as.numeric(dat_prep.base$abundance)
dat_prep.base <- dat_prep.base[which(dat_prep.base$abundance >= 0),]
dat_prep.base <- dat_prep.base[which(complete.cases(dat_prep.base)),]

dat_form.base <- formatsitepair(dat_prep.base[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.base[,c(3,4:10)])

dat_prep.0$abundance <- as.numeric(dat_prep.0$abundance)
dat_prep.0 <- dat_prep.0[which(dat_prep.0$abundance >= 0),]
dat_prep.0 <- dat_prep.0[which(complete.cases(dat_prep.0)),]

dat_form.0 <- formatsitepair(dat_prep.0[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.0[,c(3,4:10)])

dat_prep.1$abundance <- as.numeric(dat_prep.1$abundance)
dat_prep.1 <- dat_prep.1[which(dat_prep.1$abundance >= 0),]
dat_prep.1 <- dat_prep.1[which(complete.cases(dat_prep.1)),]

dat_form.1 <- formatsitepair(dat_prep.1[,1:5], bioFormat=2, abundColumn= "abundance",
                           XColumn="Long", YColumn="Lat",
                          sppColumn="species", siteColumn="site",
                          predData=dat_prep.1[,c(3,4:10)])

dat_prep.2$abundance <- as.numeric(dat_prep.2$abundance)
dat_prep.2 <- dat_prep.2[which(dat_prep.2$abundance >= 0),]
dat_prep.2 <- dat_prep.2[which(complete.cases(dat_prep.2)),]

dat_form.2 <- formatsitepair(dat_prep.2[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.2[,c(3,4:10)])

dat_prep.3$abundance <- as.numeric(dat_prep.3$abundance)
dat_prep.3 <- dat_prep.3[which(dat_prep.3$abundance >= 0),]
dat_prep.3 <- dat_prep.3[which(complete.cases(dat_prep.3)),]

dat_form.3 <- formatsitepair(dat_prep.3[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.3[,c(3,4:10)])

dat_prep.4$abundance <- as.numeric(dat_prep.4$abundance)
dat_prep.4 <- dat_prep.4[which(dat_prep.4$abundance >= 0),]
dat_prep.4 <- dat_prep.4[which(complete.cases(dat_prep.4)),]

dat_form.4 <- formatsitepair(dat_prep.4[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.4[,c(3,4:10)])

dat_prep.5$abundance <- as.numeric(dat_prep.5$abundance)
dat_prep.5 <- dat_prep.5[which(dat_prep.5$abundance >= 0),]
dat_prep.5 <- dat_prep.5[which(complete.cases(dat_prep.5)),]

dat_form.5 <- formatsitepair(dat_prep.5[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.5[,c(3,4:10)])

dat_prep.6$abundance <- as.numeric(dat_prep.6$abundance)
dat_prep.6 <- dat_prep.6[which(dat_prep.6$abundance >= 0),]
dat_prep.6 <- dat_prep.6[which(complete.cases(dat_prep.6)),]

dat_form.6 <- formatsitepair(dat_prep.6[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.6[,c(3,4:10)])

dat_prep.7$abundance <- as.numeric(dat_prep.7$abundance)
dat_prep.7 <- dat_prep.7[which(dat_prep.7$abundance >= 0),]
dat_prep.7 <- dat_prep.7[which(complete.cases(dat_prep.7)),]

dat_form.7 <- formatsitepair(dat_prep.7[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.7[,c(3,4:10)])

dat_prep.8$abundance <- as.numeric(dat_prep.8$abundance)
dat_prep.8 <- dat_prep.8[which(dat_prep.8$abundance >= 0),]
dat_prep.8 <- dat_prep.8[which(complete.cases(dat_prep.8)),]

dat_form.8 <- formatsitepair(dat_prep.8[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.8[,c(3,4:10)])

dat_prep.9$abundance <- as.numeric(dat_prep.9$abundance)
dat_prep.9 <- dat_prep.9[which(dat_prep.9$abundance >= 0),]
dat_prep.9 <- dat_prep.9[which(complete.cases(dat_prep.9)),]

dat_form.9 <- formatsitepair(dat_prep.9[,1:5], bioFormat=2, abundColumn= "abundance",
                             XColumn="Long", YColumn="Lat",
                             sppColumn="species", siteColumn="site",
                             predData=dat_prep.9[,c(3,4:10)])




# models ----
gdm.base <- gdm(dat_form.base, geo=T)
length(gdm.base$predictors)
summary(gdm.base)
gdm.crossvalidation(dat_form.base)

gdm.0 <- gdm(dat_form.0, geo=T)
length(gdm.0$predictors)
summary(gdm.0)
gdm.crossvalidation(dat_form.0)

gdm.1 <- gdm(dat_form.1, geo=T)
length(gdm.1$predictors)
summary(gdm.1)
gdm.crossvalidation(dat_form.1)

gdm.2 <- gdm(dat_form.2, geo=T)
length(gdm.2$predictors)
summary(gdm.2)
gdm.crossvalidation(dat_form.2)

gdm.3 <- gdm(dat_form.3, geo=T)
length(gdm.3$predictors)
summary(gdm.3)
gdm.crossvalidation(dat_form.3)

gdm.4 <- gdm(dat_form.4, geo=T)
length(gdm.4$predictors)
summary(gdm.4)
gdm.crossvalidation(dat_form.4)

gdm.5 <- gdm(dat_form.5, geo=T)
length(gdm.5$predictors)
summary(gdm.5)
gdm.crossvalidation(dat_form.5)

gdm.6 <- gdm(dat_form.6, geo=T)
length(gdm.6$predictors)
summary(gdm.6)
gdm.crossvalidation(dat_form.6)

gdm.7 <- gdm(dat_form.7, geo=T)
length(gdm.7$predictors)
summary(gdm.7)
gdm.crossvalidation(dat_form.7)

gdm.8 <- gdm(dat_form.8, geo=T)
length(gdm.8$predictors)
summary(gdm.8)
gdm.crossvalidation(dat_form.8)

gdm.9 <- gdm(dat_form.9, geo=T)
length(gdm.9$predictors)
summary(gdm.9)
gdm.crossvalidation(dat_form.9)






# plotting ----

# test plots to check
x11()
plot(gdm.0, plot.layout=c(3,3))
par(mfrow=c(1,1))





# extracts splines ----
exSplines.base <- isplineExtract(gdm.base)
exSplines.0 <- isplineExtract(gdm.0)
exSplines.1 <- isplineExtract(gdm.1)
exSplines.2 <- isplineExtract(gdm.2)
exSplines.3 <- isplineExtract(gdm.3)
exSplines.4 <- isplineExtract(gdm.4)
exSplines.5 <- isplineExtract(gdm.5)
exSplines.6 <- isplineExtract(gdm.6)
exSplines.7 <- isplineExtract(gdm.7)
exSplines.8 <- isplineExtract(gdm.8)
exSplines.9 <- isplineExtract(gdm.9)

splines_df.base <- data.frame(exSplines.base[[1]])
splines_df.base$time <- "< 0"
splines_df.base <- cbind(splines_df.base, exSplines.base[[2]])
colnames(splines_df.base) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.0 <- data.frame(exSplines.0[[1]])
splines_df.0$time <- "0 to 500"
splines_df.0 <- cbind(splines_df.0, exSplines.0[[2]])
colnames(splines_df.0) <- c("Geographic", "area", "isolation", "tri",
                          "precipitation", "temperature", "time", "Geographic_dist",
                          "area_dist", "isolation_dist", "tri_dist",
                          "precipitation_dist", "temperature_dist" )

splines_df.1 <- data.frame(exSplines.1[[1]])
splines_df.1$time <- "501 to 1000"
splines_df.1 <- cbind(splines_df.1, exSplines.1[[2]])
colnames(splines_df.1) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.2 <- data.frame(exSplines.2[[1]])
splines_df.2$time <- "1001 to 1500"
splines_df.2 <- cbind(splines_df.2, exSplines.2[[2]])
colnames(splines_df.2) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.3 <- data.frame(exSplines.3[[1]])
splines_df.3$time <- "1501 to 2000"
splines_df.3 <- cbind(splines_df.3, exSplines.3[[2]])
colnames(splines_df.3) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.4 <- data.frame(exSplines.4[[1]])
splines_df.4$time <- "2001 to 2500"
splines_df.4 <- cbind(splines_df.4, exSplines.4[[2]])
colnames(splines_df.4) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.5 <- data.frame(exSplines.5[[1]])
splines_df.5$time <- "2501 to 3000"
splines_df.5 <- cbind(splines_df.5, exSplines.5[[2]])
colnames(splines_df.5) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.6 <- data.frame(exSplines.6[[1]])
splines_df.6$time <- "3001 to 3500"
splines_df.6 <- cbind(splines_df.6, exSplines.6[[2]])
colnames(splines_df.6) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.7 <- data.frame(exSplines.7[[1]])
splines_df.7$time <- "3501 to 4000"
splines_df.7 <- cbind(splines_df.7, exSplines.7[[2]])
colnames(splines_df.7) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.8 <- data.frame(exSplines.8[[1]])
splines_df.8$time <- "4001 to 4500"
splines_df.8 <- cbind(splines_df.8, exSplines.8[[2]])
colnames(splines_df.8) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )

splines_df.9 <- data.frame(exSplines.9[[1]])
splines_df.9$time <- "4501 to 5000"
splines_df.9 <- cbind(splines_df.9, exSplines.9[[2]])
colnames(splines_df.9) <- c("Geographic", "area", "isolation", "tri",
                            "precipitation", "temperature", "time", "Geographic_dist",
                            "area_dist", "isolation_dist", "tri_dist",
                            "precipitation_dist", "temperature_dist" )


splines_df_all <- bind_rows(splines_df.base, splines_df.0, splines_df.1, splines_df.2, splines_df.3,
                            splines_df.4, splines_df.5, splines_df.6, splines_df.7,
                            splines_df.8, splines_df.9)

str(splines_df_all)

splines_df_all$time <- as.factor(splines_df_all$time)
splines_df_all$time <- ordered(splines_df_all$time,
                                 levels = c("< 0", "0 to 500", "501 to 1000",
                                            "1001 to 1500", "1501 to 2000",
                                            "2001 to 2500", "2501 to 3000",
                                            "3001 to 3500", "3501 to 4000",
                                            "4001 to 4500","4501 to 5000"))

# get mean splines summarizing all time steps

splines_df_mean <- data.frame(Geographic <- rep(NA, 200))
names(splines_df_mean) <- "Geographic"

splines_df_mean$Geographic <- (splines_df.base$Geographic +
                               splines_df.0$Geographic +
                               splines_df.1$Geographic +
                               splines_df.2$Geographic +
                               splines_df.3$Geographic +
                               splines_df.4$Geographic +
                               splines_df.5$Geographic +
                               splines_df.6$Geographic +
                               splines_df.7$Geographic +
                               splines_df.8$Geographic +
                               splines_df.9$Geographic)/11

splines_df_mean$Geographic_dist <- (splines_df.base$Geographic_dist +
                                      splines_df.0$Geographic_dist +
                                 splines_df.1$Geographic_dist +
                                 splines_df.2$Geographic_dist +
                                 splines_df.3$Geographic_dist +
                                 splines_df.4$Geographic_dist +
                                 splines_df.5$Geographic_dist +
                                 splines_df.6$Geographic_dist +
                                 splines_df.7$Geographic_dist +
                                 splines_df.8$Geographic_dist +
                                 splines_df.9$Geographic_dist)/11

splines_df_mean$area <- (splines_df.base$area +
                           splines_df.0$area +
                                 splines_df.1$area +
                                 splines_df.2$area +
                                 splines_df.3$area +
                                 splines_df.4$area +
                                 splines_df.5$area +
                                 splines_df.6$area +
                                 splines_df.7$area +
                                 splines_df.8$area +
                                 splines_df.9$area)/11

splines_df_mean$area_dist <- (splines_df.base$area_dist +
                                splines_df.0$area_dist +
                                 splines_df.1$area_dist +
                                 splines_df.2$area_dist +
                                 splines_df.3$area_dist +
                                 splines_df.4$area_dist +
                                 splines_df.5$area_dist +
                                 splines_df.6$area_dist +
                                 splines_df.7$area_dist +
                                 splines_df.8$area_dist +
                                 splines_df.9$area_dist)/11

splines_df_mean$isolation <- (splines_df.base$isolation +
                                splines_df.0$isolation +
                                 splines_df.1$isolation +
                                 splines_df.2$isolation +
                                 splines_df.3$isolation +
                                 splines_df.4$isolation +
                                 splines_df.5$isolation +
                                 splines_df.6$isolation +
                                 splines_df.7$isolation +
                                 splines_df.8$isolation +
                                 splines_df.9$isolation)/11

splines_df_mean$isolation_dist <- (splines_df.base$isolation_dist +
                                     splines_df.0$isolation_dist +
                                 splines_df.1$isolation_dist +
                                 splines_df.2$isolation_dist +
                                 splines_df.3$isolation_dist +
                                 splines_df.4$isolation_dist +
                                 splines_df.5$isolation_dist +
                                 splines_df.6$isolation_dist +
                                 splines_df.7$isolation_dist +
                                 splines_df.8$isolation_dist +
                                 splines_df.9$isolation_dist)/11

splines_df_mean$tri <- (splines_df.base$tri +
                          splines_df.0$tri +
                                 splines_df.1$tri +
                                 splines_df.2$tri +
                                 splines_df.3$tri +
                                 splines_df.4$tri +
                                 splines_df.5$tri +
                                 splines_df.6$tri +
                                 splines_df.7$tri +
                                 splines_df.8$tri +
                                 splines_df.9$tri)/11

splines_df_mean$tri_dist <- (splines_df.base$tri_dist +
                               splines_df.0$tri_dist +
                                 splines_df.1$tri_dist +
                                 splines_df.2$tri_dist +
                                 splines_df.3$tri_dist +
                                 splines_df.4$tri_dist +
                                 splines_df.5$tri_dist +
                                 splines_df.6$tri_dist +
                                 splines_df.7$tri_dist +
                                 splines_df.8$tri_dist +
                                 splines_df.9$tri_dist)/11

splines_df_mean$precipitation <- (splines_df.base$precipitation +
                                    splines_df.0$precipitation +
                                 splines_df.1$precipitation +
                                 splines_df.2$precipitation +
                                 splines_df.3$precipitation +
                                 splines_df.4$precipitation +
                                 splines_df.5$precipitation +
                                 splines_df.6$precipitation +
                                 splines_df.7$precipitation +
                                 splines_df.8$precipitation +
                                 splines_df.9$precipitation)/11

splines_df_mean$precipitation_dist <- (splines_df.base$precipitation_dist +
                                         splines_df.0$precipitation_dist +
                                 splines_df.1$precipitation_dist +
                                 splines_df.2$precipitation_dist +
                                 splines_df.3$precipitation_dist +
                                 splines_df.4$precipitation_dist +
                                 splines_df.5$precipitation_dist +
                                 splines_df.6$precipitation_dist +
                                 splines_df.7$precipitation_dist +
                                 splines_df.8$precipitation_dist +
                                 splines_df.9$precipitation_dist)/11

splines_df_mean$temperature <- (splines_df.base$temperature +
                                  splines_df.0$temperature +
                                 splines_df.1$temperature +
                                 splines_df.2$temperature +
                                 splines_df.3$temperature +
                                 splines_df.4$temperature +
                                 splines_df.5$temperature +
                                 splines_df.6$temperature +
                                 splines_df.7$temperature +
                                 splines_df.8$temperature +
                                 splines_df.9$temperature)/11

splines_df_mean$temperature_dist <- (splines_df.base$temperature_dist +
                                       splines_df.0$temperature_dist +
                                 splines_df.1$temperature_dist +
                                 splines_df.2$temperature_dist +
                                 splines_df.3$temperature_dist +
                                 splines_df.4$temperature_dist +
                                 splines_df.5$temperature_dist +
                                 splines_df.6$temperature_dist +
                                 splines_df.7$temperature_dist +
                                 splines_df.8$temperature_dist +
                                 splines_df.9$temperature_dist)/11

# save data ----
save(splines_df_all,
     splines_df_mean,
     file = "./Data/R_products/gdm_splines.RData")
