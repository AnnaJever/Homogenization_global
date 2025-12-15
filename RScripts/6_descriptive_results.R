# Descriptive results ####



# get data ----
load("./Data/R_products/similarity_islands.RData")



# packages ----
library(grateful)




# numbers islands ----

table(readme$ocean)




# numbers global ----
# get mean values for first and last time bin for all three similarity indices
round(mean(dat$sims_b[which(dat$times == 5000)]), digits = 2) # 0.05
round(mean(dat$sims_j[which(dat$times == 5000)]), digits = 2) # 0.06
round(mean(dat$sims_s[which(dat$times == 5000)]), digits = 2) # 0.1

round(mean(dat$sims_b[which(dat$times == 0)]), digits = 2) # 0.08
round(mean(dat$sims_j[which(dat$times == 0)]), digits = 2) # 0.07
round(mean(dat$sims_s[which(dat$times == 0)]), digits = 2) # 0.13

(round(mean(dat$sims_b[which(dat$times == 500)]), digits = 2)-round(mean(dat$sims_b[which(dat$times == 5000)]), digits = 2))/round(mean(dat$sims_b[which(dat$times == 5000)]), digits = 2)*100 # 60
(round(mean(dat$sims_j[which(dat$times == 500)]), digits = 2)-round(mean(dat$sims_j[which(dat$times == 5000)]), digits = 2))/round(mean(dat$sims_j[which(dat$times == 5000)]), digits = 2)*100 # 17
(round(mean(dat$sims_s[which(dat$times == 500)]), digits = 2)-round(mean(dat$sims_s[which(dat$times == 5000)]), digits = 2))/round(mean(dat$sims_s[which(dat$times == 5000)]), digits = 2)*100 # 30





# numbers ocean ----
round(mean(dat_global$sims_b[which(dat_global$times == 0 & dat_global$ocean == "Atlantic")]), digits = 2) # 0.07
round(mean(dat_global$sims_b[which(dat_global$times == 0 & dat_global$ocean == "Mediterranean")]), digits = 2) # 0.08
round(mean(dat_global$sims_b[which(dat_global$times == 0 & dat_global$ocean == "Pacific")]), digits = 2) # 0.13

round(mean(dat_global$sims_b[which(dat_global$times == 5000 & dat_global$ocean == "Atlantic")]), digits = 2) # 0.08
round(mean(dat_global$sims_b[which(dat_global$times == 5000 & dat_global$ocean == "Mediterranean")]), digits = 2) # 0.17
round(mean(dat_global$sims_b[which(dat_global$times == 5000 & dat_global$ocean == "Pacific")]), digits = 2) # 0.05

(0.07-0.08)/0.08*100 # -14%
(0.08-0.17)/0.17*100 # -53% Mediterranean
(0.13-0.05)/0.05*100 # 160%



# cite packages
cite_packages(output = "file",
              out.dir = ".",
              out.format = "docx",
              cite.tidyverse = TRUE,
              dependencies = FALSE,
              include.RStudio = FALSE,
              passive.voice = FALSE)
