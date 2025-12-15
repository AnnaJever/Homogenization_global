# Visualization - figures ####





# colour code ----

# Air force blue: #638591
# Barn red: #7C0902
# potential colours in between: #677680, #6a666e, #6D575C, #70474A, #762826





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
library(ggrepel)
library(grid)
library(viridis)





# load data ----
load("./Data/R_products/similarity_islands.RData")
load("./Data/R_products/similarity_islands_NG.RData")
load("./Data/R_products/gdm_splines.RData")





# Figures ----

## Figure 2 ----

### Figure 2a ----
# figure global

plot_all <- ggplot()+
  geom_point(data = dat, aes(times, sims_b), col = "#7C0902", alpha = 1/100, size = 2)+
  stat_smooth(data = dat, aes(times, sims_b), col = "#7C0902", span = 1, se = T, linewidth = 1, fill = "#7C0902", alpha = 1/2, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,2500,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.21))+
  theme_pubr(base_size = 16)

plot_all

ggsave(
  "figure2a.png",
  plot = plot_all,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# redone as sensitivity analysis with (plot SI Figure 5)
  # 1) for all three beta diversity indices,
  # 2) with and without grasses (on the basis of Bray-Curtis)
  # 3) based on both standardization approaches(on the basis of Bray-Curtis)

# for all three similarity indices
plot_all_1 <- ggplot()+
  
  # Bray-Curtis
  stat_smooth(data = dat, aes(times, sims_b), col = "#7C0902", span = 1, se = T, linewidth = 1, fill = "#7C0902", alpha = 1/2, method = "loess")+
  
  # Sorensen
  stat_smooth(data = dat, aes(times, sims_s), col = "#70474A", span = 1, se = T, linewidth = 1, fill = "#70474A", alpha = 1/2, method = "loess")+
  
  # Jaccard
  stat_smooth(data = dat, aes(times, sims_j), col = "#6a666e", span = 1, se = T, linewidth = 1, fill = "#6a666e", alpha = 1/2, method = "loess")+
  
  scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.21))+
  theme_pubr(base_size = 16)+
  annotate("text", x=5000, y=0.18, label= "Bray-Curtis", colour = "#7C0902", hjust = "left", size = 5)+ 
  annotate("text", x=5000, y=0.16, label= "Sørensen", colour = "#70474A", hjust = "left", size = 5)+ 
  annotate("text", x=5000, y=0.14, label= "Jaccard", colour = "#6a666e", hjust = "left", size = 5)

plot_all_1

ggsave(
  "figure_SI_5a.png",
  plot = plot_all_1,
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


# with and without grasses
plot_all_2 <- ggplot()+
  stat_smooth(data = dat, aes(times, sims_b), col = "#7C0902", span = 1, se = T, linewidth = 1, fill = "#7C0902", alpha = 1/2, method = "loess")+
  stat_smooth(data = dat_NG, aes(times, sims_b), col = "#6a666e", span = 1, se = T, linewidth = 1, fill = "#6a666e", alpha = 1/2, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.21))+
  theme_pubr(base_size = 16)+
  annotate("text", x=5000, y=0.18, label= "With grasses", colour = "#7C0902", hjust = "left", size = 5)+ 
  annotate("text", x=5000, y=0.16, label= "No grasses", colour = "#6a666e", hjust = "left", size = 5)

plot_all_2

ggsave(
  "figure_SI_5b.png",
  plot = plot_all_2,
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


# both standardization approaches
plot_all_3 <- ggplot()+
  stat_smooth(data = dat, aes(times, sims_b), col = "#7C0902", span = 1, se = T, linewidth = 1, fill = "#7C0902", alpha = 1/2, method = "loess")+
  stat_smooth(data = dat_con, aes(times, sims_b), col = "#6a666e", span = 1, se = T, linewidth = 1, fill = "#6a666e", alpha = 1/2, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.21))+
  theme_pubr(base_size = 16)+
  annotate("text", x=5000, y=0.18, label= "Standardization 1 (bold)", colour = "#7C0902", hjust = "left", size = 5)+ 
  annotate("text", x=5000, y=0.16, label= "Standardization 2 (conservative)", colour = "#6a666e", hjust = "left", size = 5)

plot_all_3

ggsave(
  "figure_SI_5c.png",
  plot = plot_all_3,
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





### Figure 2b ----
# figure oceanic basins
plot_oceans <- ggplot(dat_global[which(dat_global$ocean != "Indian ocean"),], aes(times, sims_b, color = factor(ocean), fill = factor(ocean)))+
  stat_smooth(se = T, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,2500,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.25))+
  scale_color_manual(values = c("Atlantic" = "#FFC100",
                                "Pacific"="#32746D",
                                "Mediterranean"="#CC998D"), name = "Ocean basin")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D"), name = "Ocean basin")+
  theme_pubr(base_size = 16)+
  theme(legend.position = "none")
  
plot_oceans


ggsave(
  "figure2b.png",
  plot = plot_oceans,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# get % change values from model
plot_oceans_ggplot <- ggplot_build(plot_oceans)$data[[1]]

plot_oceans_ggplot_A <- plot_oceans_ggplot[which(plot_oceans_ggplot$group == "1"),]
round(plot_oceans_ggplot_A$y[1], digits = 3)
round(plot_oceans_ggplot_A$y[80], digits = 3)
# 12% decrease in Atlantic

plot_oceans_ggplot_M <- plot_oceans_ggplot[which(plot_oceans_ggplot$group == "2"),]
round(plot_oceans_ggplot_M$y[1], digits = 3)
round(plot_oceans_ggplot_M$y[80], digits = 3)
# 33% decrease in Mediterranean

plot_oceans_ggplot_P <- plot_oceans_ggplot[which(plot_oceans_ggplot$group == "3"),]
round(plot_oceans_ggplot_P$y[1], digits = 3)
round(plot_oceans_ggplot_P$y[80], digits = 3)
# 184% increase in Pacific






# redone as sensitivity analysis with (plot SI Figure 5)
# 1) for all three beta diversity indices,
# 2) with and without grasses (on the basis of Bray-Curtis)
# 3) based on both standardization approaches(on the basis of Bray-Curtis)


# for all three similarity indices
plot_oceans_1 <- ggplot(dat_global[which(dat_global$ocean != "Indian ocean"),])+
  stat_smooth(aes(times, sims_b, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  stat_smooth(aes(times, sims_s, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  stat_smooth(aes(times, sims_j, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.45))+
  scale_color_manual(values = c("Atlantic" = "#FFC100",
                                "Pacific"="#32746D",
                                "Mediterranean"="#CC998D"), name = "Ocean basin")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D"), name = "Ocean basin")+
  theme_pubr(base_size = 16)+
  theme(legend.position = "none")

plot_oceans_1


ggsave(
  "figure_SI_6a.png",
  plot = plot_oceans_1,
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

# with and without grasses
plot_oceans_2 <- ggplot()+
  stat_smooth(data = dat_global[which(dat_global$ocean != "Indian ocean"),], aes(times, sims_b, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  stat_smooth(data = dat_global_NG[which(dat_global_NG$ocean != "Indian ocean"),], aes(times, sims_b, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
    scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.25))+
  scale_color_manual(values = c("Atlantic" = "#FFC100",
                                "Pacific"="#32746D",
                                "Mediterranean"="#CC998D"), name = "Ocean basin")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D"), name = "Ocean basin")+
  theme_pubr(base_size = 16)+
  theme(legend.position = "none")

plot_oceans_2


ggsave(
  "figure_SI_6b.png",
  plot = plot_oceans_2,
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

# both standardization approaches
plot_oceans_3 <- ggplot()+
  stat_smooth(data = dat_global[which(dat_global$ocean != "Indian ocean"),], aes(times, sims_b, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  stat_smooth(data = dat_global_con[which(dat_global_con$ocean != "Indian ocean"),], aes(times, sims_b, color = factor(ocean), fill = factor(ocean)), se = T, method = "loess")+
  scale_x_reverse(c(5000, -70), breaks = c(5000,0), name = "Time [cal. yrs BP]")+
  scale_y_continuous(breaks = c(0,0.1,0.2), name = "Similarity") +
  coord_cartesian(ylim = c(0,0.25))+
  scale_color_manual(values = c("Atlantic" = "#FFC100",
                                "Pacific"="#32746D",
                                "Mediterranean"="#CC998D"), name = "Ocean basin")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D"), name = "Ocean basin")+
  theme_pubr(base_size = 16)+
  theme(legend.position = "none")

plot_oceans_3


ggsave(
  "figure_SI_6c.png",
  plot = plot_oceans_3,
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





### Figure 2c ----
# figure island group per latitude

# sort by latitude
island_group_stats <- island_group_stats[order(island_group_stats$mean_lat),]


plot_lat <- ggplot(aes(x = mean_lat, y = simb_b_diff, fill = ocean), dat = island_group_stats)+
  geom_point(show.legend = FALSE,colour = "black", fill = "black", size = 3)+
  geom_hline(yintercept = 0)+
  geom_label_repel(aes(label = island_group), colour = c("white", "black", "white","white", "black", "black","white", "white", "black","black", "black", "black"),
                   size = 5,
                   box.padding = 0.5,
                   point.padding = 0.5)+
  theme_pubr(base_size = 16)+
  ylab("Temporal similarity difference")+
  xlab("Latitude (°)")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D",
                               "Indian" = "grey"),
                    labels = c("Atlantic" = "black",
                               "Pacific"= "white",
                               "Mediterranean" = "black",
                               "Indian" = "black"),
                    name = "Ocean basin",
                    guide = "none")
 

plot_lat

ggsave(
  "figure2c.png",
  plot = plot_lat,
  # device = NULL,
  path = "./Figures/",
  # scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  # dpi = 300,
  # limitsize = TRUE,
  # bg = NULL
)


# redo with other similarity indices for SI
plot_lat_1 <- ggplot(aes(x = mean_lat, y = simb_b_diff, fill = ocean), dat = island_group_stats)+
  geom_point(show.legend = FALSE, colour = "black", fill = "black", size = 3)+
  geom_hline(yintercept = 0)+
  geom_label_repel(aes(label = island_group),
                   colour = c("white", "black", "white","white", "black", "black","white", "white", "black","black", "black", "black"),
                   size = 5,
                   box.padding = 0.5,
                   point.padding = 0.5)+
  theme_pubr(base_size = 16)+
  ylab("Temporal similarity difference")+
  xlab("Latitude (°)")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D",
                               "Indian" = "lightgrey"),
                    labels = c("Atlantic" = "black",
                               "Pacific"= "white",
                               "Mediterranean" = "black",
                               "Indian" = "black"),
                    name = "Ocean basin",
                    guide = "none")+
  ggtitle("Bray-Curtis")


plot_lat_1

ggsave(
  "figure_SI_7a.png",
  plot = plot_lat_1,
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

plot_lat_2 <- ggplot(aes(x = mean_lat, y = simb_s_diff, fill = ocean), dat = island_group_stats)+
  geom_point(show.legend = FALSE,colour = "black", fill = "black", size = 3)+
  geom_hline(yintercept = 0)+
  geom_label_repel(aes(label = island_group),
                   colour = c("white", "black", "white","white", "black", "black","white", "white", "black","black", "black", "black"),
                   size = 5,
                   box.padding = 0.5,
                   point.padding = 0.5)+
  theme_pubr(base_size = 16)+
  ylab("Temporal similarity difference")+
  xlab("Latitude (°)")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D",
                               "Indian" = "grey"),
                    labels = c("Atlantic" = "black",
                               "Pacific"= "white",
                               "Mediterranean" = "black",
                               "Indian" = "black"),
                    name = "Ocean basin",
                    guide = "none")+
  ggtitle("Sørensen")


plot_lat_2

ggsave(
  "figure_SI_7b.png",
  plot = plot_lat_2,
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

plot_lat_3 <- ggplot(aes(x = mean_lat, y = simb_s_diff, fill = ocean), dat = island_group_stats)+
  geom_point(show.legend = FALSE,colour = "black", fill = "black", size = 3)+
  geom_hline(yintercept = 0)+
  geom_label_repel(aes(label = island_group),
                   colour = c("white", "black", "white","white", "black", "black","white", "white", "black","black", "black", "black"),
                   size = 5,
                   box.padding = 0.5,
                   point.padding = 0.5)+
  theme_pubr(base_size = 16)+
  ylab("Temporal similarity difference")+
  xlab("Latitude (°)")+
  scale_fill_manual(values = c("Atlantic" = "#FFC100",
                               "Pacific"="#32746D",
                               "Mediterranean"="#CC998D",
                               "Indian" = "grey"),
                    labels = c("Atlantic" = "black",
                               "Pacific"= "white",
                               "Mediterranean" = "black",
                               "Indian" = "black"),
                    name = "Ocean basin",
                    guide = "none")+
  ggtitle("Jaccard")


plot_lat_3

ggsave(
  "figure_SI_7c.png",
  plot = plot_lat_3,
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






### Figure 2c alternative ----
# figure biogeographic region per latitude

biogeo_stats <- data.frame(stats = c("3 islands, n = 56", # Balearic Islands
                                     "7 islands, n = 318", # Caribbean
                                     "2 islands, n = 10", # Galápagos
                                     "10 islands, n = 1572", # Macaronesia
                                     "5 islands, n = 158", # Melanesia
                                     "2 islands, n = 10", # New Zealand Region
                                     "3 islands, n = 10", # North Atlantic
                                     "12 islands, n = 542", # Polynesia
                                     "4 islands, n = 92", # South Atlantic
                                     "2 islands, n = 12", # Southeast Pacific
                                     "2 islands, n = 22", # Tyrrhesnain Islands
                                     "2 islands, n = 8"), # West Indian Ocean
                           zuordnung_biogeo = c("Balearic Islands", "Caribbean", "Galápagos", "Macaronesia",
                                                "Melanesia", "New Zealand Region", "North Atlantic", "Polynesia",
                                                "South Atlantic", "Southeast Pacific", "Tyrrhenian Islands", "Western Indian Ocean"))

plot_geo_reg <- ggplot()+
  geom_point(aes(times, sims_b, col = zuordnung_biogeo), data = dat_biogeo, show.legend = F, alpha = 1/10, size = 2)+
  stat_smooth(aes(times, sims_b, col = zuordnung_biogeo, fill = zuordnung_biogeo), data = dat_biogeo, se = T, show.legend = F, method = "loess")+
  facet_wrap(~zuordnung_biogeo)+
  scale_x_reverse(breaks = c(5000,2500,0))+
  xlab("Time Cal. yrs BP")+
  ylab("Similarity")+
  theme(panel.background = element_rect(fill = "NA", colour = "black"))+
  # theme(axis.text = element_text(size = 12))+
  # theme(axis.title = element_text(size = 12))+
  scale_color_manual(values = c("#CC998D", "#FFC100", "#32746D", "#FFC100", "#32746D", "#32746D", "#FFC100",  "#32746D","#FFC100","#32746D", "#CC998D", "#6a666e"))+
  scale_fill_manual(values = c("#CC998D", "#FFC100", "#32746D", "#FFC100", "#32746D", "#32746D", "#FFC100",  "#32746D","#FFC100","#32746D", "#CC998D", "#6a666e"))+
  theme_pubr(base_size = 12)+
  geom_text(data = biogeo_stats,
            aes(x = c(rep(2500, 12)), y = c(rep(0.7, 12)), label = stats), size = 3, colour = "darkgrey")

plot_geo_reg

table(dat_biogeo$zuordnung_biogeo)
table(readme$biogeo_unit)

ggsave(
  "Figure_SI_9.png",
  plot = plot_geo_reg,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 2000,
  height = 1700,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)


## Figures single islands ----

# Plot how a single island is different to all other islands
dat$times <- as.character(dat$times)
dat$times <- as.numeric(dat$times)

list_fig_single_isl <- list()

readme$start_sim_b <- NA
readme$end_sim_b <- NA

# plot can only be created for those islands that have more than one time bin

for(i in names(list_all)){
  # if(length(unique(dat$times[grep(paste0(i), dat$name)])) > 1){
  
  # create basic gglpot
  list_fig_single_isl[[i]] <- ggplot()+
    geom_point(aes(times, sims_b),
               data = dat[grep(paste0(i), dat$name),],
               alpha = 1/2, colour = "darkred", shape = 16)+
    stat_smooth(col = "darkred", fill = "darkred", aes(times, sims_b),
                data = dat[grep(paste0(i), dat$name),], se = T)+
           theme_set(theme_minimal())+
    scale_x_reverse(limits = c(5000, -75),
                    breaks = c(5000, 2500, 0))+

    ylim(0,1)
  
    readme$start_sim_b[which(readme$site == i)] <- round(ggplot_build(list_fig_single_isl[[i]])$data[[2]]$y[1], digits = 3)
    readme$end_sim_b[which(readme$site == i)] <- round(ggplot_build(list_fig_single_isl[[i]])$data[[2]]$y[nrow(ggplot_build(list_fig_single_isl[[i]])$data[[2]])], digits = 3)
    
    if(readme$start_sim_b[which(readme$site == i)] < 
       readme$end_sim_b[which(readme$site == i)]){  
      
    list_fig_single_isl[[i]] <- ggplot()+
      stat_smooth(col = "darkgrey", fill = "darkgrey", aes(times, sims_b),
                  data = dat_NG[grep(paste0(i), dat_NG$name),], se = T)+
      geom_point(aes(times, sims_b),
                 data = dat[grep(paste0(i), dat$name),],
                 alpha = 1/2, colour = "#661F1C", shape = 16)+
      stat_smooth(col = "#661F1C", fill = "#661F1C", aes(times, sims_b),
                  data = dat[grep(paste0(i), dat$name),], se = T)+
      scale_x_reverse(limits = c(5000, -75),
                      breaks = c(5000, 2500, 0))+
      ggtitle(paste0(readme$island[which(readme$site == i)]))+
      theme(plot.title = element_text(size = 15))+
      theme(plot.title = element_text(face = "bold"))+
      xlab("Time Cal. years BP")+
      ylab("Similarity")+
      ylim(0,1)+
      theme_pubclean()
    }else{
      list_fig_single_isl[[i]] <- ggplot()+
        stat_smooth(col = "darkgrey", fill = "darkgrey", aes(times, sims_b),
                    data = dat_NG[grep(paste0(i), dat_NG$name),], se = T)+
        geom_point(aes(times, sims_b),
                   data = dat[grep(paste0(i), dat$name),],
                   alpha = 1/2, colour = "#3E6274", shape = 16)+
        stat_smooth(col = "#3E6274", fill = "#3E6274", aes(times, sims_b),
                    data = dat[grep(paste0(i), dat$name),], se = T)+
        scale_x_reverse(limits = c(5000, -75),
                        breaks = c(5000, 2500, 0))+
        ggtitle(paste0(readme$island[which(readme$site == i)]))+
        theme(plot.title = element_text(size = 15))+
        theme(plot.title = element_text(face = "bold"))+
        xlab("Time Cal. years BP")+
        ylab("Similarity")+
        ylim(0,1)+
        theme_pubclean()
    }
    
    if(readme$settlement_BP[which(readme$site == i)] != ">5000"){
      list_fig_single_isl[[i]] <- list_fig_single_isl[[i]]+
        geom_vline(xintercept = as.numeric(readme$settlement_BP[which(readme$site == i)]), size=1, linetype = "F1", colour = "black")
    }
  # }
}

readme$perc_sim_b <- round((readme$end_sim_b-readme$start_sim_b)/readme$start_sim_b*100, digits = 0)


# figure with multiple plots

# all plots from atlantic (save manually, 1000 x 1000)
grid.arrange(grobs = list_fig_single_isl[which(names(list_fig_single_isl) %in% c(readme$site[which(readme$ocean == "atlantic")]))], ncol = 5)

# all plots from pacific (save manually, 1000 x 1000)
grid.arrange(grobs = list_fig_single_isl[which(names(list_fig_single_isl) %in% c(readme$site[which(readme$ocean == "pacific")]))], ncol = 5)

# all plots from mediterranean (save manually, 1000 x 400)
grid.arrange(grobs = list_fig_single_isl[which(names(list_fig_single_isl) %in% c(readme$site[which(readme$ocean == "mediterranean")]))], ncol = 5)

# all plots from indian ocean (save manually, 1000 x 200)
grid.arrange(grobs = list_fig_single_isl[which(names(list_fig_single_isl) %in% c(readme$site[which(readme$ocean == "indian")]))], ncol = 5)





## Figure environmental driver ----

### settlement dates ----

# Air force blue: #638591
# Barn red: #7C0902
# potential colours in between: #677680, #6a666e, #6D575C, #70474A, #762826



dat$diff_settlement <- factor(dat$diff_settlement,
                              levels = c("both", "one", "none"), ordered = TRUE)

b_sett <- ggplot()+
  geom_jitter(aes(diff_settlement, sims_b, color = diff_settlement),
              data = dat_global[which(!is.na(dat_global$diff_settlement)),],
              shape=16, position=position_jitter(0.2), size = 0.5)+
  geom_boxplot(aes(diff_settlement, sims_b, fill = diff_settlement),
               data = dat_global[which(!is.na(dat_global$diff_settlement)),], alpha = 0.5)+
  xlab("Difference settlement")+
  ylab("Similarity")+
  theme(panel.background = element_rect(fill = "NA", colour = "black"), legend.position = "none")+
  scale_color_manual(values = c("both" = "#7C0902",
                                "one"="#6D575C",
                                "none"="darkgrey"))+
  scale_fill_manual(values = c("both" = "#7C0902",
                               "none"="#6D575C",
                               "one"="darkgrey"))+
  theme_pubclean()
  

b_sett

ggsave(
  "figure3a.png",
  plot = b_sett,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

kruskal.test(dat_global$sims_b, dat_global$diff_settlement)
kruskal.test(dat_global$sims_j, dat_global$diff_settlement)
kruskal.test(dat_global$sims_s, dat_global$diff_settlement)

dunn_test(sims_b~diff_settlement, data=dat_global, p.adjust.method = "bonferroni")
dunn_test(sims_j~diff_settlement, data=dat_global, p.adjust.method = "bonferroni")
dunn_test(sims_s~diff_settlement, data=dat_global, p.adjust.method = "bonferroni")







### gdms ----

# plot mean gdm
plot_gdm_mean_1 <- ggplot(aes(x = Geographic, y = Geographic_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("Distance (km)")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)

plot_gdm_mean_1

ggsave(
  "figure3b.png",
  plot = plot_gdm_mean_1,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

plot_gdm_mean_2 <- ggplot(aes(x = area, y = area_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("Area (km²)")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)+
  scale_x_continuous(breaks = c(0, 40000, 80000))

plot_gdm_mean_2

ggsave(
  "figure3c.png",
  plot = plot_gdm_mean_2,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

plot_gdm_mean_3 <- ggplot(aes(x = isolation, y = isolation_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("Isolation (km)")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)+
  scale_x_continuous(breaks = c(0, 2000, 4000))

plot_gdm_mean_3

ggsave(
  "figure3d.png",
  plot = plot_gdm_mean_3,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

plot_gdm_mean_4 <- ggplot(aes(x = tri, y = tri_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("TRI")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)

plot_gdm_mean_4

ggsave(
  "figure3e.png",
  plot = plot_gdm_mean_4,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

plot_gdm_mean_5 <- ggplot(aes(x = temperature, y = temperature_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("Temperature (°C)")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)

plot_gdm_mean_5

ggsave(
  "figure3f.png",
  plot = plot_gdm_mean_5,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

plot_gdm_mean_6 <- ggplot(aes(x = precipitation, y = precipitation_dist), data = splines_df_mean)+
  geom_line(linewidth = 1)+
  theme_pubr()+
  xlab("Precipitation (mm)")+
  ylab("Dissimilarity")+
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  ylim(0,1.25)

plot_gdm_mean_6

ggsave(
  "figure3g.png",
  plot = plot_gdm_mean_6,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 800,
  height = 800,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

gdm_mean <- ggarrange(b_sett, "","", plot_gdm_mean_4, plot_gdm_mean_1,
                      plot_gdm_mean_5, plot_gdm_mean_6, plot_gdm_mean_3,
                      plot_gdm_mean_2, 
                      labels = c("A", "", "", "B", "C", "D", "E", "F", "G", "H"),
                      ncol = 3, nrow = 3,
                      hjust = -0.5,
                      vjust = 1.5,
                      font.label = list(size = 14, face = "bold", color ="black"),
                      align = "hv")

gdm_mean

ggsave(
  "gdm_mean.png",
  plot = gdm_mean,
  device = NULL,
  path = "./Figures/",
  scale = 1,
  width = 2000,
  height = 2000,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)
