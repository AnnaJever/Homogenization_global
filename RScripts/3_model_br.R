# Beta regressions ####

# Build beta regression to explain how environmental variables affect insular floristic similarity




# packages ----
library(tidyverse)
library(ggstats)
library(betareg)
library(parameters)
library(car)
library(memisc)





# get data ----
load("./Data/R_products/similarity_islands.RData")




# correlations  ----

# convert settlements with the value ">5000" to "5000"
str(readme)
readme$settlement_BP[which(readme$settlement_BP == ">5000")] <- 500
readme$settlement_BP <- as.numeric(readme$settlement_BP)
round(cor(readme[,c("Island_size", "avg_temp", "sum_prec", "elev", "tri", "isolation", "settlement_BP")], use = "complete.obs"), digits = 1)

# test correlations between predictor variables
png("./Figures/correlation_plot.png",
    width = 300,
    height = 300,
    res = NA,
    units = "px",
    bg = "white")
corrplot::corrplot(cor(readme[,c("Island_size", "avg_temp", "sum_prec", "elev", "tri", "isolation", "settlement_BP")],
                       use = "complete.obs"),
                   method = "square",
                   order = "alphabet",
                   diag = FALSE,
                   type = "upper")
dev.off()
# only correlation between tri and elevation is concerning (R = 0.7). Tri was selected.





# beta regression ----

## adjust data ----
dat_bet <- dat

# betar regression is sensitive to zeros. Therefore, add a very small amount to zeroes
hist(dat_bet$sims_b)
dat_bet$sims_b <- dat_bet$sims_b + 0.00001
hist(dat_bet$sims_b)

colnames(dat_bet) <- c("times", "sims_b", "sims_j", "sims_s", "name", "site1", "site2", "Δ Distance", "Δ Area", "Δ TRI", "Δ Isolation", "Δ Elevation", "Δ Temperature", "Δ Precipitation", "Settlement status")

## beta reg 1 ----
# all samples
br_sim_1 <- betareg(sims_b ~ `Δ Area` + `Δ Isolation` + `Δ Distance` + `Δ TRI` + `Δ Isolation` + `Δ Temperature` + `Δ Precipitation` + `Settlement status`,
                  data = dat_bet,
                  link = "logit")

# review model
summary(br_sim_1)

plot(br_sim_1)

parameters(br_sim_1)

br_sim_1 |> 
  Anova() |> 
  model_parameters()

# plot beta reg 1
br_plot_1 <- ggcoef_model(br_sim_1, colour = NULL, stripped_rows = FALSE, facet_row = NULL)+
  ggplot2::xlab("Coefficients") +
  ggplot2::ggtitle("") 

br_plot_1

ggsave(
  "figure_SI_8a.png",
  plot = br_plot_1,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 2000,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# table model results
br_sim_1_tab <- mtable(br_sim_1)
br_sim_1_tab

write.mtable(br_sim_1_tab, file="./Tables/br_sim_1_modeloutput.html.html", format = "HTML")




## beta reg 2 ----
# reduce to sample with settlement none vs both
br_sim_2 <- betareg(sims_b ~ `Δ Area` + `Δ Isolation` + `Δ Distance` + `Δ Area` + `Δ TRI` + `Δ Isolation` + `Δ Temperature` + `Δ Precipitation` + `Settlement status`,
                  data = dat_bet[which(dat_bet$`Settlement status` %in% c("none", "both")),],
                  lnk = "logit")

# review model
summary(br_sim_2)

plot(br_sim_2)

parameters(br_sim_2)

br_sim_2 |> 
  Anova() |> 
  model_parameters()

# plot beta reg 1
br_plot_2 <- ggcoef_model(br_sim_2, colour = NULL, stripped_rows = FALSE, facet_row = NULL)+
  ggplot2::xlab("Coefficients") +
  ggplot2::ggtitle("") 

br_plot_2

ggsave(
  "figure_SI_8b.png",
  plot = br_plot_2,
  device = NULL,
  path = "./Figures/Figures_SI/",
  scale = 1,
  width = 2000,
  height = 1500,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

# 550 x 350

# table model results
br_sim_2_tab <- mtable(br_sim_2)
br_sim_2_tab

write.mtable(br_sim_2_tab, file="./Tables/br_sim_2_modeloutput.html.html", format = "HTML")
