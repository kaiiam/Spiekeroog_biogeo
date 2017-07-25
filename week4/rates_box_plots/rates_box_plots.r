library("ggplot2")
library("dplyr")
library("tidyverse")

#http://ggplot2.tidyverse.org/reference/facet_grid.html
#http://ggplot2.tidyverse.org/reference/geom_boxplot.html

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/rates_box_plots")

data <- read.csv(file = "final_rates_data.csv",header = TRUE)

data <- mutate(data, oxygen_consumption_rate_uM_per_h_corrected = abs(oxygen_consumption_rate_uM_per_h_corrected ))

# plot <- data %>% 
#         ggplot(aes(depth_factor, oxygen_consumption_rate_uM_per_h_corrected)) +
#         geom_boxplot(aes(colour = Month)) +
#         geom_jitter(width = 0.2) +
#         ylab(expression(paste("Oxygen Consumption Rate (μmol l"^"-1" * "h"^"-1"* ")" ))) +
#         xlab("Depth (cm)")
# 
# 
# plot

winter_summer_plot <- data %>% 
  ggplot(aes(month, oxygen_consumption_rate_uM_per_h_corrected)) +
  theme_linedraw() +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  ylab(expression(paste("Respiration (μmol l"^"-1" * "h"^"-1"* ")" ))) +
  xlab("Season") +
  facet_grid(. ~ Month , scales = "free", space = "free")

winter_summer_plot

# summer_plot <- filter(data, Month == "Summer") %>% 
#   ggplot(aes(depth_factor, oxygen_consumption_rate_uM_per_h_corrected)) +
#   theme_linedraw() +
#   geom_boxplot() +
#   geom_jitter(width = 0.2) +
#   ylab(expression(paste("Oxygen Consumption Rate (μmol l"^"-1" * "h"^"-1"* ")" ))) +
#   xlab("Depth (cm)") +
#   labs(subtitle = "Summer") +
#   theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"))
# 
# 
# summer_plot
# 
# winter_plot <- filter(data, Month == "Winter") %>% 
#   ggplot(aes(depth_factor, oxygen_consumption_rate_uM_per_h_corrected)) +
#   theme_linedraw() +
#   geom_boxplot() +
#   geom_jitter(width = 0.2) +
#   ylab(expression(paste("Oxygen Consumption Rate (μmol l"^"-1" * "h"^"-1"* ")" ))) +
#   xlab("Depth (cm)") +
#   labs(subtitle = "Winter") +
#   theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"))
# 
# winter_plot

depths_plot <- data %>% 
  ggplot(aes(depth_factor, oxygen_consumption_rate_uM_per_h_corrected)) +
  theme_linedraw() +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  ylab(expression(paste("Respiration (μmol l"^"-1" * "h"^"-1"* ")" ))) +
  xlab("Depth (cm)") +
  facet_grid(. ~ Month , scales = "free", space = "free")

depths_plot


ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/rates_box_plots/summer_winter_comparison.jpeg", 
       plot =  winter_summer_plot, width = 6, height = 4)

ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/rates_box_plots/depth_comparison.jpeg", 
       plot =  depths_plot, width = 6, height = 4)


