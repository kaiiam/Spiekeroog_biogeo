library(dplyr)
library(ggplot2)

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/July_methods_comparison")

cont_stat = read.csv("cont_stat.csv")
cont_stat_upside_down = read.csv("cont_stat_upside_down.csv")
cont_wheel = read.csv("cont_wheel.csv")

#filter O2 less than 10uM
cont_stat <- filter(cont_stat, oxygen>10)
cont_stat_upside_down <- filter(cont_stat_upside_down, oxygen>10, time.hours >=2)
cont_wheel <- filter(cont_wheel, oxygen>10)

#plots
continuous_stationary <- cont_stat %>%
  ggplot(aes(time.hours, oxygen)) +
  geom_point( ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("continuous stationary") + theme(plot.title = element_text(hjust = 0.5))

plot(continuous_stationary)


continuous_stationary_upside_down <- cont_stat_upside_down %>%
  ggplot(aes(time.hours, oxygen)) +
  geom_point( ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("continuous stationary upside down") + theme(plot.title = element_text(hjust = 0.5))

plot(continuous_stationary_upside_down)

continuous_wheel <- cont_wheel %>%
  ggplot(aes(time.hours, oxygen)) +
  geom_point( ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("continuous wheel") + theme(plot.title = element_text(hjust = 0.5))

plot(continuous_wheel)

#regressions:
cont_stat_reg <-lm(oxygen~time.hours, data=cont_stat)
cont_stat_upside_down_reg <-lm(oxygen~time.hours, data=cont_stat_upside_down)
cont_wheel_reg <-lm(oxygen~time.hours, data=cont_wheel)

#cat together all the regression objects as a matrix
matrix_of_regressions <- cbind(
  beach_above_mwl_0_reg,
  lagoon_0_reg,
  lagoon_30_reg,
  lagoon_50_reg,
  lagoon_100_reg,
  berm_0_reg,
  berm_30_reg,
  berm_50_reg,
  berm_100_reg,
  low_water_line_0_reg,
  low_water_line_30_reg,
  low_water_line_50_reg,
  low_water_line_100_reg
)
#take the regression values out of the matrix of lm objects
regression_values <- 
  as.data.frame(t(as.data.frame(matrix_of_regressions[1,]))) %>% select(time.hours)

