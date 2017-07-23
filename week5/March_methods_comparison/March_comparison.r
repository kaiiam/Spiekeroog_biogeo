library(ggplot2)
library(dplyr)

#from https://stackoverflow.com/questions/12018499/how-to-put-labels-over-geom-bar-for-each-bar-in-r-with-ggplot2

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/March_methods_comparison")

march_data <- read.csv(file = "march_comparsion.csv",header = TRUE)

march_plot <- ggplot(march_data, aes(site, O2_rate_µM_h, fill = Method)) + 
              geom_bar(stat="identity", position = "dodge") + 
              theme_linedraw() +
              scale_fill_brewer(palette = "Set1") +
              ylab(expression(paste("Oxygen Consumption Rate (μmol l"^"-1" * "h"^"-1"* ")" ))) +
              geom_text(aes(label = O2_rate_µM_h),
              position = position_dodge(width = 0.9), vjust=-0.2) 

march_plot

ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/March_methods_comparison/march_methods_comparison.jpeg", 
       plot =  march_plot, width = 6, height = 4)
