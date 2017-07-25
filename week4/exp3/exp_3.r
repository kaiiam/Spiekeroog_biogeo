library(dplyr)
library(ggplot2)

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp3")

mydata = read.csv("exp_3_data.csv")

#add column for time in hours
mydata <- mutate(mydata, time.hours = time.minutes / 60)

# data for plots:
beach_above_mwl <- filter(mydata, station==2)
lagoon          <- filter(mydata, station %in% c(3, 143))
berm            <- filter(mydata, station %in% c(4, 164))
low_water_line  <- filter(mydata, station %in% c(5, 169))

# change depth factor levels to be in order
beach_above_mwl <- mutate(beach_above_mwl, depth = factor(depth , levels = c('0cm', '30cm', '50cm', '100cm')))
lagoon <- mutate(lagoon, depth = factor(depth , levels = c('0cm', '30cm', '50cm', '100cm')))
berm <- mutate(berm, depth = factor(depth , levels = c('0cm', '30cm', '50cm', '100cm')))
low_water_line <- mutate(low_water_line, depth = factor(depth , levels = c('0cm', '30cm', '50cm', '100cm')))

#exclude values which are less than 10μM in the regressions
beach_above_mwl_reg <- filter(beach_above_mwl, oxygen > 10, time.minutes >= 135 )
lagoon_reg <- filter(lagoon, oxygen > 10, time.minutes >= 135)
berm_reg <- filter(berm, oxygen > 10, time.minutes >= 137)
low_water_line_reg <- filter(low_water_line, oxygen > 10,
                             
                             depth=="0cm" & time.minutes >= 172 |
                             depth=="30cm" & time.minutes >= 207 |
                             depth=="50cm" & time.minutes >= 140 |
                             depth=="100cm" & time.minutes >= 64
                             )

#plots
plot_beach_above_mwl<- beach_above_mwl %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(data=beach_above_mwl_reg, se = FALSE, method = "lm") +
  ggtitle("Beach above Mean Water Line") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_lagoon<- lagoon %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(data= lagoon_reg, se = FALSE, method = "lm") +
  ggtitle("Lagoon") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_berm<- berm %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(data=berm_reg,se = FALSE, method = "lm") +
  ggtitle("Berm") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_low_water_line<- low_water_line %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(data= low_water_line_reg,se = FALSE, method = "lm") +
  ggtitle("Low Water Line") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

#view plots
plot(plot_beach_above_mwl)
plot(plot_lagoon)
plot(plot_berm)
plot(plot_low_water_line)

#save plots
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp3/rate_plots/beach_above_mwl.jpeg", 
       plot =  plot_beach_above_mwl, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp3/rate_plots/lagoon.jpeg", 
       plot =  plot_lagoon, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp3/rate_plots/berm.jpeg", 
       plot =  plot_berm, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp3/rate_plots/low_water_line.jpeg", 
       plot =  plot_low_water_line, width = 6, height = 4)


######## multiplot:
#library(grid)
multiplot <- function(..., plotlist=NULL, file, cols=2, layout= matrix(c(1,2,3,4,0,0), nrow=3, byrow=TRUE)) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

mult_plot <- multiplot(plot_beach_above_mwl,plot_lagoon,plot_berm,plot_low_water_line, cols=2)
#export pdf using cairo-pdf


#################### rate calculations: ################################

#split up dataframe into the individual lines
beach_above_mwl_0 <- filter(beach_above_mwl_reg, station==2, oxygen > 10)

lagoon_0 <- filter(lagoon_reg, station==3 & depth=="0cm", oxygen > 10)
lagoon_30 <- filter(lagoon_reg, station==143 & depth=="30cm", oxygen > 10)
lagoon_50 <- filter(lagoon_reg, station==143 & depth=="50cm", oxygen > 10)
lagoon_100 <- filter(lagoon_reg, station==143 & depth=="100cm", oxygen > 10)

berm_0 <- filter(berm_reg, station==4 & depth=="0cm", oxygen > 10)
berm_30 <- filter(berm_reg, station==164 & depth=="30cm", oxygen > 10)
berm_50 <- filter(berm_reg, station==164 & depth=="50cm", oxygen > 10)
berm_100 <- filter(berm_reg, station==164 & depth=="100cm", oxygen > 10)

low_water_line_0 <- filter(low_water_line_reg, station==5 & depth=="0cm", oxygen > 10)
low_water_line_30 <- filter(low_water_line_reg, station==169 & depth=="30cm", oxygen > 10)
low_water_line_50 <- filter(low_water_line_reg, station==169 & depth=="50cm", oxygen > 10)
low_water_line_100 <- filter(low_water_line_reg, station==169 & depth=="100cm", oxygen > 10)

#### linear regressions ####
beach_above_mwl_0_reg <-lm(oxygen~time.hours, data=beach_above_mwl_0)

lagoon_0_reg <-lm(oxygen~time.hours, data=lagoon_0)
lagoon_30_reg <-lm(oxygen~time.hours, data=lagoon_30)
lagoon_50_reg <-lm(oxygen~time.hours, data=lagoon_50)
lagoon_100_reg <-lm(oxygen~time.hours, data=lagoon_100)

berm_0_reg <-lm(oxygen~time.hours, data=berm_0)
berm_30_reg <-lm(oxygen~time.hours, data=berm_30)
berm_50_reg <-lm(oxygen~time.hours, data=berm_50)
berm_100_reg <-lm(oxygen~time.hours, data=berm_100)

low_water_line_0_reg <-lm(oxygen~time.hours, data=low_water_line_0)
low_water_line_30_reg <-lm(oxygen~time.hours, data=low_water_line_30)
low_water_line_50_reg <-lm(oxygen~time.hours, data=low_water_line_50)
low_water_line_100_reg <-lm(oxygen~time.hours, data=low_water_line_100)


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

# extract the sample names from the large dataframe
samples <- mydata %>% distinct(station, depth)

# make the table of rates
rates_table <- cbind(samples, regression_values)

rates_table <- rename(rates_table, oxygen_consumption_rate_uM_per_h = time.hours)

#write out to csv *don't run again I manually added the labels to the csv
#write.csv(rates_table, file = "March_Spiekeroog_rates_2.csv", row.names = FALSE)

# # different attempt to multiplot
# #install.packages("sjPlot")
# #from https://rdrr.io/cran/sjPlot/man/plot_grid.html 
# library(sjPlot)
# 
# x= c(plot_beach_above_mwl, plot_lagoon, plot_berm, plot_low_water_line)
# 
# sjPlot::plot_grid(x)

