library(dplyr)
library(ggplot2)

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data")

mydata = read.csv("exp_4_data.csv")

#add column for time in hours
mydata <- mutate(mydata, time.hours = time.minutes / 60)

#viewing the data
names(mydata)
head(select(mydata, time.minutes,depth))
head(mydata)
min(select(mydata, oxygen))

# data for plots:
station2 <- filter(mydata, station==2)
station4 <- filter(mydata, station==4)
station5 <- filter(mydata, station==5)
station6 <- filter(mydata, station==6)
station8 <- filter(mydata, station==8)

#plots
plot_station_2<- station2 %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("Station 2") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_station_4<- station4 %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("Station 4") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_station_5<- station5 %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("Station 5") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_station_6<- station6 %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("Station 6") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#ecd240", "#ff8811", "#f12700", "#972e23"))

plot_station_8<- station8 %>%
  ggplot(aes(time.hours, oxygen, colour = depth)) +
  geom_point(size=1, stroke=1.2 ) +
  theme_light() +
  scale_x_continuous("Time (h)") +
  scale_y_continuous(expression(paste("Oxygen (μmol l"^"-1" *")" )), limits = c(0, 250)) +
  geom_smooth(se = FALSE, method = "lm") +
  ggtitle("Station 8") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour ="Depth") + scale_colour_manual(values = c("#c0e105", "#ecd240", "#ff8811", "#f12700", "#972e23"))

#view plots
plot(plot_station_2)
plot(plot_station_4)
plot(plot_station_5)
plot(plot_station_6)
plot(plot_station_8)

ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data/rate_plots/2.jpeg", 
       plot =  plot_station_2, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data/rate_plots/4.jpeg", 
       plot =  plot_station_4, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data/rate_plots/5.jpeg", 
       plot =  plot_station_5, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data/rate_plots/6.jpeg", 
       plot =  plot_station_6, width = 6, height = 4)
ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week4/exp4/r_script_data/rate_plots/8.jpeg", 
       plot =  plot_station_8, width = 6, height = 4)

######## multiplot:
#library(grid)
multiplot <- function(..., plotlist=NULL, file, cols=2, layout= matrix(c(1,2,3,4,5,0), nrow=3, byrow=TRUE)) {
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

mult_plot <- multiplot(plot_station_2, plot_station_4, plot_station_5, plot_station_6, plot_station_8, cols=2)
#export pdf using cairo-pdf

#################### rate calculations: ################################

#split up dataframe into the individual lines
station2_10 <- filter(mydata, station==2 & depth=="10cm")
station2_30 <- filter(mydata, station==2 & depth=="30cm")
station2_50 <- filter(mydata, station==2 & depth=="50cm")
station2_70 <- filter(mydata, station==2 & depth=="70cm")

station4_10 <- filter(mydata, station==4 & depth=="10cm")
station4_30 <- filter(mydata, station==4 & depth=="30cm")
station4_50 <- filter(mydata, station==4 & depth=="50cm")
station4_60 <- filter(mydata, station==4 & depth=="60cm")

station5_10 <- filter(mydata, station==5 & depth=="10cm")
station5_30 <- filter(mydata, station==5 & depth=="30cm")
station5_50 <- filter(mydata, station==5 & depth=="50cm")
station5_70 <- filter(mydata, station==5 & depth=="70cm")

station6_10 <- filter(mydata, station==6 & depth=="10cm")
station6_30 <- filter(mydata, station==6 & depth=="30cm")
station6_50 <- filter(mydata, station==6 & depth=="50cm")
station6_80 <- filter(mydata, station==6 & depth=="80cm")

station8_sur <- filter(mydata, station==8 & depth=="0cm")
station8_10 <- filter(mydata, station==8 & depth=="10cm")
station8_30 <- filter(mydata, station==8 & depth=="30cm")
station8_50 <- filter(mydata, station==8 & depth=="50cm")
station8_80 <- filter(mydata, station==8 & depth=="80cm")

#### linear regressions ####
station2_10_reg <- lm(oxygen ~ time.hours, data=station2_10)
station2_30_reg <- lm(oxygen ~ time.hours, data=station2_30)
station2_50_reg <- lm(oxygen ~ time.hours, data=station2_50)
station2_70_reg <- lm(oxygen ~ time.hours, data=station2_70)

station4_10_reg <- lm(oxygen ~ time.hours, data=station4_10)
station4_30_reg <- lm(oxygen ~ time.hours, data=station4_30)
station4_50_reg <- lm(oxygen ~ time.hours, data=station4_50)
station4_60_reg <- lm(oxygen ~ time.hours, data=station4_60)

station5_10_reg <- lm(oxygen ~ time.hours, data=station5_10)
station5_30_reg <- lm(oxygen ~ time.hours, data=station5_30)
station5_50_reg <- lm(oxygen ~ time.hours, data=station5_50)
station5_70_reg <- lm(oxygen ~ time.hours, data=station5_70)

station6_10_reg <- lm(oxygen ~ time.hours, data=station6_10)
station6_30_reg <- lm(oxygen ~ time.hours, data=station6_30)
station6_50_reg <- lm(oxygen ~ time.hours, data=station6_50)
station6_80_reg <- lm(oxygen ~ time.hours, data=station6_80)

station8_sur_reg <- lm(oxygen ~ time.hours, data=station8_sur)
station8_10_reg <- lm(oxygen ~ time.hours, data=station8_10)
station8_30_reg <- lm(oxygen ~ time.hours, data=station8_30)
station8_50_reg <- lm(oxygen ~ time.hours, data=station8_50)
station8_80_reg <- lm(oxygen ~ time.hours, data=station8_80)

#cat together all the regression objects as a matrix
matrix_of_regressions <- cbind(
  station2_10_reg,
  station2_30_reg,
  station2_50_reg,
  station2_70_reg,
  station4_10_reg,
  station4_30_reg,
  station4_50_reg,
  station4_60_reg,
  station5_10_reg,
  station5_30_reg,
  station5_50_reg,
  station5_70_reg,
  station6_10_reg,
  station6_30_reg,
  station6_50_reg,
  station6_80_reg,
  station8_sur_reg,
  station8_10_reg,
  station8_30_reg,
  station8_50_reg,
  station8_80_reg
)
#take the regression values out of the matrix of lm objects
regression_values <- 
  as.data.frame(t(as.data.frame(matrix_of_regressions[1,]))) %>% select(time.hours)

# extract the sample names from the large dataframe
samples <- mydata %>% distinct(station, depth)

# make the table of rates
rates_table <- cbind(samples, regression_values)

rates_table <- rename(rates_table, oxygen_consumption_rate_uM_per_h = time.hours)

#write out to csv
write.csv(rates_table, file = "July_Spiekeroog_rates.csv", row.names = FALSE)
