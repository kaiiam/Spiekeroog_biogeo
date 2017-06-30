library(ggplot2)

setwd("~/Desktop/grad_school/marmic/lab_rotations/rotation_3/week1/experiments/exp2")

mydata = read.csv("exp2_out.csv")

# can use the strptime function to access the HH:MM:SS time formated data

#almost not quite working
mydata$Time..HH.MM.SS. <- toString( mydata$Time..HH.MM.SS.)


#try: https://stackoverflow.com/questions/19235466/how-do-i-plot-time-hhmmss-in-x-axis-in-r

mydata$Time..HH.MM.SS. <- as.POSIXct(strptime(mydata$Time..HH.MM.SS., format="%H:%M:%S"))
