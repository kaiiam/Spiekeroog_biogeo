#from http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/GMT_figure/test_example")

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
#gpclibPermit()

estonia_air_temperature_2 <- read.csv(file = "estonia_air_temperature_2.csv", 
                                      header = TRUE)
estonia_air_temperature_2_test <- estonia_air_temperature_2  # duplicate air temp. data file
estonia_air_temperature_2_test$x <- estonia_air_temperature_2_test$lon  # define x & y as longitude and latitude
estonia_air_temperature_2_test$y <- estonia_air_temperature_2_test$lat

#sp::coordinates	set spatial coordinates to create a Spatial object, or
#retrieve spatial coordinates from a Spatial object
#now made it into a "SpatialPointsDataFrame"
coordinates(estonia_air_temperature_2_test) = ~x + y

plot(estonia_air_temperature_2_test)

x.range <- as.numeric(c(21.76, 28.21))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(57.45, 59.72))  # min/max latitude of the interpolation area

#Create a data frame from all combinations of the supplied vectors or factors. 
#See the description of the return value for precise details of the way this is done.
#Set spatial coordinates to create a Spatial object. Assign gridded structure:

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))
                                                  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

#don't need this it was just to visualize the grid
#plot(grd, cex = 1.5, col = "grey")
#points(estonia_air_temperature_2_test, pch = 1, col = "red", cex = 1)

#may12 is the column of air temperature values
#Interpolate surface and fix the output:
#this makes the interpolated data points between the actual values over the grid
#I'll need to find a way of removing the values of var1.pred which are above the transect line
idw <- idw(formula = may12 ~ 1, locations = estonia_air_temperature_2_test, 
           newdata = grd)  # apply idw model for the data

#not sure if this is necessary could also do with dplyr
idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

#plot
ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) + 
  geom_point(data = estonia_air_temperature_2, aes(x = lon, y = lat), shape = 21, 
             colour = "red")

#adapt from this with the est_contour
ggplot() + geom_tile(data = idw.output, alpha = 0.8, aes(x = long, y = lat, 
                                                         fill = round(var1.pred, 0))) + scale_fill_gradient(low = "cyan", high = "orange") + 
  geom_point(data = estonia_air_temperature_2, aes(x = lon, y = lat), shape = 21, 
             colour = "red") + labs(fill = "Air temp.", title = "Air temperature in Estonia, 15/May/2010")




# krig tutorial from: http://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/spatialstats.html
data(meuse)
coordinates(meuse) = ~x + y
data(meuse.grid)

plot(meuse.grid$x, meuse.grid$y)

gridded(meuse.grid) = ~x + y
m <- vgm(0.59, "Sph", 874, 0.04)
# ordinary kriging:
x <- krige(log(zinc) ~ 1, meuse, meuse.grid, model = m)

## [using ordinary kriging]

spplot(x["var1.pred"], main = "ordinary kriging predictions")


grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))
# expand points to grid

#can use the curve function to get the subset of points along a mathmatical function
c <- curve((0.5)*x^2+1, from = 0, to = 5) 
#extract the list of such points via:
c$y
