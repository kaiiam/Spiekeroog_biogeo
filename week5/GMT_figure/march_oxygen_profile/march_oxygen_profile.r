#scrip sourced from http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(dplyr)
library(tidyr)
#gpclibPermit()

setwd("/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/GMT_figure/march_oxygen_profile")

march_oxygen_profile <- read.csv(file = "march_oxygen_profile.csv",header = TRUE)
march_oxygen_profile_test <- march_oxygen_profile #may not need to duplicate

min_x <- min(march_oxygen_profile_test$x)

#get x coordinantes for the grid
max_x <- max(march_oxygen_profile_test$x) - min_x

march_oxygen_profile  <- march_oxygen_profile %>% mutate(x= x - min_x )
march_oxygen_profile_test <- mutate(march_oxygen_profile_test, x= x-min_x )

min_x <- 0
min_y <- min(march_oxygen_profile_test$y) - 0.1
max_y <- max(march_oxygen_profile_test$y) + 0.1

#get y coordinates for the grid do so by modeling both the top and bottom lines
#with a function describing the slope of the beach 

#model top line
top <- filter(march_oxygen_profile_test, depth ==10)
#top <- mutate(top, x1 = x - min_x)

#model top line with loess function
loess_top <- loess(y ~ x, data = select(top, x, y))

#model bottom line:
bottom <- filter(march_oxygen_profile_test, depth ==100)
#bottom <- mutate(bottom, x1 = x - min_x)

#model bottom with loess function
loess_bottom <- loess(y ~ x, data = select(bottom, x, y))

# In order to deal with the oxygen values we need to filter NA's
march_oxygen_profile_test <- drop_na(march_oxygen_profile_test) 

#Create a data frame from all combinations of the supplied vectors or factors. 
#See the description of the return value for precise details of the way this is done.
#Set spatial coordinates to create a Spatial object. Assign gridded structure:

#set x to units of 1 coodinate unit seems ok as there is a diff of 42 long units
# the y I've set to 0.05 (5cm intervals)
#could change if necessary

####original grid
x.range <- as.numeric(c(min_x, max_x))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min_y, max_y))  # min/max latitude of the interpolation area

#changes the by to something like 0.01 to get higher resolution
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.02),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00124))

#length(seq(from = x.range[1], to = x.range[2], by = 0.02))
#length(seq(from = y.range[1], to = y.range[2], by = 0.00124))

# expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

#sp::coordinates	set spatial coordinates to create a Spatial object, or
#retrieve spatial coordinates from a Spatial object
#now made it into a "SpatialPointsDataFrame"
coordinates(march_oxygen_profile_test) = ~x + y

idw <- idw(formula = oxygen ~ 1, locations = march_oxygen_profile_test, 
           newdata = grd)  # apply idw model for the data

# krige0 <- krige0(formula = oxygen ~ 1, data = march_oxygen_profile_test, 
#           newdata = grd, model = vgm("Sph"))

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

#function to get y_coordinates from loess model using the x_coors vector
#filter values above and below beach
y_top_coors <- predict(loess_top, idw.output$long) + 0.1
y_bottom_coors <- predict(loess_bottom, idw.output$long) - 0.1

idw.output <- cbind(idw.output, y_top_coors,y_bottom_coors)
idw.output <- idw.output %>% filter(lat <= y_top_coors & lat >= y_bottom_coors )

#2 new ideas 1) set background colour though theme_light(color = "blue") or somehow
#            2) filter idw.output for values within range of black boxes (dicated by functions)
# polygon from http://sape.inf.usi.ch/quick-reference/ggplot2/geom_polygon
# annotation https://stackoverflow.com/questions/15903868/draw-multiple-squares-with-ggplot
# geom http://sape.inf.usi.ch/quick-reference/ggplot2/geom

#plot countour figure
p <- ggplot() + 
  
  annotate("polygon",
           x= c(min(idw.output$long),min((idw.output$long)),max((idw.output$long))),
           y=c(min(idw.output$lat),max(idw.output$lat),min(idw.output$lat)),
           fill="#e4dcac", alpha=1) +
  annotate("polygon",
           x= c(min(idw.output$long),max(idw.output$long),max(idw.output$long)),
           y=c(max(idw.output$lat),min(idw.output$lat),max(idw.output$lat)),
           fill="#B0C0D9", alpha=1) +
  
  ylab("Elevation (m)") +
  xlab("Distance (m) ") +
  geom_tile(data = idw.output, alpha = 1,
                     aes(x = long, y = lat, fill = round(var1.pred, 0))) +

  scale_fill_gradientn(colours = rev(rainbow(6) )) +
  
  #ggtitle("March Spiekeroog Oxygen Transect") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  
  #+ scale_fill_gradient(low = "blue", high = "red")
  geom_point(data = march_oxygen_profile, aes(x = x, y = y), shape = 0,
             colour = "black") + labs(fill = expression(paste("Oxygen (μmol l"^"-1" *")" )))
p

ggsave(filename = "/home/kai/Desktop/grad_school/marmic/lab_rotations/rotation_3/Spiekeroog_biogeo/week5/GMT_figure/march_oxygen_profile/March_oxygen_profile.jpeg", 
       plot =  p, width = 8, height = 4)


# #working old don't mess with
# #plot countour figure
# ggplot() + geom_tile(data = idw.output, alpha = 0.8,
#                      aes(x = long, y = lat, fill = round(var1.pred, 0))) +
# 
#   scale_fill_gradientn(colours = rev(rainbow(6) )) +
# 
#   #+ scale_fill_gradient(low = "blue", high = "red")
#   geom_point(data = march_oxygen_profile, aes(x = x, y = y), shape = 0,
#              colour = "black") + labs(fill = expression(paste("Oxygen (μmol l"^"-1" *")" )), title = "March Spiekeroog Oxygen Transect")



###### old #########
#http://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/spatialstats.html has a wierd shaped grid try to emulate

# ## help from http://grokbase.com/t/r/r-sig-geo/141v3bszry/can-coordinate-intervals-be-not-constant-in-spatialpixelsdataframe-object
# #try to figure out the whole sp SpatialPixelDF stuff and figure out how to coherce the grid together via the tolerance function.
# delmepx <- SpatialPixelsDataFrame(march_oxygen_profile_test, data=grd, tolerance=0.618392)
# delmepol <- as(delmepx,"SpatialPolygonsDataFrame")
# delmepx2 <- SpatialPixelsDataFrame(coordinates(delmepol), delmepol at data)
# 
# #resp
# fullgrid(delmepx)=TRUE
# #or alternatively
# delmepx = as(delmepx, "SpatialGridDataFrame")
# ##

# #plot to visualize not necessary
# plot(bottom$x1, bottom$y)
# lines(bottom$x1, predict(loess_bottom), col = "blue")
# points(x_coors, y_bottom_coors)

#having the top y and x coors each a vector we need to generate the inbetween 
#y values, all of which correspond to the existing x coordinantes. 

#df <- as.data.frame(rbind(x_coors, y_top_coors,y_bottom_coors)) 

#df <- df %>% mutate(diff_top_bottom = y_top_coors - y_bottom_coors)

# need to do the following line, for each pair in y_bottom_coors y_top_coors
#y.range <- as.numeric(c(-1.3315, 1.0653))
#y = seq(from = y.range[1], to = y.range[2], by = 0.1)

#matrix <- as.matrix(rbind(y_top_coors,y_bottom_coors))

#apply(obj, num, )  num rows is 1 columns is 2 
#so well do wide formate and apply across each column

# #change the by to get smaller increments
# y_coor_generator <- function(x, matrix) {
#   y = seq(from = x[2], to = x[1], by = 0.01 )
#   return(rev(y))
# }
# y_out <- apply(matrix,2 ,FUN=function(x) y_coor_generator(x)  )
# 
# y_out <- t(y_out)
# y_out <- as.data.frame(cbind(x_coors,y_out))

# #first iteration:
# init <- select(y_out,x_coors, y=V2)
# 
# #start for loop here i in 3:11
# for (i in 3:92) {
#   tmp <- select(y_out,x_coors, y= i)
#   init <- bind_rows(init, tmp)
# }






#old july 20th

tmp <- select(y_out,x_coors, y=V4)

init <- bind_rows(init, tmp)

#whats happening inside each
matrix[,1][1]
matrix[,1][2]

y = seq(from = matrix[,1][2], to = matrix[,1][1], by = 0.1)


#fuck it I'm doing a for loop!

y_out <- rbind(x_coors,y_out)

y_out[2,]


a <- cbind(y_out[2,],x_coors)

b <- cbind(y_out[3,],x_coors)

joint <- c(a,b)


#maybe try with dplyr 

y_out_df <- as.data.frame(y_out)

y_out_df %>% rowwise() %>% select(everything())

y_out_df %>% rowwise() %>% sapply(select(everything()), mean )



x_y_coor_match <- function(x, matrix, int) {
  
  return(x[1])
}

x_y_out <- apply(y_out,2, FUN =function(x) x_y_coor_match(x))

View(x_y_out)

#lets try it in a long format it may be better because the columns 1-423 already 
#correspond to the x values (it may not really help but it's at least good practice to try the other way)

matrix_long <- as.matrix(cbind(y_top_coors,y_bottom_coors))

matrix_long[1,][1]
matrix_long[1,][2]

y_long <- seq(from = matrix_long[1,][2], to = matrix_long[1,][1], by = 0.1)


y_coor_generator_long <- function(x, matrix) {
  
  y = seq(from = x[2], to = x[1], by = 0.1)
  
  #return(rev(y))
  return(x[2])
}

y_out_long <- apply(matrix,1 ,FUN=function(x) y_coor_generator_long(x)  )







#old from July 19th

#model top line using nls exp function
top_curve <- minpack.lm::nlsLM(y ~ (exp(1)^(a - b * x1) - c), data = select(top, x1, y),
                               start = list(a = 1.24977, b = 0.0953959, c = 0.498891 ), trace = T)
A <- coef(top_curve)[1]
B <- coef(top_curve)[2]
C <- coef(top_curve)[3]

#plot to visualize
plot(top$x1, top$y)
lines(top$x1, (exp(1)^(A - B * top$x1) - C), col = "red")

#can use the curve function to get the subset of points along a mathmatical function
top_list <- curve((exp(1)^(A - B * x)-C), from = 0, to = max_x, n=423)
points(x_coors, top_list$y)




formula(loess_fit)

bottom_list <- curve(predict(loess_fit))

#predict(loess_fit)

loess_fit$fitted

#function to take the list x_coors and call approx on each 
#to generate corresponding y values from the loess model

sapply(1:3, function(x, y) mean(y[,x]), y=m)

y_coor_generator <- function(x, loess_obj) {
  
  return(approx(x=loess_obj$fitted, y=loess_obj$x, xout=givenY)$y)
  #return(x)
}

sapply(x_coors, FUN=function(x) y_coor_generator(x, loess_fit)  )

# define a given y value at which you wish to approximate x from the loess line
givenY <- -0.9
estX <- approx(x=loess_fit$fitted, y=loess_fit$x, xout=givenY)$y
# add corresponding lines to the plot
abline(h=givenY, lty=2)
abline(v=estX, lty=2)


estX <- approx(x=loess_fit$fitted, y=loess_fit$x, xout=givenY)$y








#old from July 19
#WTF is up with that wierd red line? try it again without c?
top_curve <- minpack.lm::nlsLM(y ~ (exp(1)^(a - b * x1)), data = select(test, x1, y), 
                               start = list(a = -0.502451, b = 0.295768), trace = T)

A <- coef(top_curve)[1]
B <- coef(top_curve)[2]
C <- coef(top_curve)[3]
#coef(top_curve)

plot(test$x1, test$y)
lines(test$x1, (exp(1)^(A - B * test$x1)), col = "red")
summary(top_curve)

#try polynomial fit:

top_curve <- minpack.lm::nlsLM(y ~ (exp(1)^(a - b * x1)), data = select(test, x1, y), 
                               start = list(a = -0.502451, b = 0.295768), trace = T)


# fit a loess line
loess_fit <- loess(y ~ x1, data = select(test, x1, y))
plot(test$x1, test$y)
lines(test$x1, predict(loess_fit), col = "blue")


#predict(loess_fit)

#lm_obj <- lm(y ~ a * x1^2 + b * x1 + c, data = select(test, x1, y))



