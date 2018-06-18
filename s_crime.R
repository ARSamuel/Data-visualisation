#load in data
crime0215 <- read.csv("2015-02-metropolitan-street.csv", stringsAsFactors = F) 
crime0215 <- crime0215[, -c(3, 4, 7, 11, 12)]
crime0315 <- read.csv("2015-03-metropolitan-street.csv", stringsAsFactors = F) 
crime0315 <- crime0315[, -c(3, 4, 7, 11, 12)]
crime0415 <- read.csv("2015-04-metropolitan-street.csv", stringsAsFactors = F)
crime0415 <- crime0415[, -c(3, 4, 7, 11, 12)]
crime0515 <- read.csv("2015-05-metropolitan-street.csv", stringsAsFactors = F) 
crime0515 <- crime0515[, -c(3, 4, 7, 11, 12)]
crime0615 <- read.csv("2015-06-metropolitan-street.csv", stringsAsFactors = F) 
crime0615 <- crime0615[, -c(3, 4, 7, 11, 12)]
crime0715 <- read.csv("2015-07-metropolitan-street.csv", stringsAsFactors = F) 
crime0715 <- crime0715[, -c(3, 4, 7, 11, 12)]
crime0815 <- read.csv("2015-08-metropolitan-street.csv", stringsAsFactors = F) 
crime0815 <- crime0815[, -c(3, 4, 7, 11, 12)]
crime0915 <- read.csv("2015-09-metropolitan-street.csv", stringsAsFactors = F) 
crime0915 <- crime0915[, -c(3, 4, 7, 11, 12)]
crime1015 <- read.csv("2015-10-metropolitan-street.csv", stringsAsFactors = F) 
crime1015 <- crime1015[, -c(3, 4, 7, 11, 12)]
crime1115 <- read.csv("2015-11-metropolitan-street.csv", stringsAsFactors = F) 
crime1115 <- crime1115[, -c(3, 4, 7, 11, 12)]
crime1215 <- read.csv("2015-12-metropolitan-street.csv", stringsAsFactors = F) 
crime1215 <- crime1215[, -c(3, 4, 7, 11, 12)]
crime0116 <- read.csv("2016-01-metropolitan-street.csv", stringsAsFactors = F) 
crime0116 <- crime0116[, -c(3, 4, 7, 11, 12)]

#bind
crime <- rbind(
  crime0215, crime0315, 
  crime0415, crime0515, 
  crime0615, crime0715, 
  crime0815, crime0915, 
  crime1015, crime1115, 
  crime1215, crime0116
  )

crime <- crime[complete.cases(crime), ]
crime <- crime[,-1]
crime_sp <- SpatialPointsDataFrame(crime[, 2:3], crime, proj4string = CRS("+init=epsg:4326"))
crime_sp <- spTransform(crime_sp, CRS("+init=epsg:27700"))

#subset sexual offences
library(data.table)
subcrime <- crime[crime$Crime.type %like% "Violence and sexual offences", ]
subcrime <- subcrime[complete.cases(subcrime), ]
subcrime$Longitude <- as.numeric(subcrime$Longitude)
subcrime$Latitude <- as.numeric(subcrime$Latitude)
write.csv(subcrime, "subcrime.csv")
subcrime_sp <- SpatialPointsDataFrame(subcrime[, 2:3], subcrime, proj4string = CRS("+init=epsg:4326"))
subcrime_sp<- spTransform(subcrime_sp, CRS("+init=epsg:27700"))
writeOGR(subcrime_sp, "subcrime_sp", driver = "ESRI Shapefile", layer = "subcrime_sp")
plot(subcrime_sp)


Extent<- extent(ldn) 
resolution <- 100
#This is some magic that creates the empty grid
x <- seq(Extent[1], Extent[2], by = resolution)  # where resolution is the pixel size you desire 
y <- seq(Extent[3], Extent[4], by = resolution) 
xy <- expand.grid(x = x, y = y) 
coordinates(xy) <- ~ x + y 
gridded(xy) <- TRUE 

plot(xy)
plot(ldn, border="red", add=T)

library(adehabitatHR)
#Note we are running two functions here - first KernelUD then converting the result to a raster.
kde.sub <- raster(kernelUD(subcrime_sp, h ="href", grid = xy)) 
kde.subv <- kernelUD(subcrime_sp, h ="href", grid = xy)

#Now we can see the hotspots much more clearly.
plot(kde.subv)
plot(ldn, border = "white", add = T)


####


library(tmap)
library(tmaptools)
library(RColorBrewer)

#maps the raster in tmap, "ud" is the density variable
tm_shape(dual2) +
  tm_raster("ud")

#creates a bounding box based on the extent of the Output.Areas polygon
#bounding box helps us zoom in
bounding_box <- bbox(ldn)

#maps the raster within the bounding box
tm_shape(kde.sub, bbox = bounding_box) + tm_raster("ud")

#mask the raster by the output area polygon
#mask means that we're only including the data within the bounds of the mask
masked_kde <- mask(kde.sub, ldn)

#maps the masked raster + white output area boundaries
tm_shape(masked_kde, 
         bbox = bounding_box) +
  tm_raster("ud", 
            style= "quantile", 
            n= 100, 
            legend.show = F, 
            palette = "YlGnBu") +
  tm_shape(ldn) +
  tm_borders(alpha=.3, 
             col="white") +
  tm_layout(frame=F)

#compute hpmeranges for 75%, 50%, and 25% of points
#objects are returned as spatial polygon data frames
#range 75 is the 75% most clustered data
#os the densest 75%
range75 <- getverticeshr(kde.subv, percent = 75)
range50 <- getverticeshr(kde.subv, percent = 50)
range25 <- getverticeshr(kde.subv, percent = 25)
range10 <- getverticeshr(kde.subv, percent = 10)



#Create a grey background using the Output.Areas polygon with white borders
# . Plot the locations of houses using House.Points
# . Plot the 75% range, set attributes for border and fill (i.e. colour, transparency, line width)
# . Plot the 50% range, set attributes for border and fill (i.e. colour, transparency, line width)
# . Plot the 25% range, set attributes for border and fill (i.e. colour, transparency, line width)
# . The outside frame is removed
# add range for the densest 10%

range10 <- getverticeshr(kde.subv, percent = 10)

tm_shape(ldn) + tm_fill(col = "#c4c4c4") + tm_borders(alpha = .8, col = "white") +
  tm_shape(subcrime_sp) + tm_dots(col = "#1e264f", size = .08, alpha = .5) +
  tm_shape(range75) + tm_borders(alpha = .5, col = "#efbdcc", lwd = 2) + tm_fill(alpha = .3, col = "#9b373c") +
  tm_shape(range50) + tm_borders(alpha = .5, col = "#e28aa5", lwd = 2) + tm_fill(alpha = .3, col = "#84262b") +
  tm_shape(range25) + tm_borders(alpha = .5, col = "#dd547e", lwd = 2) + tm_fill(alpha = .3, col = "#681f23") +
  tm_shape(range10) + tm_borders(alpha = .5, col = "#870028", lwd = 2) + tm_fill(alpha = .3, col = "#3a1214") +
  tm_layout(frame = FALSE, 
            title = "Kernel Density Estimates of Sexual \nand Violent Crime In London",
            inner.margins = c(0.10, 0.15),
            legend.position = c(0.75, 0.15)) +
  tm_add_legend(type = "fill",
                col = c("#efbdcc", "#e28aa5", "#dd547e", "#870028"),
                alpha = .8,
                labels = c("75% highest density", 
                           "50% highest density", 
                           "25% highest density", 
                           "10% highest density"),
                title = "Density percentages",
                border.col = "#efbdcc")
  

