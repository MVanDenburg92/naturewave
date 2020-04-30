## ----setup, include=FALSE-----------------------------------------------------
# knitr::opts_knit$set(root.dir = "~/external/data/")

knitr::opts_chunk$set(echo = TRUE)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#Libraries
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(soundecology)
library(tuneR)
library(seewave)
library(nlme)
library(dplyr)
library(mefa)
library(plotly)
library(geospaar)
library(landscapemetrics)
library(RColorBrewer)


# install.packages("USAboundaries")
# install.packages("maps")
#rnaturalearth
#maps package
#tidycensus
library(maps)
library(USAboundaries)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
# setwd("~/Spatial_Analysis_R/naturewave/external/")


# 
# setwd("~/Spatial_Analysis_R/naturewave/external/")
# 
# read_csv(system.file("naturewave/external/DF_Fulltable_CBIND.csv", 
#                                 package = "naturewave"))

setwd(dir = "../external/data/")
DF <- read.csv("DF_Fulltable_CBIND.csv")


DF_NDSI <- DF %>% dplyr::select(SITE, NDSI_select, Buffer)


DF_Transposed_NDSI <- pivot_wider(DF_NDSI, names_from = Buffer, values_from = NDSI_select)


## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
##import Shapefile for the sites of interest

# setwd("~/Spatial_Analysis_R/naturewave/CENSUS2010TIGERROADS_SHP/")
# sample_points_f <- st_read("sample_sites_projected.shp")


setwd("../external/data/sample_sites/")
sample_points_f <- st_read("sample_sites_projected.shp")

sample_points_f <- sample_points_f %>% dplyr::select(-OBJECTID_1)


##Examine geometry list-column 
st_geometry(sample_points_f)

##Examine attributes of sample points
attributes(sample_points_f)

#Examines coordinate system of the sample points 
st_crs(sample_points_f)



# ##import Shapefile for the sites of interest projeccted into albers equal area
# getwd()
# setwd("../naturewave/external/sample_sites/")
# sample_points_Albers <- st_read("sample_sites_projected_albers.shp")
# 
# sample_points_Albers
# 
# 
# #Examines coordinate system of the sample points projected to NAD 1983 Albers 
# st_crs(sample_points_Albers)



#Change crs to be of same system for Sample_points_f and my_aoi_raster
# 
# sample_points_f <- st_transform(x = sample_points_f, crs = st_crs(my_raster))
# st_crs(sample_points_f)
# 
# my_aoi_raster <- st_transform(x = my_aoi_raster, crs = st_crs(my_raster))
# st_crs(my_aoi_raster)
# 
# my_aoi_raster_proj <- projectRaster(my_aoi_raster, crs = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")





## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
# Import AOI shapefile containing site

setwd("../external/data/sample_sites/")
AOI <- st_read("AOI.shp")
AOI <- st_transform(x = AOI, crs = st_crs(sample_points_f))

#We can also create the shapefile this way
pol <- st_polygon(list(cbind(x = c(42.02124129930544, 42.69707031707116, 42.69707031707116, 42.69707031707116, 42.02124129930544),  y = c(-71.53416964017146, -71.53416964017146, -72.30321260892146, -72.30321260892146, -71.53416964017146))))



#import counties for US
# county map of Massachusetts

counties <- st_as_sf(map(database = "county", plot = FALSE, fill = TRUE)) %>% lwgeom::st_make_valid()

#add in state and county fields using data from ID column 
counties <- counties %>% mutate(state = gsub(",.*", "", ID)) %>% mutate(county = gsub(".*,", "", ID))

massmaps <- counties %>% filter(grepl("massachusetts", state, ignore.case = TRUE))
#plot(massmaps,border = "transparent")
#or masscounties %>% plot(.,border = "transparent")

masscounties <- massmaps[, "county"] 

ma_crop <- masscounties %>% st_transform(crs = st_crs(sample_points_f)) %>% st_crop(., AOI)

#Plot out AOI on top of massachussets state boundary

plot(ma_crop)

## -----------------------------------------------------------------------------
# ##import Raster isolated to AOI, in Albers Equal Area
# 
# my_aoi_raster_albers = raster("D:/RA-withSangermano/SpatialData/NOAA-CCAP/aoi_massclass_2.tif")
# my_aoi_raster_albers
# st_crs(my_aoi_raster_albers)
# plot(my_aoi_raster_albers)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#import immperviousness raster file
imperv <- raster("../external/data/Amherst_CAPS2011/imperv.tif", crs = crs(sample_points_f))
plot(imperv)
plot(sample_points_f, col = "red", add = TRUE)
plot(AOI, add = TRUE)
st_crs(imperv)

#Crop Imperv to AOI and plot
imperv_cropped <- crop(imperv, y = AOI)
plot(imperv_cropped)
plot(sample_points_f, col = "red", add = TRUE)



#import connectedness raster file

connect <- raster("../external/data/Amherst_CAPS2011/connect.tif", crs = crs(sample_points_f))
plot(connect)
plot(sample_points_f, col = "red", add = TRUE)
plot(AOI, add = TRUE)
st_crs(connect)


#Crop connect to AOI and plot
connect_cropped <- crop(connect, y = AOI)
plot(connect_cropped)
plot(sample_points_f, col = "red", add = TRUE)


#import structure raster file
structure <- raster("../external/data/Amherst_CAPS2011/structure.tif", crs = crs(sample_points_f))
plot(structure)
plot(sample_points_f, col = "red", add = TRUE)
plot(AOI, add = TRUE)
st_crs(structure)

#Crop structure to AOI and plot
structure_cropped <- crop(structure, y = AOI)
plot(structure_cropped)
plot(sample_points_f, col = "red", add = TRUE)





# #Road traffic traffic Measures the intensity of road traffic (based on measured road traffic rates) in the neighborhood surrounding the focal cell, based on a logistic function of distance.
# Data source: landcover, traffic rates



#import connectedness raster file
traffic <- raster("../external/data/Amherst_CAPS2011/traffic.tif", crs = crs(sample_points_f))
plot(traffic)
plot(sample_points_f, col = "green", add = TRUE)
plot(AOI, add = TRUE)
st_crs(traffic)


#Crop connect to AOI and plot
traffic_cropped <- crop(traffic, y = AOI)
plot(traffic_cropped)
plot(sample_points_f, col = "green", add = TRUE)

plot(imperv_cropped)



###Example plot of data

par(mar = c(0, 0, 1, 0) + .1)
plot(structure, c, axes = FALSE, nr = 4)
plot(sample_points_f, col = "red", add=TRUE)

#Example of plotting out a single site
par(mar = c(0, 0, 1, 0) + .1)
plot(structure, c, axes = FALSE, nr = 4)
sample_points_f %>% slice(1) %>% plot(col = "red", add=TRUE)


## ----include=FALSE, warning = FALSE, message = FALSE--------------------------

#Create matrix of points from geometry fieldof our sample points to utilize with landscapemetrics
samp_matrix <- unlist(matrix(sample_points_f$geometry, ncol = 2))
samp_matrix <- matrix(data = samp_matrix, ncol = 2, byrow = TRUE)[-12,]


## ----include=FALSE, warning = FALSE, message = FALSE--------------------------

#Sources for understanding focal window
# http://r-sig-geo.2731867.n2.nabble.com/Raster-package-Focal-sum-in-circles-td7587683.html
# https://gis.stackexchange.com/questions/292409/why-different-results-of-mean-calculations-with-focal-in-r-and-esri-arcgis
# https://www.timassal.com/?p=2092
# https://gis.stackexchange.com/questions/287553/how-to-use-sum-of-circular-moving-window-for-each-center-cell-with-focal-in-r
# https://www.rdocumentation.org/packages/raster/versions/3.0-12/topics/focal
# https://www.rdocumentation.org/packages/raster/versions/3.0-12/topics/focalWeight



# NOTES ON FOCAL FUNCTION
#.............................................

#focal passes over each image pixel, and multiplies those weights by each pixel value in the neighborhood, and then sums those to get the mean
# It sums the values because sum is the default value of the argument “fun” in the function focal, which is why we have not even specified the argument “fun” in Block 1
# The focal function operates on the matrix, regardless of it being circular or rectangular and assigns the resulting value to the center cell. In a binary matrix values with 1 would be operated on and those with 0, ignored. A circular window is a rectangular matrix where, values occuring outside the defined radius are 0. 


# Since we are interested in the range of the recording device and because we have assigned a consistent value across the buffers for NDSI, ANT, and BIO varying per site per metric, we are going to use a focalWeight circle to replicate the 3000m buffer and pass it over the entirety of the images for imperv, connect, and structure. This will give us a raster for each and 


#2 apply circular moving window to continuous data
###
#set the focal weight, since we are using a circle, set number to the radius of the circle (in units of CRS)
#which you can conventientyly check using the check_landscape() function
check_landscape(imperv_cropped)
#cell resolution is 30 x 30
fw_sum <- focalWeight(imperv_cropped, 3000, type='circle') 
#have a look at the shape of the moving window
fw_sum

# apply moving window

# If we apply fw to the focal function specifying the mean function then we get unexpected results.
imperv_cropped__focal_mean_unfixed <-  focal(imperv_cropped, w = fw_sum, na.rm = TRUE, fun = mean)


# In fact to get the mean using the weighted matrix, you dont need to specify the function `mean`.Your focal weights sum to 1, such that you get the mean value when using 'fun=sum', which is the default for focal()
imperv_cropped__focal <- focal(imperv_cropped, w = fw_sum, na.rm = TRUE) 



#Plot the results

#plot original
plot(imperv_cropped, main ="Input Raster") 
#plot result of circular moving window
plot(imperv_cropped__focal, main ="Circular MW Mean Using Spatial Weight, No Mean Function")
plot(imperv_cropped__focal_mean_unfixed, main ="Circular MW Mean Unfixed") 



#NOTES ON FOCALWEIGHT
# ...................................

# With focalWeight and type='circle' the cells can be 0 or sum up to 0 (which means they are often really small numbers). Since the weights instead make it so that they add up to 0, the decimal values are returned as the values. 

#If you want to apply the function mean after the fact you have to change them to only cells of 0 and 1 
#If you do not specify the `fun = mean` you are then able to calculate the sum of the values

fw_sum[fw_sum > 0] <- 1

fw_fixed <- fw_sum


#recall the code we used above to try and get the mean....now featuring the fixed weighted matrix
imperv_cropped__focal_mean_fixed <-  focal(imperv_cropped, w = fw_fixed, na.rm = TRUE, fun = mean)


#But what if we want to calculate the sum?
imperv_cropped__focal_sum <-  focal(imperv_cropped, w = fw_fixed, na.rm = TRUE, fun = sum)




# plot(imperv_cropped__focal, main="Circular MW SUM")
plot(imperv_cropped__focal_mean_unfixed, main ="Circular MW Mean Unfixed") 
plot(imperv_cropped__focal_mean_fixed, main ="Circular MW Mean with `fun= mean` specified")
plot(imperv_cropped__focal, main ="Circular MW correct without specifying `fun = mean`")
plot(imperv_cropped, main ="Input Raster") #plot original
plot(imperv_cropped__focal_sum, main ="Circular MW correct sum only ")


geospaar::plot_noaxes(imperv_cropped__focal_mean_fixed, main ="Circular MW Mean with `fun= mean` specified")


