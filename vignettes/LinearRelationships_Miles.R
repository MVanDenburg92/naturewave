## ----setup, include=FALSE-----------------------------------------------------
# knitr::opts_knit$set(root.dir = "~/external/data/")
knitr::opts_chunk$set(echo = TRUE)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#Libraries
library(tidyverse)
library(nlme)
library(ggplot2)
library(dplyr)
library(raster)
library(sf)
library(lwgeom)
# install.packages("USAboundaries")
# install.packages("maps")
#rnaturalearth
#maps package
#tidycensus
library(maps)

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

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
# ##import Raster isolated to AOI, in Albers Equal Area
# 
# my_aoi_raster_albers = raster("D:/RA-withSangermano/SpatialData/NOAA-CCAP/aoi_massclass_2.tif")
# my_aoi_raster_albers
# st_crs(my_aoi_raster_albers)
# plot(my_aoi_raster_albers)

## ---- warning = FALSE, message = FALSE----------------------------------------

###[1]

#import immperviousness raster file
imperv <- raster("../external/data/Amherst_CAPS2011/imperv.tif", crs = crs(sample_points_f))

##Plot code to test tif
# plot(imperv)
# plot(sample_points_f, col = "red", add = TRUE)
# plot(ma_crop, add = TRUE)
# st_crs(imperv)

#Crop Imperv to AOI and plot  (code commented out)
imperv_cropped <- crop(imperv, y = ma_crop)
# plot(imperv_cropped)
# plot(sample_points_f, col = "red", add = TRUE)



#import connectedness raster file

connect <- raster("../external/data/Amherst_CAPS2011/connect.tif", crs = crs(sample_points_f))

##Plot code to test tif
# plot(connect)
# plot(sample_points_f, col = "red", add = TRUE)
# plot(ma_crop, add = TRUE)
# st_crs(connect)


#Crop connect to AOI and plot  (code commented out)
connect_cropped <- crop(connect, y = ma_crop)
# plot(connect_cropped)
# plot(sample_points_f, col = "red", add = TRUE)


#import structure raster file
structure <- raster("../external/data/Amherst_CAPS2011/structure.tif", crs = crs(sample_points_f))

##Plot code to test tif
# plot(structure)
# plot(sample_points_f, col = "red", add = TRUE)
# plot(AOI, add = TRUE)
# st_crs(structure)

#Crop structure to AOI and plot  (code commented out)
structure_cropped <- crop(structure, y = ma_crop)
# plot(structure_cropped)
# plot(sample_points_f, col = "red", add = TRUE)


# #Road traffic traffic Measures the intensity of road traffic (based on measured road traffic rates) in the neighborhood surrounding the focal cell, based on a logistic function of distance.
# Data source: landcover, traffic rates

#import connectedness raster file
traffic <- raster("../external/data/Amherst_CAPS2011/traffic.tif", crs = crs(sample_points_f))
##Plot code to test tif
# plot(traffic)
# plot(sample_points_f, col = "green", add = TRUE)
# plot(AOI, add = TRUE)
# st_crs(traffic)


#Crop connect to AOI and plot (code commented out)
traffic_cropped <- crop(traffic, y = ma_crop)
# plot(traffic_cropped)
# plot(sample_points_f, col = "green", add = TRUE)



###Example plot of data
# 
# par(mar = c(0, 0, 1, 0) + .1)
# plot(structure, c, axes = FALSE, nr = 4)
# plot(sample_points_f, col = "red", add=TRUE)
# 
# #Example of plotting out a single site
# par(mar = c(0, 0, 1, 0) + .1)
# plot(structure, c, axes = FALSE, nr = 4)
# sample_points_f %>% slice(1) %>% plot(col = "red", add=TRUE)



# Loading all the files together at once into a list
f <- dir("../external/data/Amherst_CAPS2011", full.names = TRUE, pattern = ".tif")
landcover_list <- list(connect_cropped, imperv_cropped, structure_cropped, traffic_cropped)
names(landcover_list) <- gsub("tif", "", basename(f))

#[2]  all together

# f <- dir("./data/Amherst_CAPS2011", full.names = TRUE, pattern = ".tif")
# landcover <- lapply(unname(f), function(x) {
#   r <- raster(x)
#   crs(r) <- crs(pts)
#   r
# })
# names(landcover) <- gsub(".tif", "", basename(f))


#Scale the images subtracting the min values of each (the zero values) and then dividing by the max value possible in the images, 255. 
lc_scaled <-lapply(landcover_list,function(x){
  (x- cellStats(x, min)) / 255
})
lc_stack <- stack(lc_scaled)

#Plot out all of the images at once
plot(lc_stack)

# #plot out the Connect layer
# plot(lc_stack[[1]], main = "connect")
# plot(st_geometry(sample_points_f), add = TRUE) 




## ---- warning = FALSE, message = FALSE----------------------------------------

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
# #cell resolution is 30 x 30
#...................................................................

#Now let's apply the focalweight function to get a focal matrix for each .tif file in the landcover_list 
# at the scale of best influence 

# #1500 Buffers
# fw_sum_1500 <- lapply(landcover_list,function(x){
#   fw <- focalWeight(x, 1500, type='circle')
#   })
# 
# 
# #2000 Buffers
# fw_sum_2000 <- lapply(landcover_list,function(x){
#   fw <- focalWeight(x, 2000, type='circle')
#   })

#2500 Buffers

fw_sum_2500 <- lapply(landcover_list,function(x){
  fw <- focalWeight(x, 2500, type='circle')
  })



#Runs focal at a buffer weight of 1500 m radius
# radius1 <- 1500
# lc1500 <- lapply(1:4, function(x) {  # x <- 1
#   f <- paste0("../external/data/Amherst_CAPS2011", names(lc_stack)[x], "_", radius1, ".tif")
#   r <- focal(lc_stack[[x]], w = as.matrix(fw_sum_1500[[x]]), filename = f, overwrite = TRUE)
#   return(r)
# })
# lc1500_stacked <- stack(lc1500)  # stack

# Plotting out the results
# tit1 <- "1500 (m) Buffer "
# par(mfrow = c(2, 2), mar = c(.5, 0, 2, 6))
# lapply(1:4, function(x){
#   plot(lc1500_stacked[[x]], axes = FALSE, box = FALSE, main =  toupper(c(tit1,gsub(".tif", "", basename(f[x])))))
#   plot(st_geometry(sample_points_f), add = TRUE)
# })



#Runs focal at a buffer weight of 2000 m radius

# radius2 <- 2000
# lc2000 <- lapply(1:4, function(x) {  # x <- 1
#   f <- paste0("../external/data/Amherst_CAPS2011", names(lc_stack)[x], "_", radius2, ".tif")
#   r <- focal(lc_stack[[x]], w = as.matrix(fw_sum_2000[[x]]), filename = f, overwrite = TRUE)
#   return(r)
# })
# lc2000_stacked <- stack(lc2000)  # stack


# Plotting out the results
# tit2 <- "2000 (m) Buffer "
# par(mfrow = c(2, 2), mar = c(.5, 0, 2, 6))
# lapply(1:4, function(x){
#   plot(lc2000_stacked[[x]], axes = FALSE, box = FALSE, main = toupper(c(tit2,gsub(".tif", "", basename(f[x])))))
#   plot(st_geometry(sample_points_f), add = TRUE)
# })



#Runs focal at a buffer weight of 2500 m radius

radius3 <- 2500
lc2500 <- lapply(1:4, function(x) {  # x <- 1
  f <- paste0("../external/data/Amherst_CAPS2011", names(lc_stack)[x], "_", radius3, ".tif")
  r <- focal(lc_stack[[x]], w = as.matrix(fw_sum_2500[[x]]), filename = f, overwrite = TRUE)
  return(r)
})
lc2500_stacked <- stack(lc2500)  # stack


# Plotting out the results
tit3 <- "2500 (m) Buffer"
par(mfrow = c(2, 2), mar = c(.5, 0, 2, 6))
lapply(1:4, function(x){
  plot(lc2500_stacked[[x]], axes = FALSE, box = FALSE, main = toupper(c(tit3, gsub(".tif", "", basename(f[x])))))
  plot(st_geometry(sample_points_f), add = TRUE)
})



## -----------------------------------------------------------------------------

# Code to write out rasters for use with the randomforest prediction 

path_out <- '../inst/extdata'

writeRaster(lc2500_stacked[[1]], file.path(path_out,"lc2500_connect.tif"), overwrite = TRUE)
writeRaster(lc2500_stacked[[2]], file.path(path_out,"lc2500_imperv.tif"), overwrite = TRUE)
writeRaster(lc2500_stacked[[3]], file.path(path_out,"lc2500_structure.tif"), overwrite = TRUE)
writeRaster(lc2500_stacked[[4]], file.path(path_out, "lc2500_traffic.tif"), overwrite = TRUE)


## ----eval = FALSE, include=FALSE, warning = FALSE, message = FALSE------------
#  #ALTERNATIVE METHOD
#  #.................................................................
#  
#  #Applying the window one file at a time
#  
#  #
#  crs(imperv_cropped)
#  #cell resolution is 30 x 30
#  fw_sum <- focalWeight(imperv_cropped, 3000, type='circle')
#  #have a look at the shape of the moving window
#  fw_sum
#  
#  
#  # apply moving window
#  
#  # If we apply fw to the focal function specifying the mean function then we get unexpected results.
#  
#  imperv_cropped_focal_mean_unfixed <-  focal(imperv_cropped, w = fw_sum, na.rm = TRUE, fun = mean)
#  
#  
#  # In fact to get the mean using the weighted matrix, you dont need to specify the function `mean`.Your focal weights sum to 1, such that you get the mean value when using 'fun=sum', which is the default for focal()
#  
#  ###imperv_cropped__focal <- focal(imperv_cropped, w = fw_sum, na.rm = TRUE)
#  
#  
#  
#  # #Plot the results
#  #
#  # #plot original
#  # plot(imperv_cropped, main ="Input Raster")
#  # #plot result of circular moving window
#  # plot(imperv_cropped__focal, main ="Circular MW Mean Using Spatial Weight, No Mean Function")
#  # plot(imperv_cropped__focal_mean_unfixed, main ="Circular MW Mean Unfixed")
#  
#  
#  #NOTES ON FOCALWEIGHT
#  # ...................................
#  
#  # With focalWeight and type='circle' the cells can be 0 or sum up to 0 (which means they are often really small numbers). Since the weights instead make it so that they add up to 0, the decimal values are returned as the values.
#  
#  #If you want to apply the function mean after the fact you have to change them to only cells of 0 and 1
#  #If you do not specify the `fun = mean` you are then able to calculate the sum of the values
#  
#  fw_sum[fw_sum > 0] <- 1
#  
#  fw_fixed <- fw_sum
#  
#  getwd()
#  #recall the code we used above to try and get the mean....now featuring the fixed weighted matrix
#  
#  
#  imperv_cropped_focal_mean_fixed <-  focal(imperv_cropped, w = fw_fixed, na.rm = TRUE, fun = mean)
#  
#  
#  #But what if we want to calculate the sum?
#  # ##imperv_cropped__focal_sum <-  focal(imperv_cropped, w = fw_fixed, na.rm = TRUE, fun = sum)
#  
#  # plot(imperv_cropped__focal, main="Circular MW SUM")
#  # plot(imperv_cropped__focal_mean_unfixed, main ="Circular MW Mean Unfixed")
#  
#  plot(imperv_cropped_focal_mean_fixed, main ="Circular MW Mean with `fun= mean` specified")#plot single mw mean
#  plot(imperv_cropped, main ="Input Raster") #plot original
#  # plot(imperv_cropped__focal_sum, main ="Circular MW correct sum only ")
#  

## ---- warning = FALSE, message = FALSE----------------------------------------
pts1 <- cbind(sample_points_f, raster::extract(lc_stack, sample_points_f))  # original lc


# pts_1500 <- cbind(pts1, raster::extract(lc1500_stacked, pts1))# buffer points 1500
# write.csv(pts_1500, file.path(path_out,"fv_pts_1500.csv"), row.names = FALSE)
# 
# pts_2000 <- cbind(pts1, raster::extract(lc2000_stacked, pts1))  # buffer points 2000
# write.csv(pts_2000, file.path(path_out,"fv_pts_2000.csv"), row.names = FALSE)

pts_2500 <- cbind(pts1, raster::extract(lc2500_stacked, pts1)) # buffer points 2500
write.csv(pts_2500, file.path(path_out,"fv_pts_2500.csv"), row.names = FALSE)


## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
set.seed(1)
buffs_extract <- lapply(c(500, 1000, 1500, 2000, 2500, 3000), function(x) { 
  #2a
  buf_samplesites <- st_buffer(sample_points_f, dist = x) # here is the buffer
  #2c
  list("sites" = buf_samplesites)
  #ex <- extract(imperv_cropped, buf_samplesites, fun=mean, na.rm=TRUE, df=TRUE)

})


# #attempt at completing this using a forloop
# buffs_extract_forloop <-  for(i in sizes){
# buf_samplesites[[i]] <- st_buffer(sample_points_f, sizes[i])
# list("sites" = buf_samplesites)
# 
# }

#Extract the mean value from the impervious tif file for each buffer zone

set.seed(1)
buffs_extract_zonal_Impervious <- sapply(c(500, 1000, 1500, 2000, 2500, 3000),
          function(x) { 
                        zonal <- raster::extract(imperv_cropped, sample_points_f,
                        fun= mean, buffer = x, na.rm=TRUE)
})
buffs_extract_zonal_Impervious <- as.data.frame(buffs_extract_zonal_Impervious)



#Create new field called 'SiteName' using the site names from sample_points_f$Site

buffs_extract_zonal_Impervious <- buffs_extract_zonal_Impervious %>% 
  mutate(SiteName = sample_points_f$Site) %>% rename("Buffer 500 (m)" = V1, 
                                                     "Buffer 1000 (m)" = V2, 
                                                     "Buffer 1500 (m)" = V3, 
                                                     "Buffer 2000 (m)" = V4, 
                                                     "Buffer 2500 (m)" = V5, 
                                                     "Buffer 3000 (m)" = V6) %>%
  dplyr::select(SiteName, everything())



buffs_extract_zonal_Impervious_pivotted <- buffs_extract_zonal_Impervious %>% 
  pivot_longer(-SiteName, names_to = "Buffer", values_to = "Imperv") %>% arrange(SiteName)

#Extract the mean value from the Connectedness tif file for each buffer zone

set.seed(1)
buffs_extract_zonal_connect <- sapply(c(500, 1000, 1500, 2000, 2500, 3000),
          function(x) { 
            zonal <- raster::extract(connect_cropped, sample_points_f, 
                                     fun= mean, buffer = x, na.rm=TRUE)
})
buffs_extract_zonal_connect <- as.data.frame(buffs_extract_zonal_connect)



#Create new field called 'SiteName' using the site names from sample_points_f$Site

buffs_extract_zonal_connect <- buffs_extract_zonal_connect %>%
  mutate(SiteName = sample_points_f$Site) %>% rename("Buffer 500 (m)" = V1, 
                                                     "Buffer 1000 (m)" = V2, 
                                                     "Buffer 1500 (m)" = V3, 
                                                     "Buffer 2000 (m)" = V4, 
                                                     "Buffer 2500 (m)" = V5, 
                                                     "Buffer 3000 (m)" = V6) %>%
  dplyr::select(SiteName, everything())

buffs_extract_zonal_connect_pivoted <- buffs_extract_zonal_connect %>% 
  pivot_longer(-SiteName, names_to = "Buffer", values_to = "Connectedness") %>% arrange(SiteName) 



# write.csv(buffs_extract_zonal_connect, file = "buffs_extract_zonal_connect.csv")



#Extract the mean value from the Structure tif file for each buffer zone


set.seed(1)
buffs_extract_zonal_structure <- sapply(c(500, 1000, 1500, 2000, 2500, 3000), 
            function(x) { 
              zonal <- raster::extract(structure_cropped, sample_points_f,
                                       fun= mean, buffer = x, na.rm=TRUE)
})
buffs_extract_zonal_structure <- as.data.frame(buffs_extract_zonal_structure)

#Create new field called 'SiteName' using the site names from sample_points_f$Site

buffs_extract_zonal_structure <- buffs_extract_zonal_structure %>% 
  mutate(SiteName = sample_points_f$Site) %>% rename("Buffer 500 (m)" = V1, 
                                                     "Buffer 1000 (m)" = V2, 
                                                     "Buffer 1500 (m)" = V3, 
                                                     "Buffer 2000 (m)" = V4, 
                                                     "Buffer 2500 (m)" = V5, 
                                                     "Buffer 3000 (m)" = V6) %>% 
  dplyr::select(SiteName, everything())


buffs_extract_zonal_structure_pivoted <- buffs_extract_zonal_structure %>%
  pivot_longer(-SiteName, names_to = "Buffer", values_to = "Structure") %>% arrange(SiteName) 



# write.csv(buffs_extract_zonal_structure, file = "buffs_extract_zonal_structure.csv")




#Extract the mean value from the traffic tif file for each buffer zone


set.seed(1)
buffs_extract_zonal_traffic <- sapply(c(500, 1000, 1500, 2000, 2500, 3000),
          function(x) { 
            zonal <- raster::extract(traffic_cropped, sample_points_f, 
                                     fun= mean, buffer = x, na.rm=TRUE)
})
buffs_extract_zonal_traffic <- as.data.frame(buffs_extract_zonal_traffic)

#Create new field called 'SiteName' using the site names from sample_points_f$Site

buffs_extract_zonal_traffic <- buffs_extract_zonal_traffic %>% 
  mutate(SiteName = sample_points_f$Site) %>% rename("Buffer 500 (m)" = V1,  
                                                     "Buffer 1000 (m)" = V2, 
                                                     "Buffer 1500 (m)" = V3, 
                                                     "Buffer 2000 (m)" = V4, 
                                                     "Buffer 2500 (m)" = V5, 
                                                     "Buffer 3000 (m)" = V6) %>% 
  dplyr::select(SiteName, everything())


buffs_extract_zonal_traffic_pivoted <- buffs_extract_zonal_traffic %>% 
  pivot_longer(-SiteName, names_to = "Buffer", values_to = "Traffic") %>% arrange(SiteName) 



#----------------------------------------------------------------------------------------------------------



#Creating the table with Imperv, Connect, Structure, Traffic, NDSI, Anthrophony, and Biophony


Imperv_connect_Struct_Traffic_NDSI <- buffs_extract_zonal_Impervious_pivotted %>% 
  mutate(Connectdness = buffs_extract_zonal_connect_pivoted$Connectedness,
         Structure  = buffs_extract_zonal_structure_pivoted$Structure, 
         Traffic = buffs_extract_zonal_traffic_pivoted$Traffic, 
         NDSI = DF$NDSI_select, Biophony = DF$BIO_select,
         Anthrophony = DF$ANT_select) %>% arrange(SiteName)


write.csv(Imperv_connect_Struct_Traffic_NDSI, 
          file.path(path_out, "Imperv_connect_Struct_Traffic_NDSI.csv"),
          row.names = FALSE)


# ex <- extract(x = imperv_cropped, y = samp_matrix, method='simple', 
#buffer= c(500, 1000, 1500, 2000, 2500, 3000), small=FALSE, cellnumbers=FALSE,
#fun=mean, na.rm=TRUE, df=TRUE)
# 
# 
# buffs_extract <- lapply(c(500, 1000, 1500, 2000, 2500, 3000), function(x) { 
#   set.seed(123)
#   #2a
#   buf_samplesites<- sample_points_f %>% st_buffer(dist = x) # here is the buffer
#   #2c
#   ex <- extract(imperv_cropped, buf_samplesites, fun= mean, na.rm=TRUE)
#   
#   list("sites" = buf_samplesites)
#   list("zonal stats" = ex)
#   #ex <- extract(imperv_cropped, buf_samplesites, fun=mean, na.rm=TRUE, df=TRUE)
# 
# })




# ex <- extract(imperv_cropped, buffs_no_extract, fun=mean(), na.rm=TRUE, df=TRUE)
# 
# 
# plot(buffs)

# 
# 
# #Attempt 1 to plot out the buffer sites using a for loop
# 
# par(mar = c(0, 0, 0, 0))
# # cols <- c("cyan", "blue2", "orange", "purple", "green4", "antiquewhite")  # 2
# 
# # masscounties %>%  st_union() %>%  plot(col = "grey")
# plot(st_geometry(AOI))
# 
# #Plots out buffers using sapply instead of forloop
# #Uses extent of AOI to show rings as they are too granular to be shown on the masscounties map
# 
# par(mar = c(0, 0, 0, 0))
# 
# # masscounties %>% st_union() %>%  plot(col = "grey")
# 
# plot(st_geometry(AOI))
# set.seed(1)
# sapply(1:6, function(x) { 
# plot(st_geometry(buffs_extract[[x]]$sites), add = TRUE, col = NA, pch = 1)
# })
#----------------------------------------------------------------------------------------------------------

# 
# #Manual plot out of the sites to confirm the buffers are working 
# # masscounties %>% st_union() %>%  plot(col = "grey")
# plot(st_geometry(AOI))
# plot(buffs_extract[[6]]$sites, pch = 1, col = NA, add = TRUE)
# plot(buffs_extract[[5]]$sites, pch = 1, col = NA, add = TRUE)
# plot(buffs_extract[[4]]$sites, pch = 1, col = NA, add = TRUE)
# plot(buffs_extract[[3]]$sites, pch = 1, col = NA, add = TRUE)
# plot(buffs_extract[[2]]$sites, pch = 1, col = NA, add = TRUE)
# plot(buffs_extract[[1]]$sites, pch = 1, col = NA, add = TRUE)
# 
# 
# #Oddity, buffers coming in with 
# 
# #1, 3, 5, 9, 12 have values >70
# 
# buff500_firstsite <- buffs_extract[[1]]$sites %>% slice(1)
# buff3000_firstsite <- buffs_extract[[6]]$sites %>% dplyr::slice(1)
# 
# buff500_secondsite <- buffs_extract[[1]]$sites %>% slice(2)
# buff3000_secondsite <- buffs_extract[[6]]$sites %>% dplyr::slice(2)
# 
# buff500_thirdsite <- buffs_extract[[1]]$sites %>% slice(3)
# 
# buff500_fifthsite <- buffs_extract[[1]]$sites %>% slice(5)
# 
# buff500_ninthsite <- buffs_extract[[1]]$sites %>% slice(9)
# 
# 
# 
# 
# #plot out cropped imperv values with selected outlier values
# plot(imperv_cropped, ext = extent(buff500_firstsite))
# buffs_extract[[1]]$sites %>% plot(col = NA, add=TRUE, border = "red")
# plot(imperv_cropped_focal_mean_fixed, main ="Circular MW Mean with function
# speficied", ext = extent(buff500_firstsite))
# buffs_extract[[1]]$sites %>% plot(col = NA, add=TRUE, border = "red")
# 
# 
# 
# 
# plot(imperv_cropped_focal_mean_fixed, main ="Circular MW Mean with function 
# speficied", ext = extent(buff3000_secondsite))
# buffs_extract[[6]]$sites %>% plot(col = NA, add=TRUE, border = "red")
# 
# 
# 
# #Crop imperv_cropped to the first buffer site
# 
# crop_firstsite <- crop(x = imperv_cropped, y = extent(buff500_firstsite))
# 
# 
# 
# 
# plot(imperv_cropped, ext = extent(buff500_secondsite))
# buffs_extract[[1]]$sites %>% plot(col = NA, add=TRUE, border = "red")
# 
# 
# # chirps1_dist72 <- crop(x = chirpsz[[1]], y = districts %>% slice(72))
# 
# 
# 
# plot(imperv_cropped, ext = extent(buff500_thirdsite))
# buffs_extract[[1]]$sites %>% slice(3) %>% plot(col = NA, add=TRUE, 
# border = "red")
# 
# plot(imperv_cropped, ext = extent(buff500_fifthsite))
# buffs_extract[[1]]$sites %>% slice(5) %>% plot(col = NA, add=TRUE, 
# border = "red")
# 
# plot(imperv_cropped, ext = extent(buff500_ninthsite))
# buffs_extract[[1]]$sites %>% slice(9) %>% plot(col = NA, add=TRUE, 
# border = "red")
# 
# buffs_extract[[1]]$sites %>% slice(11) %>% plot(col = NA, add=TRUE, 
# border = "red")
# 
# 
# #Print out plots of buffers
# plot(buffs_extract[[1]]$sites, pch = 1, col = NA)
# 
# 
# 
# imperv_cropped
# 
# 
# 
# plot(imperv_cropped)

# 
# 
# # apply crop, mask and rescale to each layer
# lc_crop <- lapply(landcover, function(x) {  # x <- landcover[[1]]
#   r <- crop(x, e)  # crop to box arpund pts
#   r <- mask(r, ma_crop)  # mask out no data areas
#   (r - cellStats(r, min)) / 255
# })
# lc_stack <- stack(lc_crop)  # stack layers
# 
# # quick look
# plot(lc_stack[[2]])
# plot(st_geometry(pts), add = TRUE)


## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#NDSI vs Impervious
lm_NDSI_Imperv <- lmList(NDSI ~ Imperv | Buffer, 
                         data = Imperv_connect_Struct_Traffic_NDSI, 
                         pool = FALSE)
NDSI_Imperv_lm <- summary(lm_NDSI_Imperv)$r.squared

plot(lm_NDSI_Imperv)
NDSI_Imperv_lm

cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI,
    y = Imperv_connect_Struct_Traffic_NDSI$Imperv)


#NDSI vs Connectedness

lm_NDSI_Connect <- lmList(NDSI ~ Connectdness | Buffer, 
                          data = Imperv_connect_Struct_Traffic_NDSI,
                          pool = FALSE)

NDSI_Connect_lm <-summary(lm_NDSI_Connect)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI, 
    y = Imperv_connect_Struct_Traffic_NDSI$Connectdness)



##NDSI vs Structuredness

lm_NDSI_Structure <- lmList(NDSI ~ Structure | Buffer, 
                            data = Imperv_connect_Struct_Traffic_NDSI, 
                            pool = FALSE)

NDSI_Structure_lm <-summary(lm_NDSI_Structure)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI, 
    y = Imperv_connect_Struct_Traffic_NDSI$Structure)



##NDSI vs Traffic

lm_NDSI_Traffic <- lmList(NDSI ~ Traffic | Buffer, 
                          data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
NDSI_Traffic_lm <-summary(lm_NDSI_Traffic)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI,
    y = Imperv_connect_Struct_Traffic_NDSI$Traffic)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#Anthrophony vs Impervious
lm_ANT_Imperv <- lmList(Anthrophony ~ Imperv | Buffer, 
                        data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
ANT_Imperv_lm <- summary(lm_ANT_Imperv)$r.squared


cor(x = Imperv_connect_Struct_Traffic_NDSI$Anthrophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Imperv)



#Anthrophony vs Connectedness

lm_ANT_Connect <- lmList(Anthrophony ~ Connectdness | Buffer, 
                         data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)

ANT_Connect_lm <-summary(lm_ANT_Connect)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Anthrophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Connectdness)




##Anthrophony vs Structuredness

lm_ANT_Structure <- lmList(Anthrophony ~ Structure | Buffer, 
                           data = Imperv_connect_Struct_Traffic_NDSI, 
                           pool = FALSE)

ANT_Structure_lm <-summary(lm_ANT_Structure)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Anthrophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Structure)


##Anthrophony vs Traffic

lm_ANT_Traffic <- lmList(Anthrophony ~ Traffic | Buffer, 
                         data = Imperv_connect_Struct_Traffic_NDSI, 
                         pool = FALSE)
ANT_Traffic_lm <-summary(lm_ANT_Traffic)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Anthrophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Traffic)

## ----include=FALSE, warning = FALSE, message = FALSE--------------------------
#Biophony vs Impervious

lm_BIO_Imperv <- lmList(Biophony ~ Imperv | Buffer, 
                        data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
BIO_Imperv_lm <- summary(lm_BIO_Imperv)$r.squared

lm_BIO_Imperv
BIO_Imperv_lm

cor(x = Imperv_connect_Struct_Traffic_NDSI$Biophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Imperv)


#Biophony vs Connectedness

lm_BIO_Connect <- lmList(Biophony ~ Connectdness | Buffer, 
                         data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
BIO_Connect_lm <-summary(lm_BIO_Connect)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Biophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Connectdness)


##Biophony vs Structuredness

lm_BIO_Structure <- lmList(Biophony ~ Structure | Buffer, 
                           data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
BIO_Structure_lm <-summary(lm_BIO_Structure)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Biophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Structure)



##Biophony vs Traffic

lm_BIO_Traffic <- lmList(Biophony ~ Traffic | Buffer, 
                         data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
BIO_Traffic_lm <-summary(lm_BIO_Traffic)$r.squared

cor(x = Imperv_connect_Struct_Traffic_NDSI$Biophony, 
    y = Imperv_connect_Struct_Traffic_NDSI$Traffic)

## ---- warning = FALSE, message = FALSE----------------------------------------
# as. Buffer <- Imperv_connect_Struct_Traffic_NDSI$Buffer

Buffer <- c(500, 1000,1500,2000,2500,3000)

linear_rsquared <- data.frame(Buffer, NDSI_Imperv_lm, NDSI_Connect_lm, 
                              NDSI_Structure_lm, NDSI_Traffic_lm, ANT_Imperv_lm,
                              ANT_Connect_lm, ANT_Structure_lm, ANT_Traffic_lm,
                              BIO_Imperv_lm, BIO_Connect_lm, BIO_Structure_lm,
                              BIO_Traffic_lm)



# write.csv(linear_rsquared, file = "linear_rsquared_traffic_included.csv")


#-------------------------------------------------------------------------------------------------------------------------
#Graphs


#graph of R^2 for NDSI


RSquaredPlot_NDSI <- ggplot(data = linear_rsquared) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$NDSI_Imperv_lm,
                color = 'Imperv')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$NDSI_Structure_lm, 
                color = 'Structure')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$NDSI_Connect_lm, 
                color = 'Connect')) +
 geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$NDSI_Traffic_lm, 
               color = 'Traffic')) + scale_x_log10() + 
  labs(title = "Mean NDSI ~ R squared of metrics", x ="Focal Distance (meters)", 
       y = "R squared of metrics") 

plot(RSquaredPlot_NDSI)
plotly::ggplotly(RSquaredPlot_NDSI)




#graph of R^2 for ANT


RSquaredPlot_ANT <- ggplot(data = linear_rsquared) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$ANT_Imperv_lm, 
                color = 'Imperv')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$ANT_Structure_lm, 
                color = 'Structure')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$ANT_Connect_lm, 
                color = 'Connect')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$ANT_Traffic_lm, 
                color = 'Traffic')) + scale_x_log10() + 
  labs(title = "Mean ANT ~ R squared of metrics", x ="Focal Distance (meters)", 
       y = "R squared of metrics") 

plot(RSquaredPlot_ANT)
plotly::ggplotly(RSquaredPlot_ANT)




#graph of R^2 for BIO


RSquaredPlot_Bio <- ggplot(data = linear_rsquared) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$BIO_Imperv_lm, 
                color = 'Imperv')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$BIO_Structure_lm,
                color = 'Structure')) +
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$BIO_Connect_lm,
                color = 'Connect')) + 
  geom_line(aes(x = linear_rsquared$Buffer, y = linear_rsquared$BIO_Traffic_lm,
                color = 'Traffic')) + scale_x_log10() + 
  labs(title = "Mean BIO ~ R squared of metrics", x ="Focal Distance (meters)", 
       y = "R squared of metrics") 

plot(RSquaredPlot_Bio)
plotly::ggplotly(RSquaredPlot_Bio)



