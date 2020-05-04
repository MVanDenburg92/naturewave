## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
#[1]

library(ggplot2)

rsquared_old <- read.csv("../inst/extdata/linear_rsquared_old.csv")

RSquaredPlot_NDVI <- ggplot(data = rsquared_old) + 
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$ACI_NDVI_lm, color = 'ACI')) +
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$ADI_NDVI_lm, color = 'ADI')) +
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$AEI_NDVI_lm, color = 'AEI')) +
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$BI_NDVI_lm, color = 'BI')) +
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$NDSI_NDVI_lm, color = 'NDSI'))+
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$BIO_NDVI_lm, color = 'BIOPHONY'))+
  geom_line(aes(x=rsquared_old$buffername, y=rsquared_old$ANT_NDVI_lm, color = 'ANTHRO')) +  
  labs(title = "Mean NDVI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics") 

plotly::ggplotly(RSquaredPlot_NDVI)



## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
#[2]
# #NDSI vs Impervious
# lm_NDSI_Imperv <- lmList(NDSI ~ Imperv | Buffer, 
#                          data = Imperv_connect_Struct_Traffic_NDSI, 
#                          pool = FALSE)
# NDSI_Imperv_lm <- summary(lm_NDSI_Imperv)$r.squared
# 
# plot(lm_NDSI_Imperv)
# NDSI_Imperv_lm
# 
# cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI,
#     y = Imperv_connect_Struct_Traffic_NDSI$Imperv)
# 
# 
# #NDSI vs Connectedness
# 
# lm_NDSI_Connect <- lmList(NDSI ~ Connectdness | Buffer, 
#                           data = Imperv_connect_Struct_Traffic_NDSI,
#                           pool = FALSE)
# 
# NDSI_Connect_lm <-summary(lm_NDSI_Connect)$r.squared
# 
# cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI, 
#     y = Imperv_connect_Struct_Traffic_NDSI$Connectdness)
# 
# 
# 
# ##NDSI vs Structuredness
# 
# lm_NDSI_Structure <- lmList(NDSI ~ Structure | Buffer, 
#                             data = Imperv_connect_Struct_Traffic_NDSI, 
#                             pool = FALSE)
# 
# NDSI_Structure_lm <-summary(lm_NDSI_Structure)$r.squared
# 
# cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI, 
#     y = Imperv_connect_Struct_Traffic_NDSI$Structure)
# 
# 
# 
# ##NDSI vs Traffic
# 
# lm_NDSI_Traffic <- lmList(NDSI ~ Traffic | Buffer, 
#                           data = Imperv_connect_Struct_Traffic_NDSI, pool = FALSE)
# NDSI_Traffic_lm <-summary(lm_NDSI_Traffic)$r.squared
# 
# cor(x = Imperv_connect_Struct_Traffic_NDSI$NDSI,
#     y = Imperv_connect_Struct_Traffic_NDSI$Traffic)



## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 8, fig.align = 'center'----
#[3]

library(raster)
library(maps)
library(dplyr)
library(sf)

#path to read in files
path_in <- '../inst/extdata'

# ##Runs focal at a buffer weight of 2500 m radius
# 
# radius3 <- 2500
# lc2500 <- lapply(1:4, function(x) {  # x <- 1
#   f <- paste0("../external/data/Amherst_CAPS2011", names(lc_stack)[x], "_", radius3, ".tif")
#   r <- focal(lc_stack[[x]], w = as.matrix(fw_sum_2500[[x]]), filename = f, overwrite = TRUE)
#   return(r)
# })
# lc2500_stacked <- stack(lc2500)  # Stack

#Read in files post focal weights 
Imperv <- raster("../inst/extdata/lc2500_imperv.tif")
Connectedness <- raster("../inst/extdata/lc2500_connect.tif")
Structure <- raster("../inst/extdata/lc2500_structure.tif")
Traffic <- raster("../inst/extdata/lc2500_traffic.tif")

# Create a raster stack
landcover_ls <- list(Imperv, Connectedness, Structure, Traffic)
names(landcover_ls) <- c("Imperv", "Connectedness", "Structure", "Traffic")

my_brick <- brick(landcover_ls)


# b <- brick(file.path(path_in, "lc2500_brick.tif"))  # correct, reads in whole brick

# mass <- st_as_sf(map(database = "state", plot = FALSE, fill = TRUE)) %>% lwgeom::st_make_valid() %>% filter(ID == "massachusetts")

# par(mfrow = c(2, 2), mar = c(.5, 0, 2, 6))
plot(my_brick, axes = FALSE, box = FALSE)


## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
#[4]
DS <- read.csv('../inst/extdata/Imperv_connect_Struct_Traffic_NDSI.csv', header = T) 


# Filter values using 2500 as the scale of best influence
DS_2500 <- DS %>% dplyr::filter(Buffer == "Buffer 2500 (m)") %>%
  dplyr::select(-c(Buffer, SiteName, Biophony, Anthrophony)) %>% rename("Connectedness" = Connectdness)

# Scale variable values except NDSI values 
DS_2500_scaled <- DS_2500[,1:4]/255

# Use mutate to add NDSI values back to the table. 
DS_2500_scaled <- DS_2500_scaled %>% mutate("NDSI" = DS_2500$NDSI)

# Create training and testing samples
sample = caTools::sample.split(DS_2500$NDSI, SplitRatio = .75)
train = subset(DS_2500, sample == TRUE)
test  = subset(DS_2500, sample == FALSE)

# Check out the dimensions of each sample set
dim(train)
dim(test)

# Create RandomForest Model
set.seed(100)
rf_2500 <- randomForest::randomForest(NDSI ~., data=DS_2500_scaled, mtry= 2, importance = TRUE, na.action=na.omit)

print(rf_2500)


## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
#[5]

library(ggplot2)

linear_rsquared_new <- read.csv("../inst/extdata/linear_rsquared_traffic_included.csv")

RSquaredPlot_NDSI <- ggplot(data = linear_rsquared_new) +
  geom_line(aes(x = linear_rsquared_new$Buffer, y = linear_rsquared_new$NDSI_Imperv_lm,
                color = 'Imperv')) +
  geom_line(aes(x = linear_rsquared_new$Buffer, y = linear_rsquared_new$NDSI_Structure_lm,
                color = 'Structure')) +
  geom_line(aes(x = linear_rsquared_new$Buffer, y = linear_rsquared_new$NDSI_Connect_lm,
                color = 'Connect')) +
 geom_line(aes(x = linear_rsquared_new$Buffer, y = linear_rsquared_new$NDSI_Traffic_lm,
               color = 'Traffic')) + scale_x_log10() +
  labs(title = "Mean NDSI ~ R squared of metrics", x ="Focal Distance (meters)",
       y = "R squared of metrics")

plotly::ggplotly(RSquaredPlot_NDSI)

## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
#[6]
# Show "importance" of variables: higher value means more important:
round(randomForest::importance(rf_2500), 2)

# Visualize Variable Importance Plot
randomForest::varImpPlot(rf_2500, main = "Variable Importance at 2500 buffer level")

## ---- error = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center'----
library(leaflet)
#[7]
# Use raster::predict to predict NDSI values across the landscape
ndsi_pred <- raster::predict(object = my_brick, model=rf_2500, type = 'response', index = 5)
ndsi_pred

# Display NDSI prediction layer with sample locations. First, set the CRS for the sampling locations to match the one from the raster image

sites <- st_read(dsn = "../inst/extdata/sample_sites_projected.shp")
sites <- st_transform(x = sites, crs = st_crs(ndsi_pred))

# Plot variables and predicted NDSI raster image

# Rename ndsi_pred
names(ndsi_pred) <- c("NDSI_Prediction")



#Code below creates an interactive leaflet map with clickable sites

pal2 <- colorNumeric(c("#F2F2F2FF","#EEB99FFF", "#EAB64EFF","#E6E600FF","#00A600FF"), values(ndsi_pred), na.color = "transparent")
pal3 <- colorNumeric(c("#00A600FF","#E6E600FF","#EAB64EFF","#EEB99FFF","#F2F2F2FF"), values(ndsi_pred), na.color = "transparent")

leaflet() %>% addTiles() %>% addCircleMarkers(lng = sites$Long, lat = sites$Lat, weight = 1, popup = sites$Site) %>% 
  addRasterImage(ndsi_pred, colors = pal2, opacity = 0.8) %>%
  addLegend(pal = pal3, values = values(ndsi_pred), labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),title = "Predicted NDSI Values")


