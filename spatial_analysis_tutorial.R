library(maptools)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(sf)

###BASICS

#get info about raster data before reading data into R
GDALinfo("2009586/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

#store info from above
harv_dsmCrop_info <- capture.output(
  GDALinfo("2009586/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")
)

#load raster file
DSM_Harv <- raster("2009586/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

#convert to dataframe to use in ggplot2
DSM_Harv_df <- as.data.frame(DSM_Harv, xy=TRUE)

#view data structure
str(DSM_Harv_df)

#ggplot
ggplot() +
  geom_raster(data = DSM_Harv_df , aes(x = x, y = y, fill = HARV_dsmCrop)) +
  scale_fill_viridis_c() +
  coord_quickmap()

#simpler plot (does not require df)
plot(DSM_Harv)

#view CRS string assoc with R object in proj4 format
crs(DSM_Harv)


#calculate raster min and max values
#raster stats calculated and embedded in GeoTIFF

#viewing values
minValue(DSM_Harv)
maxValue(DSM_Harv)

#if min and max not calculated
DSM_Harv <- setMinMax(DSM_Harv)

#how many bands in this raster
nlayers(DSM_Harv)


###Manipulate Raster Data in R

#read in shape file. area of interest = (AOI)
aoi_boundary_HARV <- st_read("2009586/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

#load in raster
CHM_HARV <- raster("2009586/NEON-DS-Airborne-Remote-Sensing/HARV/CHM/HARV_chmCrop.tif")

#convert to df
CHM_HARV_df <- as.data.frame(CHM_HARV, xy=TRUE)

#visualize area of CHM we want to subset
ggplot() +
  geom_raster(data = CHM_HARV_df, aes(x = x, y = y, fill = HARV_chmCrop)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) +
  geom_sf(data = aoi_boundary_HARV, color = "blue", fill = NA) +
  coord_sf()

#convert aoi_boundary_HARV from sf to "Spatial" object so crop() function works
CHM_HARV_Cropped <- crop(x=CHM_HARV, y=as(aoi_boundary_HARV, "Spatial"))

#convert CHM_HARV_Cropped into df for ggplot
CHM_HARV_Cropped_df <- as.data.frame(CHM_HARV_Cropped, xy=TRUE)

#st_bbox extracts 4 corners of rectangle
#st_as_sfc converts 4 coords into plottable polygon
#code throws error "polygon edge not found"
ggplot() +
  geom_sf(data = st_as_sfc(st_bbox(CHM_HARV)), fill = "green",
          color = "green", alpha = .2) +  
  geom_raster(data = CHM_HARV_Cropped_df,
              aes(x = x, y = y, fill = HARV_chmCrop)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_sf()


###Extracting Raster Pixel Values Using Vector Polygons

#x = raster, y = vector layer containing polygon to use as boundary
#store output values in df. default is list
tree_height <- extract(x = CHM_HARV,
                       y = as(aoi_boundary_HARV, "Spatial"),
                       df = TRUE)

#view structure
str(tree_height)

#create histogram to see height distribution at site
ggplot() + 
  geom_histogram(data = tree_height, aes(x = HARV_chmCrop)) +
  ggtitle("Histogram of CHM Height Values (m)") +
  xlab("Tree Height") + 
  ylab("Frequency of Pixels")

#view descriptive stats
summary(tree_height$HARV_chmCrop)


#Summarive Extracted Raster Values
#tell R summary stat of interest using (fun =) argument

#example = mean height value for AOI
#not using df=TRUE because extracting a single number
mean_tree_height_AOI <- extract(x = CHM_HARV,
                                y = as(aoi_boundary_HARV, "Spatial"),
                                fun = mean)

mean_tree_height_AOI


#Extract Data Using x,y Locations
#extract() can be used to extract pixel values by defining area around indiv point locations

#define summary arg (fun = mean) and buffer distance (buffer = 20)
#buffer represents radius of circular region around point
#default buffer units are same as data's CRS

#from prev lesson need for extract
point_HARV <- st_read("2009586/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

mean_tree_height_tower <- extract(x = CHM_HARV,
                                  y = as(point_HARV, "Spatial"),
                                  buffer = 20,
                                  fun = mean)
#view data
mean_tree_height_tower


##Challenge: Extract Raster Height Values For Plot Locations

#read in plot locations data
plot_locations_HARV <- read.csv("2009586/NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv")

#check structure
str(plot_locations_HARV)

#create crs object we can use to define CRS of sf object when we create it
utm18nCRS <- st_crs(point_HARV)


#convert df (csv) to sf object
#must specify cols containing X and Y values
#must specify CRS
plot_locations_sp_HARV <- st_as_sf(plot_locations_HARV, coords = c("easting", "northing"), crs = utm18nCRS)


# extract data at each plot location. df=TRUE because multiple numbers extracted
mean_tree_height_plots_HARV <- extract(x = CHM_HARV,
                                       y = as(plot_locations_sp_HARV, "Spatial"),
                                       buffer=20,
                                       fun = mean,
                                       df = TRUE)

# view data
mean_tree_height_plots_HARV

# plot data
ggplot(data = mean_tree_height_plots_HARV, aes(ID, HARV_chmCrop)) + 
  geom_col() + 
  ggtitle("Mean Tree Height at each Plot") + 
  xlab("Plot ID") + 
  ylab("Tree Height (m)")











