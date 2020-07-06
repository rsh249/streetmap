library(tidyverse)
library(osmdata)
library(raster)
library(ggplot2)
library(rgdal)
library(sf)
library(landscapetools)
library(knor)
library(RStoolbox)


# Get Open Street Map (OSM) Data
place = 'Chincoteague Virginia' # EDIT if you are searching for another place

streets <- getbb(place) %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c(available_tags("highway"))) %>%
  osmdata_sf()
streets

streets <- getbb(place)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c( "primary", 
                             "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb(place)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service")) %>%
  osmdata_sf()

######## END OSM ################



#define bounding area from streets object
box = unlist(str_split(streets$bbox, ","))
box = as.numeric(box)
lat_min = box[1]
lon_min = box[2]
lat_max = box[3]
lon_max = box[4]
ext = extent(c(lon_min, lon_max, lat_min, lat_max)) # EDIT if you are searching for another place



# # Get Landsat8 images for bands 1-4 (or 3-4 for NDVI)

# modify for another location
# learn more about these parameters here:https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/landsat-data-in-r-geotiff/
# and also here:https://docs.opendata.aws/landsat-pds/readme.html 

download.file('https://landsat-pds.s3.amazonaws.com/c1/L8/scene_list.gz', 'data/scene_list.gz')
system('gunzip data/scene_list.gz')

scene_list = data.table::fread('data/scene_list', 
                               sep = ',', 
                               colClasses = c('character',
                                              'character',
                                              'character',
                                              'numeric',
                                              'character',
                                              'character', 
                                              'character',
                                              'numeric',
                                              'numeric',
                                              'numeric',
                                              'numeric',
                                              'character'))

aoi= scene_list %>% filter(min_lon <= lon_min &
                             min_lat <= lat_min &
                             max_lon >= lon_max &
                             max_lat >= lat_max) %>% # filter for paths and rows covering study area
  filter(cloudCover < 5) %>% # Get cloud cover < 10%
  filter(acquisitionDate == max(acquisitionDate))  %>% # get most recent image
  slice(1) %>% # keep only one row
  mutate(path = str_pad(path, 3, pad='0')) %>%
  mutate(row = str_pad(row, 3, pad='0'))

download_base = paste('https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/', aoi$path, "/", aoi$row, "/", aoi$productId, "/", aoi$productId, sep='')

get4 = paste( download_base, '_B4.TIF', sep = '')
get5 = paste( download_base, '_B5.TIF', sep = '')

download.file(get4, 'B4.TIF')
download.file(get5, 'B5.TIF')


l8 = raster::stack(list.files(pattern='*TIF', full.names = TRUE))

### END get LANDSAT ###


# begin processing
crs.streets = crs(streets$osm_lines)

l8.repro <- projectRaster(l8, crs=crs.streets)

ls.ex = crop(l8.repro, ext)
l8_df = raster::as.data.frame(ls.ex, xy = TRUE)
head(l8_df)

NDVI = (ls.ex[[2]] - ls.ex[[1]])/(ls.ex[[2]] + ls.ex[[1]])

#try focal here:focal(x, w=matrix(1, 5, 5), mean)

#smooth NDVI raster to avoid noise
NDVI.d = disaggregate(NDVI, fact=c(8,8))
NDVI.f = focal(NDVI.d, w=matrix(1,7,7), mean)
NDVI.f = focal(NDVI.f, w=matrix(1,5,5), mean)
NDVI.f = focal(NDVI.f, w=matrix(1,3,3), mean)

NDVI.uc <- unsuperClass(NDVI.f, nClasses = 3)

ndvi_df = as.data.frame(NDVI.uc$map, xy=T)

## ggplot 

(ndvi.cat = ggplot() +
    geom_raster(data = ndvi_df, 
                aes(x = x, 
                    y = y, 
                    fill = layer
                ), interpolate=T, show.legend=F
    ) +
    scale_fill_gradientn(
      colours=c('black', 'darkblue', 'steelblue'),
      na.value = "black"
    )+
    geom_sf(
      data = streets$osm_lines,
      inherit.aes = FALSE,
      color = "white",
      size = .4,
      alpha = .8
    ) + 
    geom_sf(
      data = small_streets$osm_lines,
      inherit.aes = FALSE,
      color = "white",
      size = .4,
      alpha = .8
    ) + 
    coord_sf(
      xlim = c(ext[1], ext[2]),
      ylim = c(ext[3], ext[4]),
      expand = FALSE
    ) + 
    theme_void()) + 
  theme(legend.position = "none")

ggsave(ndvi.cat , filename='map_cat.png', width=9, height=9, dpi=900, units='in')

### END ###


### Notes for later:
# # RGB realcolor plot
# 
# 
# #Scale to 0-255 range for display device
# rgb_ras <- stretch(x=ls.ex[[2:4]], minv=0, maxv=255)
# 
# rgb_df = raster::as.data.frame(rgb_ras, xy = TRUE)
# 
# colnames(rgb_df)= c('x','y', 'B2','B3','B4')
# colorvec = rgb(rgb_df$B4, rgb_df$B3, rgb_df$B2, maxColorValue = 255)
# 
# ggplot() +
#   geom_raster(data = l8_df, 
#               aes(x = x, 
#                   y = y, 
#                   fill = colorvec
#               ), interpolate=T
#   ) + scale_fill_identity() +
#   geom_sf(
#     data = streets$osm_lines,
#     inherit.aes = FALSE,
#     color = "white",
#     size = .4,
#     alpha = .8
#   ) + 
#   coord_sf(
#     xlim = c(ext[1], ext[2]),
#     ylim = c(ext[3], ext[4]),
#     expand = FALSE
#   ) + 
#   theme_void()
# 
