library(tidyverse)
library(osmdata)
library(raster)
library(ggplot2)
library(rgdal)
library(sf)
library(landscapetools)


ext = extent(c(-75.42, -75.27, 37.83544, 37.97))

place = 'Chincoteague Virginia'

streets <- getbb(place) %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c(available_tags("highway"))) %>%
  osmdata_sf()
streets

streets <- getbb(place)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb(place)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service")) %>%
  osmdata_sf()

# streets.w <- getbb("Wallops Island Virginia") %>%
#   opq() %>%
#   add_osm_feature(key = "highway",
#                   value = c(available_tags("highway"))) %>%
#   osmdata_sf()
# streets.w



# # Get Landsat8 images for bands 1-4 (or 3-4 for NDVI)
# get1= 'wget https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/014/034/LC08_L1TP_014034_20191024_20191030_01_T1/LC08_L1TP_014034_20191024_20191030_01_T1_B1.TIF'
# get2= 'wget https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/014/034/LC08_L1TP_014034_20191024_20191030_01_T1/LC08_L1TP_014034_20191024_20191030_01_T1_B2.TIF'
# get3= 'wget https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/014/034/LC08_L1TP_014034_20191024_20191030_01_T1/LC08_L1TP_014034_20191024_20191030_01_T1_B3.TIF'
# get4= 'wget https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/014/034/LC08_L1TP_014034_20191024_20191030_01_T1/LC08_L1TP_014034_20191024_20191030_01_T1_B4.TIF'
get5= 'wget https://s3-us-west-2.amazonaws.com/landsat-pds/c1/L8/014/034/LC08_L1TP_014034_20191024_20191030_01_T1/LC08_L1TP_014034_20191024_20191030_01_T1_B5.TIF'

# 
# 
# system(get1)
# system(get2)
# system(get3)
# system(get4)
system(get5)

l8 = raster::stack(list.files(pattern='*TIF', full.names = TRUE))


crs.streets = crs(streets$osm_lines)

l8.repro <- projectRaster(l8, crs=crs.streets)

ls.ex = crop(l8.repro, ext)
l8_df = raster::as.data.frame(ls.ex, xy = TRUE)
head(l8_df)

NDVI = (ls.ex[[5]] - ls.ex[[4]])/(ls.ex[[5]] + ls.ex[[4]])
ndvi_df = as.data.frame(NDVI, xy=T)
## ggplot 

ggplot() +
  geom_raster(data = ndvi_df, 
              aes(x = x, 
                  y = y, 
                  fill = layer
                  ), interpolate=T
              ) +
  scale_fill_gradientn(
    colours=c('white', 'darkred'),
    na.value = "black"
    )+
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "black",
    size = .4,
    alpha = .8
  ) + 
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = "goldenrod",
    size = .4,
    alpha = .8
  ) + 
  coord_sf(
    xlim = c(ext[1], ext[2]),
    ylim = c(ext[3], ext[4]),
    expand = FALSE
  ) + 
  theme_void()




# Classify landsat raster
n=3
classified_landscape <- util_classify(NDVI,
                                      n = n
                                      )


classif_disag = disaggregate(classified_landscape, fact=c(4,4))
classif_res = resample(classified_landscape, classif_disag, method='ngb')
#try focal here:focal(x, w=matrix(1, 5, 5), mean)



classif_df = as.data.frame(classif_res, xy=TRUE)

#classif_poly = rasterToPolygons(classified_landscape>2, 
             #                   fun=NULL, n=16, na.rm=TRUE, digits=8, dissolve=T)

#cpoly_df = as.data.frame(classif_poly2, xy=TRUE)

#sf_poly <- as(classif_poly, "sf")
#sf::st_crs(sf_poly) <- 4326
#sf_poly$id <- c(1,2)
#(poly_plot = ggplot(sf_poly) +
 # geom_sf(aes(fill = as.factor(id)), lwd=0) +
  #scale_fill_manual(values=c('black', 'gray95')) +
  
(poly_plot = ggplot() +
    geom_raster(data=classif_df, 
                aes(x=x, y=y, fill=layer),
                interpolate=T
              
                ) +
    scale_fill_gradientn(
       colours=c('black', 'gray90', 'steelblue'),
       na.value = "black"
     ) +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "goldenrod",
    size = .5,
    alpha = .8
  ) + 
    geom_sf(
      data = small_streets$osm_lines,
      inherit.aes = FALSE,
      color = "goldenrod",
      size = .3,
      alpha = .8
    ) + 
  coord_sf(
    xlim = c(ext[1], ext[2]),
    ylim = c(ext[3], ext[4]),
    expand = FALSE
  ) + theme_void() + 
  theme(legend.position = "none") 
)
ggsave(poly_plot, filename='map_poly.png', height = 9, width = 9, dpi=400, units='in')


# RGB realcolor plot


#Scale to 0-255 range for display device
rgb_ras <- stretch(x=ls.ex, minv=0, maxv=255)

rgb_df = raster::as.data.frame(rgb_ras, xy = TRUE)

colnames(rgb_df)= c('x','y','B1','B2','B3','B4','B5')
colorvec = rgb(rgb_df$B4, rgb_df$B3, rgb_df$B2, maxColorValue = 255)

ggplot() +
  geom_raster(data = l8_df, 
              aes(x = x, 
                  y = y, 
                  fill = colorvec
              ), interpolate=T
  ) + scale_fill_identity() +
  geom_sf(
    data = streets$osm_lines,
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
  theme_void()

