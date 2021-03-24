library(tidyverse)
library(raster)
library(mapdata)
library(marmap)

chl_raster = raster('data/A20021822017212.L3m_MC_CHL_chlor_a_9km.nc')
class(chl_raster)
chl_raster

names(chl_raster) = "chl_a"

chl_pts = raster::rasterToPoints(chl_raster, spatial=TRUE)
class(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df) # mg/m^3

## plot it
global_chl_map = ggplot() + 
  geom_raster(data=chl_df, aes(x=x, y=y, fill=log10(chl_a))) + 
  theme_classic() + 
  scale_fill_gradientn(colors=cols, limits=c(-1.5, 0.75), name="log_10(chl_a)")

global_chl_map
hist(log10(chl_df$chl_a))
cols = rainbow(7, rev=TRUE)[-1]

ggsave(global_chl_map, filename="figures/happy_chl.pdf", device="pdf",
       height = 5, width=9)

# Gulf of Maine
lon = c(-72, -62)
lat = c(39, 47)

chl_GOM_raster = raster::crop(chl_raster, extent(c(lon, lat)))
chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial=TRUE))

world_map = map_data("worldHires")

GOM_chl_map = ggplot() + 
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a))) + # imaged chl
  theme_bw() +
  scale_fill_gradientn(colors=cols, limits=c(-1, 1.75)) +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group)) +  # added coastline
  coord_fixed(xlim=lon, ylim=lat, expand=FALSE) # didnt finish


## NOAA bathymetry data

## Gulf of Maine
lon = c(-72, -62)
lat = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1=-72, lon2=-62, lat1 = lat[1], lat2=lat[2],
                                   resolution=4)
class(bath_m_raw)
bath_m_df = marmap::fortify.bathy(bath_m_raw)
class(bath_m_df)
head(bath_m_df)

bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>0, NA, z))

GOM_bath_map = ggplot() + 
  geom_raster(data=bath_m, aes(x=x, y=y, fill=depth_m)) + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group)) + 
  coord_fixed(ratio=1.3, xlim=lon, ylim=lat, expand=FALSE)

