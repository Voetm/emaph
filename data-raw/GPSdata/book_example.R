library(sp)
library(rgdal)
library(rgeos)
library(geosphere)
library(lubridate)

library(emaph)
library(tidyverse)

library(adehabitatHR)
library(lubridate)

# Analyse week 4 data only;
d <- subset(GPSdata, month(TimeStamp) == 3)

# downsample to 30-minute frequency
d$TimeStamp <- round_date(TimeStamp, unit = "5 minutes")
d <- d %>%
  group_by(TimeStamp) %>%
  summarise(lon = mean(lon),
            lat = mean(lat))

# create SpationalPoint object
xy <- SpatialPoints(
  as.data.frame(d[c("lon", "lat")]),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Transform to UTM, so that x and y coordinates are in meters
xy <- spTransform(
  xy,
  CRS("+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"))


kd <- kernelUD(xy, grid = 100, h = "href", kern = "epa")
v <- getverticeshr(kd, 95)

# all in one
vd <- fortify(v)
ggplot(data = vd, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "red", size = 1, alpha = .2) +
  geom_point(data = unique(as.data.frame(coordinates(xy))),
             aes(x = lon, y = lat, group = NULL),
             alpha = .01) +
  coord_equal() + theme_void(14)

