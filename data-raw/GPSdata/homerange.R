library(sp)
library(rgdal)
library(rgeos)
library(geosphere)
library(lubridate)
library(adehabitatHR)
library(emaph)
library(tidyverse)

d <- subset(GPSdata, TimeStamp >= "2017-02-01" & TimeStamp <= "2017-06-01")
d$group <- factor(weekdays(d$TimeStamp, abbreviate = TRUE),
                  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                  ordered = TRUE)
#d <- unique(d[c("lon", "lat", "group")])

xy <- SpatialPointsDataFrame(
  as.data.frame(d[c("lon", "lat")]),
  data.frame(ID = d$group),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

xy <- spTransform(
  xy,
  CRS("+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"))

c <- as.data.frame(coordinates(xy))
xy <- xy[c$lon > 620000 & c$lon < 630000 &
         c$lat > 5790000 & c$lat < 5815000,   ]

bbox(xy)
plot(d$lon, d$lat)
plot(xy)
ggplot(data = as.data.frame(xy), aes(lon, lat)) +
  geom_point() + theme_void(18) +
  facet_wrap(~ ID)


# create custom grid
resolution = 100
long <- seq(600000,650000, by = resolution)
lat <- seq(5770000,5835000, by = resolution)
ab <- expand.grid(long = long,lat=lat)
coordinates(ab) <- ~long + lat
gridded(ab) <- TRUE
class(ab)


#a = kernelUD(xy, grid = ab,
#             h = "href", kern = "bivnorm")
#image(a)
#getverticeshr(a[[1]], percent = 90)
library(ggplot2)

kd <- kernelUD(xy, grid = ab, h = "href", kern = "bivnorm")

get_vertices <- function(xy, kd, grid, percent) {
  kd_names <- names(kd)
  ud <- lapply(kd, function(x) try(getverticeshr(x, percent)))
  # changing each polygons id to the species name for rbind call
  sapply(1:length(ud), function(i) {
    row.names(ud[[i]]) <<- kd_names[i]
  })
  sdf_poly <- Reduce(rbind, ud)
  df <- fortify(sdf_poly)
}
df1 = get_vertices(xy, kd = kd, grid = ab, 95)

df1$id = factor(df1$id,
                   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                   ordered = TRUE)

df.points <- as.data.frame(xy)
names(df.points) <- c("id", "long", "lat")

# all in one
g <- ggplot(df1, aes(x = long, y = lat, group = group)) +
  geom_polygon(alpha = .2, aes(fill = id, color = id), size = 1) +
  geom_point(data = df.points, aes(group = NULL), alpha = .01) +
  coord_equal() + ggthemes::scale_fill_gdocs() + ggthemes::scale_color_gdocs() +
  theme_void(18)

g + theme_void(18)

# facetted
g + theme_void(18) +
  facet_wrap(~id, ncol = 5) + guides(colour = "none")


#xlim(615000, 635000) + ylim(5790000, 5820000)




v <- getverticeshr(kd, 80)
vd <- as.data.frame(v)

ggplot(data = vd, aes(group = id)) +
  geom_bar(aes(weight = area))



