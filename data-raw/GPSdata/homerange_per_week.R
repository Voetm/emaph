library(sp)
library(rgdal)
library(rgeos)
library(geosphere)
library(lubridate)
library(adehabitatHR)
library(emaph)
library(tidyverse)

d <- subset(GPSdata, TimeStamp >= "2017-01-01" & TimeStamp <= "2017-07-01")


d <- d %>% group_by(round_date(TimeStamp, unit = "10 minutes")) %>%
  summarise(lon = mean(lon, na.rm = TRUE), lat = mean(lat, na.rm = TRUE))
names(d)[1] = "TimeStamp"

d$group <- factor(sprintf("%02d", week(d$TimeStamp)))


#d <- unique(d[c("lon", "lat", "group")])
N <- table(d$group)


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
#plot(d$lon, d$lat)
#plot(xy)
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


df.points <- as.data.frame(xy)
names(df.points) <- c("id", "long", "lat")


# all in one
g <- ggplot(df1, aes(x = long, y = lat, group = group)) +
  geom_polygon(alpha = .2, aes(fill = id, color = id), size = 1) +
  geom_point(data = df.points, aes(group = NULL), alpha = .01) +
  coord_equal() +
  scale_fill_manual(values = colorRampPalette(ggthemes::solarized_pal()(8))(26)) +
  scale_color_manual(values = colorRampPalette(ggthemes::solarized_pal()(8))(26)) +
  theme_void(18)

g

# facetted
g +
  facet_wrap(~id, ncol = 9) + guides(colour = FALSE) + guides(fill = FALSE)


#xlim(615000, 635000) + ylim(5790000, 5820000)



# AREA -------------------------------------------------------------------------

v <- getverticeshr(kd, 90)
vd <- as.data.frame(v)

vd$N <- as.numeric(table(d$group))
vd$t <- 1:nrow(vd)


ggplot(data = vd, aes(x = t, y = area)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Week", y = "HomeRange Area") +
  theme_bw(14)

plot(N, cex = 4)

fm = lm(area ~ I(scale(N, scale = FALSE)), data = vd)
summary(fm)

ggplot(data = vd, aes(x = t, y = resid(fm))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Week", y = "HomeRange Area (corrected)") +
  theme_bw(14)



fm2 = lm(resid(fm) ~ t, data = vd)
summary(fm2)






