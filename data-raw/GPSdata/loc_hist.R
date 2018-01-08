# https://www.r-bloggers.com/how-to-map-your-google-location-history-with-r/


# import data
library(jsonlite)
system.time(x <- fromJSON("loc_jr_oct_2017.json"))

# extracting the locations dataframe
loc = x$locations

# converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7


# descriptives
nrow(loc)
min(loc$time)
max(loc$time)

# calculate the number of data points per day, month and year
library(lubridate)
library(zoo)

loc$date <- as.Date(loc$time, '%Y/%m/%d')
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

points_p_day <- data.frame(table(loc$date), group = "day")
points_p_month <- data.frame(table(loc$month_year), group = "month")
points_p_year <- data.frame(table(loc$year), group = "year")

# set up plotting theme
library(ggplot2)
library(ggmap)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "right",
      legend.background = element_blank(),
      panel.margin = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}
points <- rbind(points_p_day[, -1], points_p_month[, -1], points_p_year[, -1])

ggplot(points, aes(x = group, y = Freq)) + 
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) + 
  geom_boxplot(aes(color = group), size = 1, outlier.colour = NA) + 
  facet_grid(group ~ ., scales = "free") + my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "Number of data points",
    title = "How many data points did Google collect about me?",
    subtitle = "Number of data points per day, month and year",
    caption = "\nGoogle collected datapoints"
  )


# accuracy
accuracy <- data.frame(accuracy = loc$accuracy, group = ifelse(loc$accuracy < 800, "high", ifelse(loc$accuracy < 5000, "middle", "low")))

accuracy$group <- factor(accuracy$group, levels = c("high", "middle", "low"))

ggplot(accuracy, aes(x = accuracy, fill = group)) + 
  geom_histogram() + 
  facet_grid(group ~ ., scales="free") + 
  my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "Accuracy in metres",
    y = "Count",
    title = "How accurate is the location data?",
    subtitle = "Histogram of accuracy of location points",
    caption = "\nMost data points are pretty accurate, 
    but there are still many data points with a high inaccuracy.
    These were probably from areas with bad satellite reception."
  )



germany <- get_map(location = 'Germany', zoom = 5)
ggmap(germany) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Europe",
    caption = "\nA simple point plot shows recorded positions.")



zaandam <- get_map(location = 'Zaandam', zoom = 12)

options(stringsAsFactors = T)
ggmap(zaandam) + 
  stat_summary_2d(geom = "tile", bins = 100, data = loc, aes(x = lon, y = lat, z = accuracy), alpha = 0.5) + 
  scale_fill_gradient(low = "blue", high = "red", guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points around Münster",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions 
    and their accuracy in and around Zaandam")



# velocity
loc_2 <- loc[which(!is.na(loc$velocity)), ]

zaandam <- get_map(location = 'Zaandam', zoom = 12)

ggmap(zaandam) + geom_point(data = loc_2, aes(x = lon, y = lat, color = velocity), alpha = 0.3) + 
  theme(legend.position = "right") + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Location history data points in Münster",
       subtitle = "Color scale shows velocity measured for location",
       caption = "\nA point plot where points are colored according 
       to velocity nicely reflects that I moved generally 
       slower in the city center than on the autobahn") +
  scale_colour_gradient(low = "blue", high = "red", guide = guide_legend(title = "Velocity"))



# distance
loc3$lat.p1 <- shift.vec(loc3$lat, -1)
loc3$lon.p1 <- shift.vec(loc3$lon, -1)

# Calculating distances between points (in metres) with the function pointDistance from the 'raster' package.
library(raster)
loc3$dist.to.prev <- apply(loc3, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})
# distance in km
round(sum(as.numeric(as.character(loc3$dist.to.prev)), na.rm = TRUE)*0.001, digits = 2)
## [1] 54466.08
distance_p_month <- aggregate(loc3$dist.to.prev, by = list(month_year = as.factor(loc3$month_year)), FUN = sum)
distance_p_month$x <- distance_p_month$x*0.001
ggplot(distance_p_month[-1, ], aes(x = month_year, y = x,  fill = month_year)) + 
  geom_bar(stat = "identity")  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Distance in km",
    title = "Distance traveled per month in 2016",
    caption = "This barplot shows the sum of distances between recorded 
    positions for 2016. In September we went to the US and Canada."
  )


# activities
activities <- loc3$activity

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]

df <- do.call("rbind", activities)
main_activity <- sapply(df$activity, function(x) x[[1]][1][[1]][1])

activities_2 <- data.frame(main_activity = main_activity, 
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, origin = "1970-01-01"))

head(activities_2)


ggplot(activities_2, aes(x = main_activity, group = main_activity, fill = main_activity)) + 
  geom_bar()  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Count",
    title = "Main activities in 2016",
    caption = "Associated activity for recorded positions. 
    Because Google records activity probabilities for each position, 
    only the activity with highest likelihood were chosen for each position."
  )











# fiddrent approach
# https://github.com/edzer/trajectories/blob/master/demo/google.R



# import data
library(jsonlite)
system.time(x <- fromJSON("loc_jr_oct_2017.json"))

# extracting the locations dataframe
loc = x$locations

# converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

loc = subset(loc, accuracy < 100)
loc = subset(loc, time > "2017-01-01")


a = loc$activity
types = unique(unlist(lapply(a, function(x) if (is.null(x[[2]])) "null" else x[[2]][[1]]$type)))
types = types[types != "null"] # NULL entries
getType = function(x, Type) {
  if (is.null(x)) 
    as.numeric(NA) 
  else {
    s = subset(x$activity[[1]], type == Type)$confidence
    if (length(s) == 0) 
      s = 0.0
    s[1]  # there might be more than one, we're now ignoring all others!
  }
}

# very slow
for (Tp in types) # untangle:
  loc[[Tp]] = sapply(a, getType, Type = Tp)

a = apply(loc[,types], 1, function(x) if(all(is.na(x))) NA else which.max(x))
loc$activity = factor(types[a])
loc$confidence = apply(loc[,types], 1, function(x) if(all(is.na(x))) NA else x[which.max(x)])
loc$activity = NULL # remove the complex list

library(sp)
loc.sp = loc
coordinates(loc.sp) = ~lon+lat
proj4string(loc.sp) = CRS("+proj=longlat +datum=WGS84")

library(spacetime)
library(trajectories)
tr = Track(STIDF(geometry(loc.sp), loc.sp$time, loc.sp@data))

stcube(tr)
stcube(tr, showMap = TRUE)
stplot(tr)

#require(mapview)
#mapview(as(tr2, "Spatial"), zcol = "pm10", lwd = 5, legend = TRUE)
#tr2 = generalize(tr, timeInterval = "15 min", tol = 2, toPoints=TRUE)


