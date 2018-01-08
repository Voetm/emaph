# NOTE: code adapted from example found at:
#    https://rpubs.com/robchavez/60451

# libraries --------------------------------------------------------------------
library(tidyverse)
library(jsonlite)

# get GPSdata ------------------------------------------------------------------

temp <- "data-raw/GPSdata/GPSdata.zip"
con <- unz(temp, "loc_jr_oct_2017.json")
d <- read_file(con)


# be patient: it takes over a minute to parse the full file
system.time(x <- fromJSON(d))


# set variables ----------------------------------------------------------------

# extracting the locations dataframe
locs = x$locations


# converting time column from posix milliseconds into a readable time scale
ldf <- data.frame(TimeStamp = as.numeric(locs$timestampMs) / 1000)
ldf$TimeStamp <- as.POSIXct(ldf$TimeStamp, origin = "1970-01-01")

attr(ldf$TimeStamp , "label") <- "Datetime of measurement"


# converting longitude and latitude from E7 to GPS coordinates
ldf$lat = locs$latitudeE7 / 1e7
attr(ldf$lat , "label") <- "GPS Latitude"

ldf$lon = locs$longitudeE7 / 1e7
attr(ldf$lon , "label") <- "GPS Longitude"



# Accuracy doesn't need changing.
ldf$accuracy <- locs$accuracy
attr(ldf$accuracy , "label") <- "GPS estimated accuracy (in meters)"


# activity

# get the most likely activity type and confidence for each time point.
get_act <- function(f){
  if (is.null(f[[1]])) {
    data.frame(
      activity = NA,
      confidence = NA,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      activity = f[[2]][[1]][[1]][1],
      confidence = f[[2]][[1]][[2]][1],
      stringsAsFactors = F
    )
  }
}

l <- map(locs$activity, get_act)
act <- do.call(rbind, l)


# combine activity data with the main dataset
ldf$activity <- as.character(act[, 1])
ldf$activity[ldf$activity == "NA"] = NA
attr(ldf$activity , "label") <- "Activity prediction"


ldf$activity_confidence <- as.numeric(act[, 2])
ldf$activity_confidence[ldf$confidence == "NA"] = NA
attr(ldf$activity_confidence , "label") <- "Activity prediction confidence"


# Velocity, altitude and heading need no alteration:
ldf$velocity <- locs$velocity
attr(ldf$velocity , "label") <- "Velocity (in km per hour)"


ldf$altitude <- locs$altitude
attr(ldf$altitude , "label") <- "Altitude (in meters)"


# heading has too many missing values
#ldf$heading  <- locs$heading


# subset -----------------------------------------------------------------------

# data collecting becomes interesting at dec 2016
ldf <- ldf %>% filter(TimeStamp > "2016-12-01 00:00:00")



# save ------------------------------------------------------------------------
GPSdata <- as.tibble(ldf)
save(file = "data-raw/GPSdata/GPSdata.Rda", GPSdata)


# Inject in package  -----------------------------------------------------------
devtools::use_data(GPSdata, overwrite = TRUE, compress = "xz")


# check ------------------------------------------------------------------------

# View(GPSdata[1:100, ])

