# libraries --------------------------------------------------------------------
library(tidyverse)
library(haven)
library(hms)

# get Caspar data --------------------------------------------------------------

load("data-raw/caspar/source/ema.Rda")

# set variables ----------------------------------------------------------------

# id
d <- tibble(id = ema$id)
attr(d$id, "label") <- "Participant id"


# date
d$timestamp <- ema$t
attr(d$timestamp, "label") <- "Date/time of measurement"

# form
d$form <- ema$form
attr(d$form, "label") <- "item set"

# assessment round
d$round <- ema$round
attr(d$round, "label") <- "assessment round"

# item
d$item <- as.character(ema$item)
d$item <- factor(d$item)
attr(d$item, "label") <- "item"

# score
d$score = ema$score
attr(d$score, "label") <- "item score"


# save ------------------------------------------------------------------------
caspar <- d
save(file = "data-raw/caspar/caspar.Rda", caspar)


# Inject in package data -------------------------------------------------------
devtools::use_data(caspar, overwrite = TRUE, compress = "xz")

# check ------------------------------------------------------------------------

# View(csd)
summary(caspar)


