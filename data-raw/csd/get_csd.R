# libraries --------------------------------------------------------------------
library(tidyverse)
library(haven)
library(hms)

# get EMAdata ------------------------------------------------------------------

temp <- "data-raw/csd/ESMdata.zip"

# uncomment next line to re-download data from source location
#download.file("https://osf.io/c6xt4/download", temp, mode="wb")

con <- unz(temp, "ESMdata/ESMdata.csv")
d <- read_csv(con, col_names = TRUE)

# uncomment next line if you re-downloaded the data
#unlink(temp)


# set variables ----------------------------------------------------------------

# remove rowcounter
d <- d[-1]


# phase
d$phase = factor(d$phase,
                 levels = 1:5,
                 labels = c(
                   "baseline",
                   "double blind before reducing medication",
                   "double blind during medication reduction",
                   "phase after medication reduction",
                   "phase after experiment"

                 ))
attr(d$phase, "label") <- "Various phases in experiment"


# concentrat
attr(d$concentrat, "label") <- "Concentration of anti-depressant"


# Time -----------

# date
# (define this first, so that dayno can be properly re-calculated)
d$date <- as.Date(d$date, format = "%d/%m/%y")
attr(d$date, "label") <- "Date of measurement"

# dayno
attr(d$dayno, "label") <- "Day number in experiment"
d$dayno <- as.numeric(difftime(d$date, d$date[1], unit = "days")) + 1

# beepno
attr(d$beepno, "label") <- "Sequence of measurements within a day"

# beeptime
d$beeptime <- as.hms(d$beeptime)
attr(d$beeptime, "label") <- "Time of presentation of questionnaire"


# resptime_s
d$resptime_s <- as.hms(d$resptime_s)
attr(d$resptime_s, "label") <- "Time at start questionnaire"


# resptime_s
d$resptime_e <- as.hms(d$resptime_e)
attr(d$resptime_e, "label") <- "Time at end questionnaire"


# resp_abort
d$resp_abort <- factor(d$resp_abort,
                       levels = 0:1,
                       labels = c("no", "yes"))
attr(d$resp_abort, "label") <- "Questionnaire aborted"


# Mood items -----------

# mood_relaxed
d$mood_relaxed <- labelled(d$mood_relaxed,
                           labels = c("not" = 1, "very" = 7))
attr(d$mood_relaxed, "label") <- "I feel relaxed"


# mood_down (recoded to 1-7)
d$mood_down <- d$mood_down + 4
d$mood_down <- labelled(d$mood_down,
                        labels = c("not" = 1, "very" = 7))
attr(d$mood_down, "label") <- "I feel down"


# mood_irritat
d$mood_irritat <- labelled(d$mood_irritat,
                           labels = c("not" = 1, "very" = 7))
attr(d$mood_irritat, "label") <- "I feel irritated"


# mood_satisfi
d$mood_satisfi <- labelled(d$mood_satisfi,
                           labels = c("not" = 1, "very" = 7))
attr(d$mood_satisfi, "label") <- "I feel satisfied"


# mood_lonely (recoded to 1-7)
d$mood_lonely <- d$mood_lonely + 4
d$mood_lonely <- labelled(d$mood_lonely,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_lonely, "label") <- "I feel lonely"


# mood_anxious (recoded to 1-7)
d$mood_anxious <- d$mood_anxious + 4
d$mood_anxious <- labelled(d$mood_anxious,
                           labels = c("not" = 1, "very" = 7))
attr(d$mood_anxious, "label") <- "I feel anxious"


# mood_enthus
d$mood_enthus <- labelled(d$mood_enthus,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_enthus, "label") <- "I feel enthusiastic"


# mood_enthus
d$mood_suspic <- labelled(d$mood_suspic,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_suspic, "label") <- "I feel suspicious"


# mood_cheerf
d$mood_cheerf <- labelled(d$mood_cheerf,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_cheerf, "label") <- "I feel cheerful"


# mood_guilty (recoded to 1-7)
d$mood_guilty <- d$mood_guilty + 4
d$mood_guilty <- labelled(d$mood_guilty,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_guilty, "label") <- "I feel guilty"


# mood_doubt
d$mood_doubt <- labelled(d$mood_doubt,
                         labels = c("not" = 1, "very" = 7))
attr(d$mood_doubt, "label") <- "I feel indecisive"


# mood_strong
d$mood_strong <- labelled(d$mood_strong,
                          labels = c("not" = 1, "very" = 7))
attr(d$mood_strong, "label") <- "I feel strong"




# pat_restl
d$pat_restl <- labelled(d$pat_restl,
                        labels = c("not" = 1, "very" = 7))
attr(d$pat_restl, "label") <- "I feel restless"


# pat_agitate
d$pat_agitate <- labelled(d$pat_agitate,
                          labels = c("not" = 1, "very" = 7))
attr(d$pat_agitate, "label") <- "I feel agitated"


# pat_worry
d$pat_worry <- labelled(d$pat_worry,
                        labels = c("not" = 1, "very" = 7))
attr(d$pat_worry, "label") <- "I worry"


# pat_concent
d$pat_concent <- labelled(d$pat_concent,
                          labels = c("not" = 1, "very" = 7))
attr(d$pat_concent, "label") <- "I can concentrate well"


# se_selflike
d$se_selflike <- labelled(d$se_selflike,
                          labels = c("not" = 1, "very" = 7))
attr(d$se_selflike, "label") <- "I like myself"


# se_ashamed
d$se_ashamed <- labelled(d$se_ashamed,
                         labels = c("not" = 1, "very" = 7))
attr(d$se_ashamed, "label") <- "I am ashamed of myself"


# se_selfdoub
d$se_selfdoub <- labelled(d$se_selfdoub,
                         labels = c("not" = 1, "very" = 7))
attr(d$se_selfdoub, "label") <- "I doubt myself"


# se_handle
d$se_handle <- labelled(d$se_handle,
                          labels = c("not" = 1, "very" = 7))
attr(d$se_handle, "label") <- "I can handle anything"


# Social items -----------

# soc_who1
d$soc_who1 <- factor(d$soc_who1,
                     levels = c(0, 10, 17, 19, 27, 29, 30, 40, 49, 50),
                     labels = c(
                       "nobody",
                       "partner",
                       "family resident",
                       "roommates",
                       "family non-resident",
                       "family living at other places",
                       "friends",
                       "colleagues",
                       "acquaintances",
                       "strangers/others"))
attr(d$soc_who1, "label") <- "Who am I with?"


# soc_enjoy_alone
d$soc_enjoy_alone <- labelled(d$soc_enjoy_alone,
                        labels = c("not" = 1, "very" = 7))
attr(d$soc_enjoy_alone, "label") <- "I enjoy to be alone."


# soc_prefcomp
d$soc_prefcomp <- labelled(d$soc_prefcomp,
                           labels = c("not" = 1, "very" = 7))
attr(d$soc_prefcomp, "label") <- "I prefer being in company."


# soc_prefcomp
d$soc_prefcomp <- labelled(d$soc_prefcomp,
                           labels = c("not" = 1, "very" = 7))
attr(d$soc_prefcomp, "label") <- "I prefer being in company."


# soc_who2
d$soc_who2 <- factor(d$soc_who2,
                     levels = c(0, 10, 17, 19, 27, 29, 30, 40, 49, 50),
                     labels = c(
                       "nobody",
                       "partner",
                       "family resident",
                       "roommates",
                       "family non-resident",
                       "family living at other places",
                       "friends",
                       "colleagues",
                       "acquaintances",
                       "strangers/others"))
attr(d$soc_who2, "label") <- "Who else am I with?"


# soc_who3
d$soc_who3 <- factor(d$soc_who3,
                     levels = c(0, 10, 17, 19, 27, 29, 30, 40, 49, 50),
                     labels = c(
                       "nobody",
                       "partner",
                       "family resident",
                       "roommates",
                       "family non-resident",
                       "family living at other places",
                       "friends",
                       "colleagues",
                       "acquaintances",
                       "strangers/others"))
attr(d$soc_who3, "label") <- "And .. (who else in addition)"


# soc_belong
d$soc_belong <- labelled(d$soc_belong,
                        labels = c("1 person" = 1, "6+ persons" = 7))
attr(d$soc_belong, "label") <- "I prefer being in company."


# soc_pleasant
d$soc_pleasant <- labelled(d$soc_pleasant,
                           labels = c("not" = 1, "very" = 7))
attr(d$soc_pleasant, "label") <- "I find this company pleasant."


# soc_prefalone
d$soc_prefalone <- labelled(d$soc_prefalone,
                           labels = c("not" = 1, "very" = 7))
attr(d$soc_prefalone, "label") <- "I prefer to be alone."


# soc_together
d$soc_together <- labelled(d$soc_together,
                            labels = c("not" = 1, "very" = 7))
attr(d$soc_together, "label") <- "We are doing something together."


# Physical items -----------

# phy_hungry
d$phy_hungry <- labelled(d$phy_hungry,
                           labels = c("not" = 1, "very" = 7))
attr(d$phy_hungry, "label") <- "I am hungry."


# phy_tired
d$phy_tired <- labelled(d$phy_tired,
                         labels = c("not" = 1, "very" = 7))
attr(d$phy_tired, "label") <- "I am tired."


# phy_pain
d$phy_pain <- labelled(d$phy_pain,
                        labels = c("not" = 1, "very" = 7))
attr(d$phy_pain, "label") <- "I am pain."


# phy_dizzy
d$phy_dizzy <- labelled(d$phy_dizzy,
                       labels = c("not" = 1, "very" = 7))
attr(d$phy_dizzy, "label") <- "I feel dizzy."


# phy_drymouth
d$phy_drymouth <- labelled(d$phy_drymouth,
                        labels = c("not" = 1, "very" = 7))
attr(d$phy_drymouth, "label") <- "I have a dry mouth."


# phy_nauseous
d$phy_nauseous <- labelled(d$phy_nauseous,
                           labels = c("not" = 1, "very" = 7))
attr(d$phy_nauseous, "label") <- "I feel nauseous."


# phy_headache
d$phy_headache <- labelled(d$phy_headache,
                           labels = c("not" = 1, "very" = 7))
attr(d$phy_headache, "label") <- "I have a headache."


# phy_sleepy
d$phy_sleepy <- labelled(d$phy_sleepy,
                           labels = c("not" = 1, "very" = 7))
attr(d$phy_sleepy, "label") <- "I am sleepy."


# Activity items -----------

# act_what1
d$act_what1 <- factor(d$act_what1,
                     levels = c(0, 1, 10, 20, 21, 26, 27, 41,
                                43, 45, 47, 49, 51, 60, 88, 89),
                     labels = c(
                       "nothing",
                       "resting",
                       "work/studies",
                       "housekeeping/shopping",
                       "caring for others",
                       "medical care",
                       "taking care of oneself",
                       "sports",
                       "active relaxation",
                       "passive relaxation",
                       "chatting/texting/facebook etc",
                       "chilling",
                       "talking",
                       "eating/drinking",
                       "traveling",
                       "other"))
attr(d$act_what1, "label") <- "What am I doing (right before the beep)?"


# act_what2
d$act_what2 <- factor(d$act_what2,
                      levels = c(0, 1, 10, 20, 21, 26, 27, 41,
                                 43, 45, 47, 49, 51, 60, 88, 89),
                      labels = c(
                        "nothing",
                        "resting",
                        "work/studies",
                        "housekeeping/shopping",
                        "caring for others",
                        "medical care",
                        "taking care of oneself",
                        "sports",
                        "active relaxation",
                        "passive relaxation",
                        "chatting/texting/facebook etc",
                        "chilling",
                        "talking",
                        "eating/drinking",
                        "traveling",
                        "other"))
attr(d$act_what2, "label") <- "What else are you doing?"


# act_difficul
d$act_difficul <- labelled(d$act_difficul,
                           labels = c("not" = 1, "very" = 7))
attr(d$act_difficul, "label") <- "This (activity) requires effort."


# act_well
d$act_well <- labelled(d$act_well,
                       labels = c("not" = 1, "very" = 7))
attr(d$act_well, "label") <- "I am good at this"


# act_enjoy
d$act_enjoy <- labelled(d$act_enjoy,
                       labels = c("not" = 1, "very" = 7))
attr(d$act_enjoy, "label") <- "I like doing this."


# phy_physact
d$phy_physact <- labelled(d$phy_physact,
                         labels = c("not" = 1, "very" = 7))
attr(d$phy_physact, "label") <- "From the last beep onwards I was physically active"



# Event items -----------

# event_pleas (recoded to 1-7)
d$event_pleas <- d$event_pleas + 4
d$event_pleas <- labelled(d$event_pleas,
                          labels = c("very unpleasant" = 1,
                                     "very pleasant"   = 7))
attr(d$event_pleas, "label") <- "This event was..."


# event_import (recoded to 1-7)
d$event_import <- d$event_import + 4
d$event_import <- labelled(d$event_import,
                          labels = c("very unimportant" = 1,
                                     "very important"   = 7))
attr(d$event_import, "label") <- "This event was..."


# event_cause
d$event_cause <- factor(d$event_cause,
                        levels = 1:5,
                        labels = c(
                          "something that happened to me",
                          "something I had influence on",
                          "some routine or regular event",
                          "a thought/feeling",
                          "other"))
attr(d$event_cause, "label") <- "This event was ..."


# event_concern
d$event_concern <- factor(d$event_concern,
                         levels = 1:6,
                         labels = c(
                          "contact with other people",
                          "the Environment I was in",
                          "the state I am in",
                          "activity",
                          "new information",
                          "other"))
attr(d$event_concern, "label") <- "This event mainly had to do with ..."


# event_freq
d$event_freq <- factor(d$event_freq,
                          levels = 1:4,
                          labels = c(
                            "several times a day",
                            "daily",
                            "weekly",
                            "monthly"))
attr(d$event_freq, "label") <- "This event usually happens ..."


# event_pertain
d$event_pertain <- factor(d$event_pertain,
                       levels = 1:7,
                       labels = c(
                         "others",
                         "myself",
                         "concrete things",
                         "activity",
                         "something abstract",
                         "unknown",
                         "other"))
attr(d$event_pertain, "label") <- "This event pertained to ..."


# event_disturb
d$event_disturb <- labelled(d$event_disturb,
                            labels = c("not" = 1, "very" = 7))
attr(d$event_disturb, "label") <- "This beep disturbs me."


# event_ordinary
d$evn_ordinary <- labelled(d$evn_ordinary,
                           labels = c("not" = 1, "very" = 7))
attr(d$evn_ordinary, "label") <- "I found this an ordinary day."


# event_niceday
d$evn_niceday <- labelled(d$evn_niceday,
                          labels = c("not" = 1, "very" = 7))
attr(d$evn_niceday, "label") <- "I found this a nice day."


# event_inflmood
d$evn_inflmood <- labelled(d$evn_inflmood,
                           labels = c("not" = 1, "very" = 7))
attr(d$evn_inflmood, "label") <- "Filling in this questionnaire influenced my mood."


# Evening diary -----------

# evn_pager
d$evn_pager <- labelled(d$evn_pager,
                        labels = c("not" = 1, "very" = 7))
attr(d$evn_pager, "label") <- "Filling in this questionnaire influenced my mood."


# evn_work
d$evn_work <- factor(d$evn_work,
                     levels = 0:1,
                     labels = c("no", "yes"))
attr(d$evn_work, "label") <- "I worked/studied today."


# evn_med
d$evn_med <- factor(d$evn_med,
                    levels = 0:1,
                    labels = c("no", "yes"))
attr(d$evn_med, "label") <- "I took my medication today."



# Morning diary -----------

# mor_asleep
d$mor_asleep <- factor(d$mor_asleep,
                       levels = 1:8,
                       labels = c(
                         "0-5 minutes",
                         "5-15 minutes",
                         "15-30 minutes",
                         "30-45 minutes",
                         "45-60 minutes",
                         "60-120 minutes",
                         "120-240 minutes",
                         "240+ minutes"))
attr(d$mor_asleep, "label") <- "How long did it take me to fall asleep last night?"


# mor_nrwakeup
d$mor_nrwakeup <- factor(d$mor_nrwakeup,
                         levels = 0:6,
                         labels = c(
                           "0 times",
                           "1",
                           "2",
                           "3",
                           "4",
                           "5",
                           "5+ times"))
attr(d$mor_nrwakeup, "label") <- "How often did I wake up last night?"


# mor_lieawake
d$mor_lieawake <- factor(d$mor_lieawake,
                           levels = 1:8,
                           labels = c(
                             "0-5 minutes",
                             "5-15 minutes",
                             "15-30 minutes",
                             "30-45 minutes",
                             "45-60 minutes",
                             "60-120 minutes",
                             "120-240 minutes",
                             "240+ minutes"))
attr(d$mor_lieawake, "label") <- "How long did I lie awake this morning before I got up?"


# mor_qualsleep
d$mor_qualsleep <- labelled(d$mor_qualsleep,
                          labels = c("not" = 1,
                                     "moderate" = 4,
                                     "very" = 7))
attr(d$mor_qualsleep, "label") <- "I slept well."


# mor_feellike
d$mor_feellike <- labelled(d$mor_feellike,
                          labels = c("not" = 1,
                                     "moderate" = 4,
                                     "very" = 7))
attr(d$mor_feellike, "label") <- "I am looking forward to this day."


# mor_med
d$mor_med <- factor(d$mor_med,
                    levels = 0:1,
                    labels = c("no", "yes"))
attr(d$mor_med, "label") <- "I took my medication yesterday."


# SCL90R -----------

# SCL-90-R dep scale items
d[grep("SCL.90.R", names(d))] <-
  lapply(d[grep("SCL.90.R", names(d))], function(x) {
    x <- labelled(x,
                  labels = c(
                    "not at all"   = 0,
                    "a little bit" = 1,
                    "moderately"   = 2,
                    "quite a bit"  = 3,
                    "extremely"    = 4
                  ))
  })

# lowercase variables, stick to _ as delimiter
names(d)[71:84] = sub("SCL.90.R.", "scl90r_", names(d)[71:84])
names(d)[75] = "scl90r_05" # consistent double digits

attr(d$scl90r_14, "label") <- "How much were you bothered by feeling low in energy or slowed down?"
attr(d$scl90r_20, "label") <- "How much were you bothered by crying easily?"
attr(d$scl90r_22, "label") <- "How much were you bothered by feeling of being trapped or caught?"
attr(d$scl90r_05, "label") <- "How much were you bothered by loss of sexual interest or pleasure?"
attr(d$scl90r_29, "label") <- "How much were you bothered by feeling lonely?"
attr(d$scl90r_26, "label") <- "How much were you bothered by blaming yourself for things??"
attr(d$scl90r_15, "label") <- "How much were you bothered by thoughts of ending your life?"
attr(d$scl90r_30, "label") <- "How much were you bothered by feeling blue?"
attr(d$scl90r_31, "label") <- "How much were you bothered by worrying too much about things?"
attr(d$scl90r_32, "label") <- "How much were you bothered by feeling no interest in things?"
attr(d$scl90r_54, "label") <- "How much were you bothered by feeling hopeless about the future?"
attr(d$scl90r_71, "label") <- "How much were you bothered by feeling everything is an effort?"
attr(d$scl90r_79, "label") <- "How much were you bothered by feelings of worthlessness?"


# dep
names(d)[85] = "scl90r_dep"
attr(d$scl90r_dep, "label") <- "SCL-90-R depression score"


# re-order columns -------------------------------------------------------------

d <- d[c(
  2:3,          # experiment information
  4:5, 1, 6:9,  # when was the data collected?
  10:85         # measures
)]


# save ------------------------------------------------------------------------
csd <- d

#

save(file = "data-raw/csd/csd.Rda", csd)


# Inject in package data -------------------------------------------------------
devtools::use_data(csd, overwrite = TRUE, compress = "xz")


# check ------------------------------------------------------------------------

# View(csd)
summary(csd)


