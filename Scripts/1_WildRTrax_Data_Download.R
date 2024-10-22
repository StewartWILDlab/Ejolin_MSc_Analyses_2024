# 0. Load packages ###################################################################
library(lubridate)
library(dplyr)
library(tidyr)
library(mefa4)
library(stringr)
library(tidyverse)

project<-"TDN"

# 1. Install wildRtrax ################################################################
## 1.1 Download directly from Github using the remotes package: ####
#remotes::install_github('ABbiodiversity/wildRtrax')

## 1.2 Interested in recent fixes? #####################################
#Download the development branch instead, by including the @development tag: 
#remotes::install_github('ABbiodiversity/wildRtrax@development')

## 1.3 Download Val's branch version###################################
# First, install my branch!
#install.packages("devtools")
devtools::install_github("https://github.com/VLucet/wildRtrax/tree/val-fix-wt_cam")

#Load wildRtrax
library(wildRtrax)

#First we need to set up our username and password as environment variables.

##Note: These need to be called WT_USERNAME and WT_PASSWORD, respectively.

#Sys.setenv(WT_USERNAME = "joli6810@mylaurier.ca", WT_PASSWORD = "Dinoman_9")

#These values live only on your device. But be careful about including sensitive information in a script that you may (inadvertently) share!
#One solution - save a login script file locally1:

credentials <- "Sys.setenv(WT_USERNAME = 'joli6810@mylaurier.ca', WT_PASSWORD = 'Dinoman_9')"
writeLines(credentials, "login.R")

# Then, at the top of your data download script, source the file
source("login.R")

# Now, all you need to do is run one function. With no arguments!!!
wt_auth()

# Which camera/ARU projects do you have access to? Use the `wt_get_download_summary` function.
my_projects <- wt_get_download_summary( 
  sensor_id = "CAM"
)

glimpse(my_projects, width = 90)

#The wt_download_report() function mimics the data download on the WildTrax website.
#You need to supply the project_id value, which we can get from wt_get_download_summary().

# Obtain the project_id value
wt_get_download_summary(sensor_id = "CAM") %>%
  filter(project == "Thaidene Nëné Biodiversity Project 2021") %>%
  select(project_id) %>%
  pull()


# 2. Download main report ###############################################################
tdn_main_raw <- wt_download_report(
  project_id = 712,
  sensor_id = "CAM",
  report = "main",
  weather_cols = FALSE
)


tdn_main_raw %>% select(1:15) %>% glimpse(width = 90)

##2.1 Filter outlier photos/cameras #####################################

#filter raw data downloads before going on to data exploration/analysis

#BMS-CRU-004-01 had one single photo taken on 2021-03-03 11:24:10 before being deployed in August 2021
tag_data_tmp<-filter(tdn_main_raw,!((image_id == 60289868) & #filter image by image_id 
                                      (image_date_time == " 2021-03-03 11:24:10"))) #and image_date_time
stopifnot(nrow(tag_data_tmp)==(nrow(tdn_main_raw)-1)) #stop if the new df isn't smaller by one row

#BIO-TDN-029-02 wasn't retrieved until 2023 but staff walked passed it on 2022-08-19 13:30:10
tag_data_tmp2<-filter(tag_data_tmp,!((location == "BIO-TDN-029-02") & #filter by location
                                  (as_datetime(image_date_time)>as_datetime("2022-08-19 13:30:10")))) #filter images taken after 2022-08-19 13:30:10
stopifnot(nrow(tag_data_tmp2)==(nrow(tag_data_tmp)-1076)) #stop if the new df isn't smaller by 1076

#long term deployments retrieved early, not comparable with other data

#BIO-TDN-052-06 filter out all images after 2022-08-17 16:57:50
tag_data_tmp3<-filter(tag_data_tmp2,!((location == "BIO-TDN-052-06") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 16:57:50")))) #filter out images taken after date/time
stopifnot(nrow(tag_data_tmp3)==(nrow(tag_data_tmp2)-857)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-052-07 filter out all images after 2022-08-17 16:55:08
tag_data_tmp4<-filter(tag_data_tmp3,!((location == "BIO-TDN-052-07") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 16:55:08")))) #filter out images taken after date/time
stopifnot(nrow(tag_data_tmp4)==(nrow(tag_data_tmp3)-866)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-165-01 filter out all images after 2022-08-17 11:58:18
tag_data_tmp5<-filter(tag_data_tmp4,!((location == "BIO-TDN-165-01") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 11:58:18")))) #filter out images taken after date/time
stopifnot(nrow(tag_data_tmp5)==(nrow(tag_data_tmp4)-1603)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-165-06 filter out all images after 2022-08-17 13:37:20
tag_data_tmp6<-filter(tag_data_tmp5,!((location == "BIO-TDN-165-06") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 13:37:20")))) #filter out images taken after date/time
stopifnot(nrow(tag_data_tmp6)==(nrow(tag_data_tmp5)-542)) #stop if the new df isn't smaller by the correct number of tags

#remove the following stations completely

#BIO-TDN-HF-G1, BIO-TDN-HF-G2, BIO-TDN-HF-LF, BIO-TDN-HF-P1, BIO-TDN-HF-R1 all new sites which were added then removed. 
locations_to_filter <- c("BIO-TDN-HF-G1", "BIO-TDN-HF-G2", "BIO-TDN-HF-LF", "BIO-TDN-HF-P1", "BIO-TDN-HF-R1")
tag_data<- tag_data_tmp6 %>% filter(!location %in% locations_to_filter)

length(unique(tag_data$location)) #check to make sure the HF locations got filtered, should be 307

rm(tag_data_tmp,tag_data_tmp2,tag_data_tmp3,tag_data_tmp4,tag_data_tmp5,tag_data_tmp6) #remove tag_data_tmp to clean environment

## 2.2 Remove tags which were "Out of Range" ###############################
unique(tag_data$image_fov) #check fov values

tag_data_filter <- tag_data %>% filter(!image_fov == "Out of Range") #filter out "Out of Range" 
unique(tag_data_filter$image_fov) #check fov values after the filter

#1887640 tags including extra sites/deployments
#1811617 tags wout extra sites/deployments
#1559715 tags wout "Out of Range"
#251903 tags "Out of Range"

#267569 tags of animals ID'd to species level

## 2.3 Correct species names####
#proper common names and facilitates codes later

#change tag_data_filter to sp_summary names
tag_data_filter[tag_data_filter$species_common_name == "Beaver","species_common_name"] <- "American Beaver"
tag_data_filter[tag_data_filter$species_common_name == "Gray Jay", "species_common_name"] <- "Canada Jay"
tag_data_filter[tag_data_filter$species_common_name == "Bald eagle", "species_common_name"] <- "Bald Eagle"

# change barren-ground to just caribou
tag_data_filter[tag_data_filter$species_common_name == "Barren-ground Caribou","species_common_name"] <- "Caribou"

# 3. Independent detections ########################################################

tdn_wt_ind_det <- wt_ind_detect(
  x = tag_data_filter,
  threshold = 30,
  units = "minutes",
  remove_human = TRUE,
  remove_domestic = TRUE
)


glimpse(tdn_wt_ind_det, width = 75)

#8608 independent detections incl all species, with Out of Range
#8336 independent detections incl all species, wout Out of Range
#272  independent detections incl all species, while cameras were Out of Range


# 4. Camera Summaries ##########################################################################

## 4.1 Base wildRtrax summarise function ######################

# wt_summarise_cam function groups months in different years resulting in over 
# 31 days of monthly effort at cameras which were deployed earlier than their 
# retrieval date

# wt_summarise_cam function labels weeks as a number rather than a date (e.g. Week 1)
# Unclear if Week 1 at one camera would be the same as Week 1 at another camera

# UPDATE: monthly and weekly data over 1 year deployments are aggregated, 
# Val wrote code to take care of this and submitted a pull request

#tdn_summarised_month <- wt_summarise_cam(
  #detect_data = tdn_wt_ind_det,  # Supply your detection data
  #raw_data = tag_data,  # Supply your raw image data
  #time_interval = "month", # Now specify the time interval you're interested in 
  #variable = "detections",   # What variable are you interested in?
  #output_format = "long"   # Your desired output format (wide or long)
#)

#summary(tdn_summarised_full_focal$n_days_effort)

## 4.2 Val summary functions #############################

#daily summary using wt_summarise_cam
### NOTE: daily summary doesn't take "Out of Range" data, previously filtered out, into account ####
# days at cameras which were "out of Range" should not be counted as it inflates overall survey effort

###UPDATE: 2023-12-18, Use daily_lookup.csv created in 4_analysis_data_creation.Rmd to subset cam_summaries_day to only the days where the camera was actually functioning properly ####
TDN_daily_lookup<-read.csv("Processed_Data/WildCo/TDN_daily_lookup.csv")

if ("season" %in% colnames(TDN_daily_lookup)) {
  # Remove the "season" column
  TDN_daily_lookup <- TDN_daily_lookup[, !colnames(TDN_daily_lookup) %in% "season"]
} else {  # Print a message or take appropriate action if the column doesn't exist
  print("The column 'season' does not exist in the data frame.")
}
#rename "date" to day to allow join
TDN_daily_lookup <- TDN_daily_lookup %>%
  rename(day = date)

# Convert "day" column in TDN_daily_lookup to Date type
TDN_daily_lookup <- TDN_daily_lookup %>%
  mutate(day = as.Date(day))

#summarise independent detections by day across all cameras
cam_summaries_day <- wt_summarise_cam(detect_data = tdn_wt_ind_det, 
                                      raw_data = tag_data_filter,
                                      time_interval = "day", 
                                      variable = "detections",
                                      output_format = "wide")

# Filter cam_summaries_day based on TDN_daily_lookup
filtered_cam_summaries_day <- cam_summaries_day %>%
  inner_join(TDN_daily_lookup, by = c("location", "day"))

# filtered_cam_summaries_day now contains only the rows where location and day
# are present in both cam_summaries_day and TDN_daily_lookup

#weekly summary
### NOTE: Replace current week label with YEAR-WXX ####
# where each week in each year is assigned a number 1-52.
###UPDATE: 2023-12-18, this format is fine considering it's just the 52 weeks of the year...####
### NOTE: Doesn't take Out of Range into account, inflates effort
###UPDATE: 2023-12-18, just use daily summary and Val's home made function below ####
#cam_summaries_week <- wt_summarise_cam(detect_data = tdn_wt_ind_det, 
                                      #raw_data = tag_data_filter,
                                      #time_interval = "week", 
                                      #variable = "detections",
                                      #output_format = "wide")

#monthly summary
### NOTE: Doesn't take Out of Range into account, inflates effort
#cam_summaries_month <- wt_summarise_cam(detect_data = tdn_wt_ind_det, 
                                      #raw_data = tag_data_filter,
                                      #time_interval = "month", 
                                      #variable = "detections",
                                      #output_format = "wide")

#full study period
### NOTE: function creates the same dataframe with day/full time_interval ####
#cam_summaries_full <- wt_summarise_cam(detect_data = tdn_wt_ind_det, 
                                       #raw_data = tag_data_filter,
                                       #time_interval = "full", 
                                       #variable = "detections",
                                       #output_format = "wide")

#use daily summary to generate monthly detections
cam_summaries_month <- filtered_cam_summaries_day |>
  mutate(month = month(day)) |>
  relocate(month, .after = year) |>
  group_by(location, year, month) |>
  summarise(across(`n_days_effort`:`Yellow-rumped Warbler`, sum))

#weekly summary
#creates a 53rd week in 2021 with only 1 day of effort for cameras not 
#Out of Range at the end of December, not sure if this is an issue
cam_summaries_week <- filtered_cam_summaries_day |>
  mutate(week = week(day)) |>
  relocate(week, .after = year) |>
  group_by(location, year, week) |>
  summarise(across(`n_days_effort`:`Yellow-rumped Warbler`, sum))


#full summary
cam_summaries_full <- filtered_cam_summaries_day |>
  group_by(location) |>
  summarise(across(`n_days_effort`:`Yellow-rumped Warbler`, sum))

#biweekly summary
cam_summaries_biweek <- filtered_cam_summaries_day %>%
  mutate(biweek = ceiling(week(day) / 2)) %>%  # Divide weeks by 2 to create two-week intervals
  relocate(biweek, .after = year) %>%
  group_by(location, year, biweek) %>%
  summarise(across(`n_days_effort`:`Yellow-rumped Warbler`, sum))

## 4.3 Write csv ####

#daily summary
write.csv(filtered_cam_summaries_day, paste0(project,"_cam_summaries_day.csv"), row.names = F)

#weekly summary
write.csv(cam_summaries_week, paste0(project,"_cam_summaries_week.csv"), row.names = F)

#monthly summary
write.csv(cam_summaries_month, paste0(project,"_cam_summaries_month.csv"), row.names = F)

#full summary
write.csv(cam_summaries_full, paste0(project,"_cam_summaries_full.csv"), row.names = F)

#biweekly summary
write.csv(cam_summaries_biweek, paste0(project,"_cam_summaries_biweek.csv"), row.names = F)


#5. Focal Sp ###########################################################################

## 5.1 Subset focal species detections #################################
#subset to our focal species: Caribou, Muskox, Moose, Wolves, Grizzly, BBear, Wolverine, Lynx
tag_data_focal <- tag_data_filter %>% 
  filter(species_common_name %in% c("Caribou","Muskox", 
        "Moose","Black Bear","Grizzly Bear","Gray Wolf","Wolverine",
        "Canada Lynx"))

#248804 tags of our focal species

focal_tag_table<-table(tag_data_focal$location, tag_data_focal$species_common_name)
colSums(focal_tag_table) #check number of tags for each focal species

#Caribou 181593, Muskox 46907, Moose 13138, 
#Black Bear 3700, Gray Wolf 1315, Grizzly Bear 1039
#Canada Lynx 578, Wolverine 534

## 5.2 Independent detections of focal species ###########################

tdn_wt_ind_det_focal <- wt_ind_detect(
  x = tag_data_focal,
  threshold = 30,
  units = "minutes",
  remove_human = TRUE,
  remove_domestic = TRUE
)

glimpse(tdn_wt_ind_det_focal, width = 75)
#3888 independent detections of focal sp, wout Out of Range

## 5.3 Camera summaries of focal species ################################

#summarise independent detections by day across all cameras
cam_summaries_day_focal <- wt_summarise_cam(detect_data = tdn_wt_ind_det_focal, 
                                      raw_data = tag_data, #cant be tag_data_focal or else the effort will be off
                                      time_interval = "day", 
                                      variable = "detections",
                                      output_format = "wide")

# Filter cam_summaries_day based on TDN_daily_lookup to get accurate effort data
filtered_cam_summaries_day_focal <- cam_summaries_day_focal %>%
  inner_join(TDN_daily_lookup, by = c("location", "day"))

#monthly summary
cam_summaries_month_focal <- filtered_cam_summaries_day_focal |>
  mutate(month = month(day)) |>
  relocate(month, .after = year) |>
  group_by(location, year, month) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#weekly summary
#creates a 53rd week in 2021 with only 1 day of effort for cameras not 
#Out of Range at the end of December, not sure if this is an issue
cam_summaries_week_focal <- filtered_cam_summaries_day_focal |>
  mutate(week = week(day)) |>
  relocate(week, .after = year) |>
  group_by(location, year, week) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#full summary
cam_summaries_full_focal <- filtered_cam_summaries_day_focal |>
  group_by(location) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#biweekly summary
cam_summaries_biweek_focal <- filtered_cam_summaries_day_focal %>%
  mutate(biweek = ceiling(week(day) / 2)) %>%  # Divide weeks by 2 to create two-week intervals
  relocate(biweek, .after = year) %>%
  group_by(location, year, biweek) %>%
  summarise(across(`n_days_effort`:`Wolverine`, sum))

## 5.4 Write csv ####

#daily summary focal
write.csv(filtered_cam_summaries_day_focal, paste0(project,"_cam_summaries_day_focal.csv"), row.names = F)

#weekly summary focal
write.csv(cam_summaries_week_focal, paste0(project,"_cam_summaries_week_focal.csv"), row.names = F)

#monthly summary focal
write.csv(cam_summaries_month_focal, paste0(project,"_cam_summaries_month_focal.csv"), row.names = F)

#full summary focal
write.csv(cam_summaries_full_focal, paste0(project,"_cam_summaries_full_focal.csv"), row.names = F)

#biweekly summary focal
write.csv(cam_summaries_biweek_focal, paste0(project,"_cam_summaries_biweek_focal.csv"), row.names = F)

## 5.5 Cluster Summary ####
#create total camera summary by site for all species

#read in csv with camera and cluster names
site_locations<-read.csv("Raw_Data/TDN_camera_site_locations.csv")

#join to cam_summaries_full_focal
site_summaries_full_tmp<- left_join(cam_summaries_full, site_locations, by = "location")

#reorder columns
site_summaries_full_tmp<-site_summaries_full_tmp %>% 
  select(location,latitude,longitude,location_site,latitude_site,longitude_site,everything())

#remove camera names and locations
site_summaries_full<- site_summaries_full_tmp %>%
  select(-location, -latitude, -longitude)

#group detections by camera cluster/site
site_summaries_full <-  site_summaries_full |>
  group_by(location_site,latitude_site,longitude_site) |>
  summarise(across(`n_days_effort`:`Yellow-rumped Warbler`, sum))

#write csv
write.csv(site_summaries_full, paste0(project,"_clust_summaries_full.csv"), row.names = F)

# 6. Mammals only ####

## 6.1 Subset mammal species detections #################################
#Could've been easier to do this by joing with sp_rank data from another df
tag_data_mam <- tag_data_filter %>% 
  filter(species_common_name %in% c("Caribou","Muskox", 
        "Moose","Black Bear","Grizzly Bear","Gray Wolf","Wolverine",
        "Canada Lynx","Red Squirrel","Marten","Snowshoe Hare",
        "Porcupine","Northern Flying Squirrel","Ermine","Red Fox",
        "American Beaver","Muskrat","Arctic Hare","Arctic Ground Squirrel",
        "Arctic Fox"))

#263364 tags of our mammal species

mam_tag_table<-table(tag_data_mam$location, tag_data_mam$species_common_name)
colSums(mam_tag_table) #check number of tags for each mammal species

#Arctic Fox 10, Arctic Ground Squirrel 171, Arctic Hare 187, 
#Caribou 181593, American Beaver 3, Black Bear 3700, Canada Lynx 578, 
#Ermine 10, Gray Wolf 1315, Grizzly Bear 1039, Marten 675, Moose 13138, 
#Muskox 46907, Muskrat 3, Northern Flying Squirrel 3, Porcupine 437, Red Fox 422,
#Red Squirrel 2000, Snowshoe Hare 10639, Wolverine 534

## 6.2 Independent detections of mammal species ###########################

tdn_wt_ind_det_mam <- wt_ind_detect(
  x = tag_data_mam,
  threshold = 30,
  units = "minutes",
  remove_human = TRUE,
  remove_domestic = TRUE
)

glimpse(tdn_wt_ind_det_focal, width = 75)
#7258 independent detections of mammal sp, wout Out of Range

## 6.3 Camera summaries of mammal species ################################

#summarise independent detections by day across all cameras
cam_summaries_day_mam <- wt_summarise_cam(detect_data = tdn_wt_ind_det_mam, 
                                            raw_data = tag_data, #cant be tag_data_focal or else the effort will be off
                                            time_interval = "day", 
                                            variable = "detections",
                                            output_format = "wide")

# Filter cam_summaries_day based on TDN_daily_lookup to get accurate effort data
filtered_cam_summaries_day_mam <- cam_summaries_day_mam %>%
  inner_join(TDN_daily_lookup, by = c("location", "day"))

#monthly summary
cam_summaries_month_mam <- filtered_cam_summaries_day_mam |>
  mutate(month = month(day)) |>
  relocate(month, .after = year) |>
  group_by(location, year, month) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#weekly summary
#creates a 53rd week in 2021 with only 1 day of effort for cameras not 
#Out of Range at the end of December, not sure if this is an issue
cam_summaries_week_mam <- filtered_cam_summaries_day_mam |>
  mutate(week = week(day)) |>
  relocate(week, .after = year) |>
  group_by(location, year, week) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#full summary
cam_summaries_full_mam <- filtered_cam_summaries_day_mam |>
  group_by(location) |>
  summarise(across(`n_days_effort`:`Wolverine`, sum))

#biweekly summary
cam_summaries_biweek_mam <- filtered_cam_summaries_day_mam %>%
  mutate(biweek = ceiling(week(day) / 2)) %>%  # Divide weeks by 2 to create two-week intervals
  relocate(biweek, .after = year) %>%
  group_by(location, year, biweek) %>%
  summarise(across(`n_days_effort`:`Wolverine`, sum))

## 6.4 Write csv ####

#daily summary focal
write.csv(filtered_cam_summaries_day_mam, paste0(project,"_cam_summaries_day_mam.csv"), row.names = F)

#weekly summary focal
write.csv(cam_summaries_week_mam, paste0(project,"_cam_summaries_week_mam.csv"), row.names = F)

#monthly summary focal
write.csv(cam_summaries_month_mam, paste0(project,"_cam_summaries_month_mam.csv"), row.names = F)

#full summary focal
write.csv(cam_summaries_full_mam, paste0(project,"_cam_summaries_full_mam.csv"), row.names = F)

#biweekly summary focal
write.csv(cam_summaries_biweek_mam, paste0(project,"_cam_summaries_biweek_mam.csv"), row.names = F)

