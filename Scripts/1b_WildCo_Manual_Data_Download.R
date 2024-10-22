#### Intro ####

# This script is to make Wildtrax camera data output cleaner
# and create a deployment spreadsheet, pull group count from the comments, 
# and create an independent detections spreadsheet

# This script takes raw detection data from Wildtrax data download
# and it saves four data frames

# -1- Project_CAM_Species_List_v#.csv : A basic species list
# -2- Project_CAM_Deployment_Data_v#.csv :  Deployment information with start date, end date, and duration (effort)
# -3- Project_CAM_Detection_Data_v#.csv : Detection data. Just the raw detection data, but a bit cleaned
# -4- Project_CAM_Independent_Detections_v#.csv : Independent detections, summarized with group count, based on a time threshold you specify


# Created by Laura Nicole Stewart
# And Chris Beirne
# laura.nicole.stewart@gmail.com 

#updated for 2023 WildTrax data download formats by Eric Jolin
#ejolin@alumni.uoguelph.ca

##################################################################################

# 2024: Used for some data exploration, replaced for analysis by more streamlined data download process in 1_WildRTrax_Data_Download.R

##################################################################################

#### 0. Set up and load data ####

library(lubridate)
library(dplyr)
library(tidyr)
library(mefa4)
library(stringr)
library(tidyverse)

version<-"v14"
project<-"TDN" # version and project for naming saved csvs

independent<- 30 
# Set the "independence" interval in minutes

#new WT download format split things up between two different reports and doesn't include species_rank or species_class
#new new WT download format created several new csv's, main_report is a more detailed version of tag_report
tag_data_raw<-read.csv("Raw_Data/TDN_RawData_27112023/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_main_report.csv")
length(unique(tag_data_raw$location))

#####0.1. filter outlier photos from the raw data downloads####

#BMS-CRU-004-01 had one single photo taken on 2021-03-03 11:24:10 before being deployed in August 2021
tag_data_tmp<-filter(tag_data_raw,!((image_id == 60289868) & #filter image by image_id 
          (image_date_time == " 2021-03-03 11:24:10"))) #and image_date_time
stopifnot(nrow(tag_data_tmp)==(nrow(tag_data_raw)-1)) #stop if the new df isn't smaller by one row

#BIO-TDN-029-02 wasn't retrieved until 2023 but staff walked passed it on 2022-08-19 13:30:10
tag_data_tmp2<-filter(tag_data_tmp,!((location == "BIO-TDN-029-02") & #filter by location
          (as_datetime(image_date_time)>as_datetime("2022-08-19 13:30:10")))) #filter out images taken after 2022-08-19 13:30:10
stopifnot(nrow(tag_data_tmp2)==(nrow(tag_data_tmp)-1076)) #stop if the new df isn't smaller by the correct number of tags

#long term deployments retrieved early, not comparable with other data

#BIO-TDN-052-06 filter out all images after 2022-08-17 16:57:50
tag_data_tmp3<-filter(tag_data_tmp2,!((location == "BIO-TDN-052-06") & #filter by location
                                       (as_datetime(image_date_time)>as_datetime("2022-08-17 16:57:50")))) #filter out images taken after date/time
stopifnot(nrow(tag_data_tmp3)==(nrow(tag_data_tmp2)-631)) #stop if the new df isn't smaller by the correct number of tags

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

#repeat filters with image dataset
image_data_raw<-read.csv("Raw_Data/TDN_RawData_27112023/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_image_report.csv")
length(unique(image_data_raw$location))

#BMS-CRU-004-01 had one single photo taken on 2021-03-03 11:24:10 before being deployed in August 2021
image_data_tmp<-filter(image_data_raw,!((image_id == 60289868) & #filter image by image_id 
                      (image_date_time == " 2021-03-03 11:24:10"))) #and image_date_time
stopifnot(nrow(image_data_tmp)==(nrow(image_data_raw)-1)) #stop if the new df isn't smaller by one row

#BIO-TDN-029-02 wasn't retrieved until 2023 but staff walked passed it on 2022-08-19 13:30:10
image_data_tmp2<-filter(image_data_tmp,!((location == "BIO-TDN-029-02") & #filter by location
                  (as_datetime(image_date_time)>as_datetime("2022-08-19 13:30:10")))) #filter images taken after 2022-08-19 13:30:10
stopifnot(nrow(image_data_tmp2)==(nrow(image_data_tmp)-1064)) #stop if the new df isn't smaller by 1064

#long term deployments retrieved early, not comparable with other data

#BIO-TDN-052-06 filter out all images after 2022-08-17 16:57:50
image_data_tmp3<-filter(image_data_tmp2,!((location == "BIO-TDN-052-06") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 16:57:50")))) #filter out images taken after date/time
stopifnot(nrow(image_data_tmp3)==(nrow(image_data_tmp2)-1289)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-052-07 filter out all images after 2022-08-17 16:55:08
image_data_tmp4<-filter(image_data_tmp3,!((location == "BIO-TDN-052-07") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 16:55:08")))) #filter out images taken after date/time
stopifnot(nrow(image_data_tmp4)==(nrow(image_data_tmp3)-1253)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-165-01 filter out all images after 2022-08-17 11:58:18
image_data_tmp5<-filter(image_data_tmp4,!((location == "BIO-TDN-165-01") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 11:58:18")))) #filter out images taken after date/time
stopifnot(nrow(image_data_tmp5)==(nrow(image_data_tmp4)-3422)) #stop if the new df isn't smaller by the correct number of tags

#BIO-TDN-165-06 filter out all images after 2022-08-17 13:37:20
image_data_tmp6<-filter(image_data_tmp5,!((location == "BIO-TDN-165-06") & #filter by location
                                        (as_datetime(image_date_time)>as_datetime("2022-08-17 13:37:20")))) #filter out images taken after date/time
stopifnot(nrow(image_data_tmp6)==(nrow(image_data_tmp5)-1127)) #stop if the new df isn't smaller by the correct number of tags

#remove the following stations completely

#BIO-TDN-HF-G1, BIO-TDN-HF-G2, BIO-TDN-HF-LF, BIO-TDN-HF-P1, BIO-TDN-HF-R1 all new sites which were added then removed. 
image_data<- image_data_tmp6 %>% filter(!location %in% locations_to_filter)

length(unique(image_data$location)) #check to make sure the HF locations got filtered, should be 307

rm(image_data_tmp,image_data_tmp2,image_data_tmp3,image_data_tmp4,image_data_tmp5,image_data_tmp6) #remove image_data_tmp to clean environment

rm(locations_to_filter) #remove hf location list to clean things up

#image_snow_depth_m and image_water_depth_m recorded differently in each dataset, remove to try to fix
tag_data<-tag_data %>% 
  select(-image_snow_depth_m,-image_water_depth_m)

image_data<-image_data %>% 
  select(-image_snow_depth_m,-image_water_depth_m)

species_rank_class_data<-read.csv("Raw_Data/TDN_CAM_Species_rank_class_v2.csv")

spat_cov_data<-read.csv("Processed_Data/TDN_camera_locations_and_covariates_v14.csv")

#merge
data<-left_join(tag_data,image_data,by=c('project_id','location','location_id','image_id','image_set_id','image_date_time',
                                     'image_fov','equipment_serial','image_snow','image_in_wildtrax'))

#check to make sure the image_id column is unique
stopifnot(length(unique(image_data$image_id))==nrow(image_data))

#should just be able to use these columns but dplyr still creates duplicate columns for omitted shared headings
#data<-left_join(tag_data,image_data,by=c('project_id','location','location_id','image_id'))

#change capitization of scientific name
data$species_scientific_name<-str_to_sentence(data$species_scientific_name)

#add species_rank and species_class columns to keep everything equal and allow data exploration later on
data<-left_join(data,species_rank_class_data,by=c("species_scientific_name","species_common_name"))

head(data) 
tail(data) # the way this is ordered makes no sense to me but it doesn't really matter
str(data) # pretty much everything is a character "chr"
table(data$equipment_serial) # how odd

# Load pantheria data for mammal species traits:
# Download the txt file from the UO - Biostats Github:
# https://github.com/UO-Biostats/UO_ABS/tree/master/CLASS_MATERIALS/Datasets/PanTHERIA 
pantheria <- read.delim("Raw_Data/PanTHERIA_WR05_mammals.txt")

# Load clements checklist for bird species taxonomy:
# https://www.birds.cornell.edu/clementschecklist/download/
birdfam<-read.csv("Raw_Data/Clements-Checklist-v2022-October-2022.csv")


#### 1. Clean data and save species list ####

data$location<-as.factor(data$location) # tell R location is a factor not a character

length(unique(data$location))

data$image_fov<-as.factor(data$image_fov)
data$age_class<-as.factor(data$age_class)
data$sex_class<-as.factor(data$sex_class)
data$count<-as.numeric(data$individual_count) # as NUMERIC this time
data$individual_count<-NULL
data$image_fov<-as.factor(data$image_fov)
summary(data$image_fov)

# Replace the funny Wildtrax VNA with a proper NA
data[data == "VNA"]<-NA

as.POSIXct(head(data$image_date_time))
as.POSIXct(head(data$image_date_time), tz = "MST") # these were deployed in MST
data$image_date_time = as.POSIXct(data$image_date_time, tz = "MST")

str(data)
summary(data)

#standardize capitalization
#data$common_name <- str_to_title(data$common_name)
#str_to_sentence(data$common_name)

table(data$species_common_name)

data[data$species_common_name == "Beaver","species_common_name"] <- "American Beaver"
data[data$species_common_name == "Gray Jay", "species_common_name"] <- "Canada Jay"
data[data$species_common_name == "Bald eagle", "species_common_name"] <- "Bald Eagle"

# change barren-ground to just caribou
data[data$species_common_name == "Barren-ground Caribou","species_common_name"] <- "Caribou"
data[data$species_common_name == "Caribou","species_scientific_name"] <- "Rangifer tarandus"
data[data$species_common_name == "Caribou","species_rank"] <- "Species"

# change capitalization of  order name
data$species_class <- str_to_title(data$species_class)

data$species_common_name<-as.factor(data$species_common_name)
data=arrange(data,location,image_date_time)

spp <- data %>% 
  filter(species_rank %in% c("Species", "Subspecies")) %>%
  select(species_common_name, species_scientific_name, species_class) %>%
  unique() %>%
  arrange(desc(species_class), species_common_name) %>%
  as.data.frame()

# merge with species traits data
# fix family column in clements
head(birdfam$family)
pos=str_locate(birdfam$family, " " )[,2]
str_sub(birdfam$family, start = 1, end = pos-1)
birdfam$family<-str_sub(birdfam$family, start = 1, end = pos-1)

birdfam$mass_g <- rep(NA, nrow(birdfam))

birds<-birdfam[birdfam$scientific.name %in% data$species_scientific_name,
               c("scientific.name","order","family", "mass_g")]

mammals<-pantheria[pantheria$MSW05_Binomial %in% data$species_scientific_name, 
                   c("MSW05_Binomial","MSW05_Order", "MSW05_Family","X5.1_AdultBodyMass_g")]

str(birdfam)
str(mammals)
names(mammals) = names(birds)

traits<-rbind(birds,mammals)
traits

spp<- merge(spp, traits, by.x="species_scientific_name", by.y = "scientific.name", all.x = T)

# need order and family for a few species
##### NOTE: this section is a bit of a relic and you probably shouldnt need it apart from arctic ground squirrel#####

#spp[spp$common_name == "Spruce Grouse","order"] <- "Galliformes"
#spp[spp$common_name == "Spruce Grouse","family"] <- "Phasianidae"
spp[spp$species_common_name == "Arctic Ground Squirrel","order"] <- "Rodentia"
spp[spp$species_common_name == "Arctic Ground Squirrel","family"] <- "Sciuridae"
#spp[spp$common_name == "American Robin","family"] <- "Turdidae"
#spp[spp$common_name == "Bald eagle","family"] <- "Accipitridae"
#spp[spp$common_name == "Canada Goose","family"] <- "Anatidae"
#spp[spp$common_name == "Canada Jay","family"] <- "Corvidae"
#spp[spp$common_name == "Common Raven","family"] <- "Corvidae"
#spp[spp$common_name == "Common Redpoll","family"] <- "Fringillidae"
#spp[spp$common_name == "Dark-eyed Junco","family"] <- "Passerellidae"
#spp[spp$common_name == "Greater White-fronted Goose","family"] <- "Anatidae"
#spp[spp$common_name == "Green-winged Teal","family"] <- "Anatidae"
#spp[spp$common_name == "Harris's Sparrow","family"] <- "Passerellidae"
#spp[spp$common_name == "Hermit Thrush","family"] <- "Turdidae"
#spp[spp$common_name == "Lapland Longspur","family"] <- "Calcariidae"
#spp[spp$common_name == "Mallard","family"] <- "Anatidae"
#spp[spp$common_name == "Northern Flicker","family"] <- "Picidae"
#spp[spp$common_name == "Northern Harrier","family"] <- "Accipitridae"
#spp[spp$common_name == "Northern Pintail","family"] <- "Anatidae"
#spp[spp$common_name == "Rock Ptarmigan","family"] <- "Phasianidae"
#spp[spp$common_name == "Rusty Blackbird","family"] <- "Icteridae"
#spp[spp$common_name == "Sharp-tailed Grouse","family"] <- "Phasianidae"
#spp[spp$common_name == "Snow Bunting","family"] <- "Calcariidae"
#spp[spp$common_name == "Snow Goose","family"] <- "Anatidae"
#spp[spp$common_name == "Spruce Grouse","family"] <- "Phasianidae"
#spp[spp$common_name == "Whimbrel","family"] <- "Scolopacidae"
#spp[spp$common_name == "White-crowned Sparrow","family"] <- "Passerellidae"
#spp[spp$common_name == "Willow Ptarmigan","family"] <- "Phasianidae"
#spp[spp$common_name == "Yellow-rumped Warbler","family"] <- "Parulidae"

# now we need to fill in all the masses for birds and arctic ground squirrel

spp<-arrange(spp,desc(species_class),species_common_name)
spp

spp[spp$species_common_name == "American Golden-Plover",6] <- 120
spp[spp$species_common_name == "American Tree Sparrow",6] <- 21
spp[spp$species_common_name == "Horned Lark",6] <- 31
spp[spp$species_common_name == "Northern Hawk Owl",6] <- 320
spp[spp$species_common_name == "Sharp-shinned Hawk",6] <- 151
spp[spp$species_common_name == "Song Sparrow",6] <- 19
spp[spp$species_common_name == "Parasitic Jaeger",6] <- 454
spp[spp$species_common_name == "Sandhill Crane",6] <- 3460
spp[spp$species_common_name == "American Robin",6] <- 77
spp[spp$species_common_name == "American Wigeon",6] <- 720
spp[spp$species_common_name == "Arctic Ground Squirrel",6] <- 800
spp[spp$species_common_name == "Canada Goose",6] <- 4500
spp[spp$species_common_name == "Common Raven",6] <- 1200
spp[spp$species_common_name == "Dark-eyed Junco",6] <- 19
spp[spp$species_common_name == "Canada Jay",6] <- 70
spp[spp$species_common_name == "Green-winged Teal",6] <- 350
spp[spp$species_common_name == "Hermit Thrush",6] <- 31
spp[spp$species_common_name == "Mallard",6] <- 1100
spp[spp$species_common_name == "Northern Flicker",6] <- 130
spp[spp$species_common_name == "Northern Harrier",6] <- 420
spp[spp$species_common_name == "Northern Pintail",6] <- 800
spp[spp$species_common_name == "Rock Ptarmigan",6] <- 420
spp[spp$species_common_name == "Rusty Blackbird",6] <- 60
spp[spp$species_common_name == "Sharp-tailed Grouse",6] <- 880
spp[spp$species_common_name == "Spruce Grouse",6] <- 460
spp[spp$species_common_name == "White-crowned Sparrow",6] <- 29
spp[spp$species_common_name == "Willow Ptarmigan",6] <- 550
spp[spp$species_common_name == "Yellow-rumped Warbler",6] <- 12.3
spp[spp$species_common_name == "Bald Eagle",6] <- 4127
spp[spp$species_common_name == "Common Redpoll",6] <- 14
spp[spp$species_common_name == "Greater White-fronted Goose",6] <- 2495
spp[spp$species_common_name == "Harris's Sparrow",6] <- 35
spp[spp$species_common_name == "Lapland Longspur",6] <- 29
spp[spp$species_common_name == "Snow Bunting",6] <- 35
spp[spp$species_common_name == "Snow Goose",6] <- 3200
spp[spp$species_common_name == "Whimbrel",6] <- 390
spp[spp$species_common_name == "Cackling Goose",6] <- 1900
spp[spp$species_common_name == "American Wigeon",6] <- 1000



write.csv(spp, paste0(project, "_CAM_Species_List_", version, ".csv" ), row.names = F)


#### 2. Create deployment dataframe using START and END tags ####

deployment_data = data[,c("project_id", "location","image_date_time","image_fov")]
deployment_data = deployment_data %>%
  subset(image_fov == "START - First Good Image in FOV" | image_fov == "END - Last Good Image in FOV")
deployment_data=arrange(deployment_data,location,image_date_time)  
head(deployment_data)
deployment_data=unique(deployment_data) #START and END will be duplicated if there
#                                         is more than one tag on those images

# now bring in the first and last images into this data frame
# in case they haven't been tagged as START or END

# start with the first row
firstnlast=data[1,c("project_id", "location","image_date_time","image_fov")]

# now add every row where the station name changes over
f.l.data<-data[data$image_fov != "Out of Range",] #except not the out of range images
i=1
for (i in 2: nrow(f.l.data)){
  if (f.l.data$location[i] != f.l.data$location[i-1]){
    firstnlast=rbind(firstnlast,f.l.data[(i-1):i,c("project_id", "location","image_date_time","image_fov")])
  }
}
# now add the last last row
firstnlast=rbind(firstnlast,f.l.data[nrow(f.l.data),c("project_id", "location","image_date_time","image_fov")]) # last row

firstnlast=firstnlast %>% 
  mutate(new.FOV = rep(c("START - First Good Image in FOV", "END - Last Good Image in FOV"),(nrow(firstnlast))/2)) %>%
  mutate(image_fov = new.FOV)%>%
  select(-new.FOV)


summary(firstnlast)
summary(table(firstnlast$location)==2) # should all be TRUE

deploy_full=rbind(deployment_data,firstnlast)
head(deploy_full)
deploy_full=arrange(deploy_full,location,image_date_time)

dups=deploy_full[duplicated(deploy_full),]
deploy_full=unique(deploy_full)

# time to look for mistakes again

mistakes.fieldofview=deploy_full[1,]
mistakes.fieldofview[1,]<-rep(NA,4)

for (i in 2: nrow(deploy_full)){
  if (deploy_full$image_fov[i] == deploy_full$image_fov[i-1]){
    mistakes.fieldofview=rbind(mistakes.fieldofview,deploy_full[(i-1):i,])
  }
}

mistakes.fieldofview

# add a deployment ID in order to turn long format into wide format
#####NOTE: Could change paste0 function to change deployment IDs to a more useful format#####

deploy_full$deployment_id = rep(NA,nrow(deploy_full))
sort(rep(paste0("d",1:(nrow(deploy_full)/2)),2)) # sure, whatever
ident<-sort(rep(paste0("d",1:(nrow(deploy_full)/2)),2))
deploy_full$deployment_id = ident

summary(table(deploy_full$location)%%2 == 0) # do all stations have an even number of rows?
# if any are false, then the answer is no, and there is a problem
m=table(deploy_full$location)%%2 == 0
m[m==FALSE] #which ones have errors?

head(spread(deploy_full,image_fov,image_date_time))
str(deploy_full)

deploy_wide<-deploy_full%>%
  group_by(image_fov)%>%
  mutate(row=row_number())%>%
  pivot_wider(
    names_from = c(image_fov),
    values_from = image_date_time
  ) %>%
  select(-row)%>%
  select(c(project_id,location,`START - First Good Image in FOV`,`END - Last Good Image in FOV`,
           deployment_id))%>%
  as.data.frame()

str(deploy_wide)

# change variable names to standardized
names(deploy_wide)<- c("project_id",	"location",
                       "deployment_begin_date",	"deployment_end_date",
                       "deployment_id")

deploy_wide = deploy_wide %>% mutate(duration = deployment_end_date - deployment_begin_date)
class(deploy_wide$duration)

#####NOTE: these scripts designed to get something more similar to the wildlife insights metadata may be messing things up####
#change deploy_id into something more useful and similar to CMI workshop
#deploy_wide$deployment_id<-paste0(deploy_wide$location,"_",deploy_wide$deployment_begin_date)

#merge with covariate data to make it more similar to CMI workshop
#deploy_wide_cov<-merge(deploy_wide,spat_cov_data,by="location")

#write.csv(deploy_wide_cov, paste0(project,"_CAM_Deployment_Data_", version, ".csv"),row.names = F)


#write.csv(deploy_wide, paste0(project,"_CAM_Deployment_Data_", version, ".csv"),row.names = F)


#### 3. Clean data and variable names for detection data ####

str(data)

# there's a lot of stuff coming out of Wildtrax that I don't want
# Choose below which variables to remove:

lessdata = data %>%
  select(-project_id,-location_buffer_m,-image_in_wildtrax,-image_fire,-image_nice,-image_snow,-behaviours,-health_diseases,
         -coat_colours,-coat_attributes,-tine_attributes,-direction_travel,-has_collar,-has_eartag,
         -ihf,-observer_id,-source_file_name,-equipment_make,-equipment_model, -equipment_serial,-image_is_blurred,-media_url)

str(lessdata)

#####NOTE: these scripts designed to get something more similar to the wildlife insights metadata may be messing things up####
#####NOTE: need to figure out way to join deployment_id to detection data and independent detection data. Maybe using date detected or something.####
#lessdata<-merge(lessdata,deploy_wide,by=c("project","location"))
#lessdata<-lessdata %>% select(-duration, -deployment_begin_date, -deployment_end_date)

#write.csv(lessdata, paste0(project, "_CAM_Detection_Data_", version,".csv" ), row.names=F)


#### 4. Independent detections and group count ####

#####NOTE: this csv doesn't seem to work if the codes above modified for the CMI workshop analyses aren't hashtagged out#####
#####NOTE: for some reason the FOV tags are entered into the group_count column####

# Remove observations without animals detected
dat <- lessdata[lessdata$species_common_name!="NONE",]

# Order the dataframe by location, date
dat <- dat[order(dat$location, dat$image_date_time),]
head(dat)

dat <- dat %>%
  arrange(location, image_date_time) %>%
  group_by(location, species_common_name) %>%
  mutate(duration = int_length(lag(image_date_time) %--% image_date_time) )

str(dat)

# loop that assigns group ID
dat$event_id <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$event_id[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$event_id[nrow(dat)] <- dat$event_id[nrow(dat)-1]
} else{
  dat$event_id[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}

# Minimum group size at the tag level (includes age and sex)

tpos<-str_locate(dat$tag_comments, "gc")[,2]
tmptag=str_sub(dat$tag_comments, start = tpos+1, end = tpos+3)
tendpos=str_locate(tmptag,"\\D")[,2]
tendpos[is.na(tendpos)]<-2
dat$groupcount_tag<- str_sub(tmptag,start = 1, end = tendpos-1)

# Now collapse the thing into independent events
class(dat$count) # should be numeric

levels(dat$image_fov)
dat$image_fov <- factor(dat$image_fov, ordered = T , levels = c("WITHIN","START - First Good Image in FOV",
                                     "END - Last Good Image in Field Of View","Out of Range"))

events<-dat %>%
  # first group by event and the other variables we want to keep
  dplyr::group_by(event_id, species_common_name, species_scientific_name,age_class, sex_class) %>%
  # then take group count as the max of the comments gc# count (gc_tag)
  # and seperately, as the max of the individual image count (gc_regular)
  dplyr::summarise (gc_tag = max(groupcount_tag, na.rm = T),
                    gc_regular = max(count, na.rm = T),
                    fov = min(image_fov, na.rm = T))
# replace impossible values with NAs
events$gc_regular[events$gc_regular == -Inf]<-NA

for (i in 1: nrow(events)){
  if (is.na(events$gc_tag [i])){ # where there was no tag comment
    events$gc_tag[i] <- events$gc_regular[i] # fill in group count from gc_regular
  }
}

events$gc_tag<-as.numeric(events$gc_tag)

names(events)[8]
names(events)[8]<-"group_count"
events <- select(events, -gc_regular) # remove the now redundant group count column

# Calculate the event length and size 

# find out the last and the first of the time in the group
top <- dat %>% group_by(event_id) %>% top_n(1,image_date_time) %>% select(event_id, image_date_time)
bot <- dat %>% group_by(event_id) %>% top_n(-1,image_date_time) %>% select(event_id, image_date_time)
names(bot)[2] <- c("date_detected_end")
dec_no <- dat %>% group_by(location, event_id) %>% summarise(n())
names(dec_no)[3]<-"number_images"

# calculate the duration
diff <-  top %>% left_join(bot, by="event_id") %>%
  mutate(duration=abs(int_length(image_date_time %--% date_detected_end))) %>%
  left_join(dec_no, by="event_id")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

# Merge the data
independent_data <-  merge(diff, events, by = "event_id", all.y = T)
str(independent_data)

# rename group_count to image_fov, not sure why this one ended up like this...
independent_data<-independent_data %>% 
  rename("image_fov"="group_count")

# Save it for a rainy day
# write.csv(independent_data, paste0(project,"_CAM_Independent_Detections_",version, ".csv"), row.names = F)

#### 5. Independent detections at the site level ####

#the above csv calculates independent detections across all cameras, rather than across camera sites/clusters. 
#This means there could be detections within 30min of each other at two cameras at the same site that are being labelled as independent.
#We assume non-independence between cameras within the same site to partially account for imperfect detection. 

