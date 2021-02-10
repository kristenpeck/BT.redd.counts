# This script includes some basic QA of Bull Trout redd data loaded into a 
# spreadsheet by Triton Env. in 2019/2020

#Author: Kristen Peck

#Date 3-April-2020

#Packages
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(sf)
library(mapview)


#### QA- Visits ####

# Load data

visits <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Visit")

str(visits)

#regions included in visits - should only be Omineca and Peace
unique(visits$Region)

#all site IDs should be unique. Any duplicates?
anyDuplicated(visits$Site_ID)

#distribution of # of surveys over the years by region
ggplot(data=visits)+
  geom_histogram(aes(Year, fill=Region), binwidth=2)

#surveys where date is missing:
(no_date <- visits %>% 
  mutate(date = ymd(paste(Year,Month,Day))) %>% 
  filter(is.na(date)))


#surveys in the Peace where method is missing
no_method_peace <- visits %>% 
  filter(is.na(`method (walk/flight)`)) %>% 
  filter(Region %in% "Peace")
no_method_peace




#### QA- Barriers ####

barrier <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Barrier")

str(barrier)

#check that site IDs have a match in the visits table. If this comes up with 0 data, there are no stragglers

barrier %>% 
  anti_join(visits, by="Site_ID")

#any without a location?

barrier %>% 
  filter(is.na(UTME))

#any need UTM zone filled in? 

barrier %>% 
  filter(is.na(UTMZ))

unique(barrier$UTMZ) #need to read in as UTM zones individually, then transform to BC Albers
#map barriers:
#UTM zone 9 is 3156
#UTM zone 10 is 3157
#UTM zone 11 is 2955


UTM9 <- barrier %>% 
  filter(UTMZ %in% 9) 
bar_pts9 <- st_as_sf(UTM9, coords = c("UTME", "UTMN"), 
                        crs = 3156)
bar_pts9 <- st_transform(bar_pts9, crs=3005)

UTM10 <- barrier %>% 
  filter(UTMZ %in% 10)
bar_pts10 <- st_as_sf(UTM10, coords = c("UTME", "UTMN"), 
                      crs = 3157)
bar_pts10 <- st_transform(bar_pts10, crs=3005)

barrier_pts <- st_union(bar_pts9,bar_pts10)



mapview(barrier_pts)


#### QA- Counts ####

counts <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Counts")

str(counts)

#check that site IDs have a match in the visits table. If this comes up with 0 data, 
# there are no stragglers

counts %>% 
  anti_join(visits, by="Site_ID")



### MAPS ####
#any without a location?

counts %>% 
  filter(is.na(UTME))

#any need UTM zone filled in? 

counts %>% 
  filter(!is.na(UTME) & is.na(UTMZ))
counts$UTMZ <- ifelse(is.na(counts$UTME), NA, counts$UTMZ)
counts %>% 
  filter(is.na(UTME) & !is.na(UTMZ))
unique(counts$UTMZ) #need to read in as UTM zones individually, then transform to BC Albers
#UTM zone 9 is 3156
#UTM zone 10 is 3157
#UTM zone 11 is 2955

UTM9 <- counts %>% 
  filter(UTMZ %in% 9) 
count_pts9 <- st_as_sf(UTM9, coords = c("UTME", "UTMN"), 
                     crs = 3156)
count_pts9 <- st_transform(count_pts9, crs=3005)


UTM10 <- counts %>% 
  filter(UTMZ %in% 10)
count_pts10 <- st_as_sf(UTM10, coords = c("UTME", "UTMN"), 
                      crs = 3157)
count_pts10 <- st_transform(count_pts10, crs=3005)


UTM11 <- counts %>% 
  filter(UTMZ %in% 11)
count_pts11 <- st_as_sf(UTM11, coords = c("UTME", "UTMN"), 
                        crs = 2955)
count_pts11 <- st_transform(count_pts11, crs=3005)

count_pts1 <- st_join(count_pts10,count_pts11)
count_pts <- st_join(count_pts1, count_pts9)

mapview(count_pts)


#create table with years vs. streams with total redds

visit.yr <- visits %>% 
  filter(Region %in% "Peace") %>% 
  mutate(date = ymd(paste(Year,Month,Day))) %>% 
  arrange(date) %>% 
  left_join(counts, by="Site_ID") %>% 
  mutate(all.redds = Complete_REDDCount+`Incomplete_REDD Count`+`Any Redd`) %>% 
  dplyr::group_by(Location, Year) %>% 
  dplyr::summarise(survey.yrs = unique(Year), start.date = date[1], 
                   total.redds = sum(all.redds),
                   total.test.redds = sum(`Test Redd`)) %>% 
  arrange(Location)

write.csv(visit.yr, "visit.yr.csv", row.names = F)











