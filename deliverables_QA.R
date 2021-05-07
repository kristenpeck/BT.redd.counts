# This script includes some basic QA of Bull Trout redd data loaded into a 
# spreadsheet by Triton Env. in 2019/2020

#Author: Kristen Peck

#Date 3-April-2020
#Updated 23-March-2021

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

#visits <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Visit")
visits <- read_xlsx("BTSurveyData2019-mod_V3-QA_TedV3.xlsx", sheet = "Visit")


str(visits)

#regions included in visits - should only be Omineca and Peace
unique(visits$Region)

#all site IDs should be unique. Any duplicates?
anyDuplicated(paste0(visits$Site_ID,visits$Survey_ID))
visits[duplicated(paste0(visits$Site_ID,visits$Survey_ID)) %in% TRUE,]

# # of survey rows over the years by region

ggplot(data=visits)+
  geom_histogram(aes(Year, fill=Region), binwidth=1, position = "dodge")+
  labs(y="# of Site Visits") +
  scale_x_continuous(breaks = seq(min(visits$Year), max(visits$Year),2),
                     labels = seq(min(visits$Year), max(visits$Year),2))

#surveys where date is missing:

(no_date <- visits[is.na(ymd(paste(visits$Year,visits$Month,visits$Day))),])


#surveys in the Peace where method is missing
(no_method_peace <- visits %>% 
  filter(is.na(`method (walk/flight)`)) %>% 
  filter(Region %in% "Peace"))




#### QA- Barriers ####

#barrier <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Barrier")
barrier <- read_xlsx("BTSurveyData2019-mod_V3-QA_TedV3.xlsx", sheet = "Barrier")

str(barrier)

#check that site IDs have a match in the visits table. If this comes up with 0 data, 
  #that's good

barrier %>% 
  anti_join(visits, by="Site_ID")

#any without a location?

barrier %>% 
  filter(is.na(UTME))

visits[which(visits$Site_ID %in% barrier[is.na(barrier$UTME),"Site_ID"]),]


#any need UTM zone filled in? 

barrier %>% 
  filter(is.na(UTMZ))


### Map- Barriers ####


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

#counts <- read_xlsx("BTSurveyData2019-mod_V3-QA.xlsx", sheet = "Counts")
counts <- read_xlsx("BTSurveyData2019-mod_V3-QA_TedV3.xlsx", sheet = "Counts")

str(counts)

#check that site IDs have a match in the visits table. If this comes up with 0 data, 
# there are no stragglers

counts %>% 
  anti_join(visits, by="Site_ID")


# join counts and visits

counts.visits <- counts %>% 
  left_join(visits, by="Site_ID") %>% 
  filter(Region %in% "Peace")

### Maps - Counts ####
#any without a location?

counts.visits %>% 
  filter(is.na(UTME))

#any need UTM zone filled in? 

counts.visits %>% 
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
  filter(UTMZ %in% 10, !is.na(UTME))


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

count_pts <- count_pts10

mapview(count_pts)


# library(bcdata)
# 
#  bc <- bcdc_query_geodata('7-5m-provinces-and-states-the-atlas-of-canada-base-maps-for-bc') %>% 
#    filter(ENGLISH_NAME == 'British Columbia') %>% 
#    collect()
#  bc
# 
#   Peace <- bcdc_query_geodata('WHSE_ADMIN_BOUNDARIES.EADM_WLAP_REGION_BND_AREA_SVW') %>% 
#     filter(REGION_NAME == 'Peace') %>% 
#     collect()
#   Peace 
#   
#  EDU.Peace <- bcdc_query_geodata('WHSE_LAND_AND_NATURAL_RESOURCE.EAUBC_ECO_DRAINAGE_UNITS_SP') %>%
#     filter(INTERSECTS(Peace)) %>% 
#     collect()
#   
#  bb.EDU <- st_bbox(EDU.Peace)
#  
#  lks.mapping <- bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') %>%
#   filter(AREA_HA > 10000) %>%
#   #filter(INTERSECTS(Peace)) %>% 
#    collect()
#  
#  
#   ggplot()+
#     geom_sf(data=bc, fill="white")+
#     geom_sf(data=EDU.Peace, fill="grey")+
#     geom_sf(data=Peace, colour = "black",fill="transparent", size=2)+
#     geom_sf(data=lks.mapping, col="blue")+
#     geom_sf_label(data=EDU.Peace, aes(label = ECO_DRAINAGE_UNIT))+
#     coord_sf(xlim = c(bb.EDU$xmin,bb.EDU$xmax), ylim = c(bb.EDU$ymin, bb.EDU$ymax), expand = FALSE)
#   
mapview(Peace)+mapview(count_pts)
  
mapview(Peace, col.regions=NA, legend=F)+mapview(count_pts, zcol = "Watershed", legend = TRUE)
  
established.raw = st_read("Region_9_real_bull_trout_WHAs.shp")
established <- st_transform(established.raw, crs=3005)
established.buff <- st_buffer(established, dist = 1000)


proposed.raw = st_read("wha_9-166to171_Corrected.shp")
proposed <- st_transform(proposed.raw, crs=3005)
proposed.buff <- st_buffer(proposed, dist = 1000)

mapview(Peace, col.regions=NA, legend=F)+mapview(established.buff, lwd=2)+
          mapview(proposed.buff, col.regions="red", lwd=2)
mapview(established.buff, map.types = c("Esri.WorldTerrain"))+
  mapview(proposed.buff, col.regions="red")

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
                   total.test.redds = sum(`Test Redd`),
                   watershed = unique(Watershed)) %>% 
  arrange(Location)

write.csv(visit.yr, "visit.yr.csv", row.names = F)



visit.yr.liard <- visit.yr %>% 
  filter(watershed %in% "Liard")

visit.yr.peace <- visit.yr %>% 
  filter(watershed %in% "Peace")

visit.yr.halfway <- visit.yr %>% 
  filter(watershed %in% "Halfway")

(bar.watershed <- ggplot(visit.yr.peace, aes(x=Year, y=total.redds, fill=Location))+
    geom_bar(position="stack", stat="identity")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size=12), 
          panel.grid.minor = element_blank())+
    scale_x_continuous(breaks =seq(min(visit.yr.peace$Year),max(visit.yr.peace$Year),1))+
    labs(x="Year", y="total redds", 
         fill = "Stream"))


unique(visit.yr$Location)



ggplot(visit.yr.peace)+
  geom_line(aes(x=Year, y=total.redds, colour=Location),
            size=2, na.rm=F)+
  geom_point(aes(x=Year, y=total.redds, colour=Location), size = 2)+
  theme_bw()+
  scale_x_continuous(breaks =seq(min(visit.yr.peace$Year),max(visit.yr.peace$Year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=12),
        panel.grid.minor = element_blank())

ggplot(visit.yr.liard)+
  geom_line(aes(x=Year, y=total.redds, colour=Location),
            size=2, na.rm=F)+
  geom_point(aes(x=Year, y=total.redds, colour=Location), size = 2)+
  theme_bw()+
  scale_x_continuous(breaks =seq(min(visit.yr.liard$Year),max(visit.yr.liard$Year),1))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=12),
        panel.grid.minor = element_blank())



