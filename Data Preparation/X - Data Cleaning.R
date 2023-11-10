
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
#install.packages("pacman")
pacman::p_load("readxl","dplyr", "tidyr", "lubridate", "ggplot2", "hrbrthemes", "ggmap", "broom", "geojsonio", "maptools", "sf")

# Load data
## Load internal data
load("./Data/Climate Data/fwi_1980_2023_states.RData")
fwi <- fwi_1980_2023
iod <- read.table("./Data/Climate Data/bom.gov.au_climate_enso_dmi-monthly.txt", sep=",", col.names = c("Date","IOD"))
soi <- read.table("./Data/Climate Data/bom.gov.au_climate_enso_soi_monthly.txt", sep=",", col.names = c("Date","SOI"))

## Load external data
### ABS National Land Cover Account: https://www.abs.gov.au/statistics/environment/environmental-management/national-land-cover-account/latest-release#data-downloads
land_cover <- read_excel("./Data/Geographic Data/46162DO010_2020.xlsx", range="Table 10.2!A6:AJ102")
### Climate Change in Australia: https://www.climatechangeinaustralia.gov.au/en/obtain-data/download-datasets/#Hist
climate <- read.table("./Data/Cleaned Data/climate_data.csv", sep=",", header=TRUE)
### ABD Digital Boundary Files: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
aus_state_shp <- read_sf("./Data/Geospatial Data/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")

# Convert date formats
fwi$date <- ymd(fwi$date)
iod$Date <- ymd(paste0(substr(iod$Date, start=1, stop=6),"01"))
soi$Date <- ymd(paste0(soi$Date,"01"))
climate$Date <- ymd(climate$Date)

# Clean data
## fwi
### 1. Remove all null values in state
fwi <- fwi %>% 
  filter(!is.na(state))

### 2. Find the 99th percentile of fwi for each state
#### 2A. Test
# fwi_nsw <- fwi %>% 
#   filter(state == "New South Wales")
# 
# result <- quantile(fwi_nsw$fwi, 0.99)

#### 2B. Define function filter_and_quantile 
filter_and_quantile <- function(state_name, percentile=0.90, data=fwi){
  data_state <- data %>% 
    filter(state == state_name)
  percentile_value <- as.numeric(quantile(data_state$fwi, percentile))
  return(percentile_value)
}

#### 2C. Apply function filter_and_quantile 
states <- unique(fwi$state)
fwi_99 <- as.numeric(sapply(states,filter_and_quantile))
fwi_states_99 <- data.frame(State = states, FWI_99th = fwi_99)

fwi_95 <- as.numeric(sapply(states,filter_and_quantile))
fwi_states_95 <- data.frame(State = states, FWI_95th = fwi_95)

fwi_90 <- as.numeric(sapply(states,filter_and_quantile))
fwi_states_90 <- data.frame(State = states, FWI_90th = fwi_90)

# fwi_nsw_99 <- filter_and_quantile("New South Wales", 0.99, fwi)
# fwi_nt_99 <- filter_and_quantile("Northern Territory", 0.99, fwi)
# fwi_qld_99 <- filter_and_quantile("Queensland", 0.99, fwi)
# fwi_sa_99 <- filter_and_quantile("South Australia", 0.99, fwi)
# fwi_wa_99 <- filter_and_quantile("Western Australia", 0.99, fwi)
# fwi_tas_99 <- filter_and_quantile("Tasmania", 0.99, fwi)
# fwi_vic_99 <- filter_and_quantile("Victoria", 0.99, fwi)

### 3. Find the 99th percentile of fwi for each state
fwi_state <- fwi %>% 
  group_by(date, state) %>% 
  summarise(FWI_mean = mean(fwi),
            FWI_median = median(fwi),
            FWI_max = max(fwi)) %>% 
  left_join(fwi_states_99, by=join_by("state"=="State")) %>% 
  left_join(fwi_states_95, by=join_by("state"=="State")) %>% 
  left_join(fwi_states_90, by=join_by("state"=="State")) %>% 
  mutate(FWI_99th_flag = FWI_max >= FWI_99th) %>% 
  mutate(FWI_95th_flag = FWI_max >= FWI_95th) %>% 
  mutate(FWI_90th_flag = FWI_max >= FWI_90th)


## iod & soi
### Assumption 2: assume that the iod and soi indicies given are at the start of the year-month and linearly interpolates the iod and soi indicies for the rest of the dates
iod_2 <- data.frame(Date = seq(min(iod$Date),max(iod$Date), by="days"))
iod_2$YearMonthStartDate <- ymd(year(iod_2$Date)*10000+month(iod_2$Date)*100+1)
iod_2$YearMonthEndDate <- add_with_rollback(iod_2$YearMonthStartDate, months(1), roll_to_first=TRUE)
iod_2 <- iod_2 %>% 
  left_join(iod, by=join_by(YearMonthStartDate==Date)) %>% 
  rename("IOD_Start"="IOD") %>%
  left_join(iod, by=join_by(YearMonthEndDate==Date)) %>% 
  rename("IOD_End"="IOD") %>%
  mutate(DaysFromStart = as.numeric(Date-YearMonthStartDate)) %>% 
  mutate(DaysUntilEnd = as.numeric(YearMonthEndDate- Date)) %>%
  mutate(IOD_adjusted = (IOD_Start*DaysUntilEnd+IOD_End*DaysFromStart)/(DaysUntilEnd+DaysFromStart)) %>%
  select(Date, IOD_adjusted) %>% 
  rename(IOD = IOD_adjusted)

soi_2 <- data.frame(Date = seq(min(soi$Date),max(soi$Date), by="days"))
soi_2$YearMonthStartDate <- ymd(year(soi_2$Date)*10000+month(soi_2$Date)*100+1)
soi_2$YearMonthEndDate <- add_with_rollback(soi_2$YearMonthStartDate, months(1), roll_to_first=TRUE)
soi_2 <- soi_2 %>% 
  left_join(soi, by=join_by(YearMonthStartDate==Date)) %>% 
  rename("SOI_Start"="SOI") %>%
  left_join(soi, by=join_by(YearMonthEndDate==Date)) %>% 
  rename("SOI_End"="SOI") %>%
  mutate(DaysFromStart = as.numeric(Date-YearMonthStartDate)) %>% 
  mutate(DaysUntilEnd = as.numeric(YearMonthEndDate- Date)) %>%
  mutate(SOI_adjusted = (SOI_Start*DaysUntilEnd+SOI_End*DaysFromStart)/(DaysUntilEnd+DaysFromStart)) %>%
  select(Date, SOI_adjusted) %>% 
  rename(SOI = SOI_adjusted)

## land_cover
### 1. Filter out null values and filter out ACT
land_cover <- land_cover %>% 
  filter((`State Name` != "Australian Capital Territory") & (`Land Cover Classification` != "No data"))

land_cover_reformatted <- land_cover %>% 
  pivot_longer(
    cols = !c("State Code", "State Name", "Land Cover Classification"),
    names_to = "Year",
    values_to = "Area"
  ) %>% 
  pivot_wider(names_from = `Land Cover Classification`,
              values_from = Area)


### 2. Convert land cover into percentage of land area covered by a. Artificial_surfaces, b. Cultivated_terrestrial_vegetated, c. Natural_terrestrial_vegetated and d. Water.
land_cover_state <- land_cover_reformatted %>% 
  select(c("State Name"
           ,"Year"
           ,"Artificial surfaces"
           ,"Cultivated terrestrial vegetated: herbaceous"
           ,"Natural terrestrial vegetated: herbaceous"
           ,"Natural terrestrial vegetated: woody"
           ,"Natural surfaces"
           ,"Natural aquatic vegetated: herbaceous"
           ,"Natural aquatic vegetated: woody"
           ,"Water: perennial"
           ,"Water: non-perennial"
           ,"Tidal area"
           ,"Total")) %>% 
  mutate_at(c("Artificial surfaces"
              ,"Cultivated terrestrial vegetated: herbaceous"
              ,"Natural terrestrial vegetated: herbaceous"
              ,"Natural terrestrial vegetated: woody"
              ,"Natural surfaces"
              ,"Natural aquatic vegetated: herbaceous"
              ,"Natural aquatic vegetated: woody"
              ,"Water: perennial"
              ,"Water: non-perennial"
              ,"Tidal area"
              ,"Total"), ~replace_na(as.numeric(.),0)) %>% 
  mutate(across(c("Artificial surfaces"
                  ,"Cultivated terrestrial vegetated: herbaceous"
                  ,"Natural terrestrial vegetated: herbaceous"
                  ,"Natural terrestrial vegetated: woody"
                  ,"Natural surfaces"
                  ,"Natural aquatic vegetated: herbaceous"
                  ,"Natural aquatic vegetated: woody"
                  ,"Water: perennial"
                  ,"Water: non-perennial"
                  ,"Tidal area"), ~./Total, .names="{col}")) %>% 
  select(-Total) %>% 
  mutate(Artificial_surfaces = `Artificial surfaces`) %>% 
  mutate(Cultivated_terrestrial_vegetated = `Cultivated terrestrial vegetated: herbaceous`) %>% 
  mutate(Natural_terrestrial_vegetated = `Natural terrestrial vegetated: herbaceous` + `Natural terrestrial vegetated: woody` + `Natural surfaces`) %>% 
  mutate(Water = `Natural aquatic vegetated: herbaceous` + `Natural aquatic vegetated: woody` + `Water: perennial` + `Water: non-perennial` + `Tidal area`) %>% 
  select(-c("Artificial surfaces"
            ,"Cultivated terrestrial vegetated: herbaceous"
            ,"Natural terrestrial vegetated: herbaceous"
            ,"Natural terrestrial vegetated: woody"
            ,"Natural surfaces"
            ,"Natural aquatic vegetated: herbaceous"
            ,"Natural aquatic vegetated: woody"
            ,"Water: perennial"
            ,"Water: non-perennial"
            ,"Tidal area")) %>% 
  rename(State = `State Name`)

# ## climate
# climate_geometry <- climate %>% 
#   select(c("LON","LAT")) %>% 
#   distinct() %>% 
#   st_as_sf(coords = c("LON", "LAT"), crs=4326) %>% # EPSG4326 is WGS84 lat/long
#   st_transform(crs = st_crs(aus_state_shp))
# 
# climate_geocord <- climate %>% 
#   select(c("LON","LAT")) %>% 
#   distinct()
# 
# climate_gg <- cbind(climate_geometry,climate_geocord)
# 
# climate_geocord_state_mapping <- st_join(aus_state_shp, climate_gg, left=FALSE) %>% 
#   select(c("STE_NAME21","LON", "LAT"))
# 
# climate_state <- climate %>% 
#   left_join(climate_geocord_state_mapping, join_by("LON"=="LON", "LAT"=="LAT"))
# 
# climate_state <- climate_state %>% 
#   select(-c("LON", "LAT", "geometry")) %>%
#   rename(state=STE_NAME21) %>%
#   group_by(Date, state) %>% 
#   summarise(max_temp = max(max_temp),
#             min_temp = min(min_temp),
#             mean_temp = (mean(max_temp)+mean(min_temp))/2,
#             max_pan_evaporation = max(pan_evaporation),
#             min_pan_evaporation = min(pan_evaporation),
#             mean_pan_evaporation = mean(pan_evaporation),
#             max_relative_humidity = (max(relative_humidity_9am)>=max(relative_humidity_3pm))*max(relative_humidity_9am) + (max(relative_humidity_9am)<max(relative_humidity_3pm))*max(relative_humidity_3pm),
#             min_relative_humidity = (min(relative_humidity_9am)<=min(relative_humidity_3pm))*min(relative_humidity_9am) + (min(relative_humidity_9am)>min(relative_humidity_3pm))*min(relative_humidity_3pm),
#             mean_relative_humidity = (mean(relative_humidity_9am)+mean(relative_humidity_3pm))/2)
# 



# write.csv(fwi_state, "./Data/Cleaned Data/fwi.csv", row.names=FALSE)
# write.csv(iod_2, "./Data/Cleaned Data/iod.csv", row.names=FALSE)
# write.csv(soi_2, "./Data/Cleaned Data/soi.csv", row.names=FALSE)
# write.csv(land_cover_state, "./Data/Cleaned Data/land_cover.csv", row.names=FALSE)
# write.csv(climate_state, "./Data/Cleaned Data/climate.csv", row.names=FALSE)





X <- fwi_state %>% 
  rename(Date = date) %>% 
  rename(State = state) %>% 
  left_join(iod_2, join_by(Date==Date)) %>% 
  left_join(soi_2, join_by(Date==Date)) %>% 
  mutate(Year = case_when(
    year(Date)<1988 ~ "1988",
    year(Date)>2020 ~ "2020",
    TRUE ~ as.character(year(Date)))) %>% 
  left_join(land_cover_state, join_by(State==State, Year == Year)) %>% 
  select(-Year)
  # left_join(climate_state, join_by('date'=='Date','state'=='state'), copy=TRUE) %>%
  # select(-State.Code) 



write.csv(X, "./Data/Cleaned Data/X.csv", row.names=FALSE)
