
# Load packages
#install.packages("pacman")
pacman::p_load("readxl","dplyr", "lubridate", "ggplot2", "hrbrthemes", "ggmap", "broom", "geojsonio", "maptools")

# Load data
load("fwi_nsw.RData")
fwi <- fwi_nsw
iod <- read.table("bom.gov.au_climate_enso_dmi-monthly.txt", sep=",", col.names = c("Date","IOD"))
soi <- read.table("bom.gov.au_climate_enso_soi_monthly.txt", sep=",", col.names = c("Date","SOI"))
ica_cat_event_history <- read_excel("ICA-Historical-Normalised-Catastrophe-August-2023.xlsx", range="ICA_CAT_Historical!A10:X742")


# Clean data

## Format date columns
fwi$date <- ymd(fwi$date)
iod$Date <- ymd(paste0(substr(iod$Date, start=1, stop=6),"01"))
soi$Date <- ymd(paste0(soi$Date,"01"))
ica_cat_event_history$CAT_Event_Start <- ymd(ica_cat_event_history$CAT_Event_Start)
ica_cat_event_history$CAT_Event_Finish <- ymd(ica_cat_event_history$CAT_Event_Finish)

## Filter for bushfire events only
ica_bushfire_event_history <- ica_cat_event_history %>% 
  filter(Type == "Bushfire")

## Join tables on dates
### Assumption 1: assume that the iod and soi indicies are the same within the same year-month (e.g. iod_2023/10/02 = iod_2023/10/02)
df_1 <- fwi %>% 
  mutate(date_2 = ymd(year(date)*10000+month(date)*100+1)) %>% 
  left_join(iod, by=join_by(date_2==Date)) %>% 
  left_join(soi, by=join_by(date_2==Date))




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
  select(Date,IOD_Start, IOD_adjusted)

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
  select(Date,SOI_Start, SOI_adjusted)

### Visualisation comparing Assumption 1 (blue) & Assumption 2 (red)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#### IOD
ggplot(iod_2 %>% filter(Date>=as.Date("2020-01-01")) %>% rename("Assumption_1" = "IOD_Start") %>% rename("Assumption_2" = "IOD_adjusted"), aes(x=Date) ) +
  geom_line(aes(y=Assumption_1), color=cbPalette[3] , alpha=0.5) +
  geom_line(aes(y=Assumption_2), color=cbPalette[8] , alpha=0.5) +
  theme_ipsum()
#### SOI
ggplot(soi_2 %>% filter(Date>=as.Date("2020-01-01")) %>% rename("Assumption_1" = "SOI_Start") %>% rename("Assumption_2" = "SOI_adjusted"), aes(x=Date) ) +
  geom_line(aes(y=Assumption_1), color=cbPalette[3] , alpha=0.5) +
  geom_line(aes(y=Assumption_2), color=cbPalette[8] , alpha=0.5) +
  theme_ipsum()

df_2 <- fwi %>% 
  left_join(iod_2, by=join_by(date==Date)) %>% 
  left_join(soi_2, by=join_by(date==Date)) %>% 
  select(-c("IOD_Start","SOI_Start"))


# Visualisation
# Maps
