## Set working directory
# setwd("C:/Users/Callista Surjadi/Downloads")

## Initialise packages
# install.packages("pacman")
pacman::p_load("tidyverse", "readxl")

## Import datasets
### A.Internal data
load("fwi_1980_2023_states.RData")
FWI <- fwi_1980_2023

IOD <- read.table("bom.gov.au_climate_enso_dmi-monthly.txt", sep=",", 
                  col.names = c("Date","IOD"))
SOI <- read.table("bom.gov.au_climate_enso_soi_monthly.txt", sep=",", 
                  col.names = c("Date","SOI"))
ICA_CAT_Hist <- read_excel("ICA-Historical-Normalised-Catastrophe-August-2023.xlsx",
                                    range="ICA_CAT_Historical!A10:X742")

### B.External data
# ABS National Land Cover Account: https://www.abs.gov.au/statistics/environment/environmental-management/national-land-cover-account/latest-release#data-downloads
Land_cover <- read.csv("46162DO011_2020.csv", skip=4, header=TRUE)

## Convert date formats
FWI$date <- ymd(FWI$date)
IOD$Date <- ymd(paste0(substr(IOD$Date, start=1, stop=6),"01"))
SOI$Date <- ymd(paste0(SOI$Date,"01"))
ICA_CAT_Hist$CAT_Event_Start <- ymd(ICA_CAT_Hist$CAT_Event_Start)
ICA_CAT_Hist$CAT_Event_Finish <- ymd(ICA_CAT_Hist$CAT_Event_Finish)

## Data cleaning
### A.Remove null values
FWI <- FWI %>% 
  filter(!is.na(state))

Land_cover <- Land_cover %>% 
  filter((State.Code != 0)&(State.Name != "")&(State.Name != "Australian Capital Territory")) %>% 
  filter(Item == "Opening stock") 

### B.Apply flag for 90th and 99th percentile
#### Define function filter_and_quantile
# filters by state name and returns nth percentile of fwi for that state
filter_and_quantile <- function(state_name, percentile=0.99, data=FWI){
  data_state <- data %>% 
    filter(state == state_name)
  percentile_value <- as.numeric(quantile(data_state$fwi, percentile))
  return(percentile_value)
}

#### Find 90th and 99th percentile of fwi for each state
states <- unique(FWI$state)

FWI_90th <- as.numeric(lapply(states,filter_and_quantile, percentile = 0.9))
FWI_states_90 <- data.frame(State = states, FWI_90th = FWI_90th)

FWI_99th <- as.numeric(sapply(states,filter_and_quantile))
FWI_states_99 <- data.frame(State = states, FWI_99th = FWI_99th)

#### Add flag if max fwi for state exceeds nth percentile
FWI_state <- FWI %>% 
  group_by(date, state) %>% 
  summarise(FWI_max = max(fwi)) %>% 
  left_join(FWI_states_90, by=join_by("state"=="State")) %>% 
  left_join(FWI_states_99, by=join_by("state"=="State")) %>% 
  mutate(FWI_90th_flag = FWI_max >= FWI_90th) %>%
  mutate(FWI_99th_flag = FWI_max >= FWI_99th)

### C.Interpolate daily SOI and IOD
# assume that the iod and soi indicies given are at the start of the year-month and linearly interpolates the iod and soi indicies for the rest of the dates
IOD_2 <- data.frame(Date = seq(min(IOD$Date),max(IOD$Date), by="days"))
IOD_2$YearMonthStartDate <- ymd(year(IOD_2$Date)*10000+month(IOD_2$Date)*100+1)
IOD_2$YearMonthEndDate <- add_with_rollback(IOD_2$YearMonthStartDate, months(1), roll_to_first=TRUE)
IOD_2 <- IOD_2 %>% 
  left_join(IOD, by=join_by(YearMonthStartDate==Date)) %>% 
  rename("IOD_Start"="IOD") %>%
  left_join(IOD, by=join_by(YearMonthEndDate==Date)) %>% 
  rename("IOD_End"="IOD") %>%
  mutate(DaysFromStart = as.numeric(Date-YearMonthStartDate)) %>% 
  mutate(DaysUntilEnd = as.numeric(YearMonthEndDate- Date)) %>%
  mutate(IOD = (IOD_Start*DaysUntilEnd+IOD_End*DaysFromStart)/(DaysUntilEnd+DaysFromStart)) %>%
  select(Date,IOD)

SOI_2 <- data.frame(Date = seq(min(SOI$Date),max(SOI$Date), by="days"))
SOI_2$YearMonthStartDate <- ymd(year(SOI_2$Date)*10000+month(SOI_2$Date)*100+1)
SOI_2$YearMonthEndDate <- add_with_rollback(SOI_2$YearMonthStartDate, months(1), roll_to_first=TRUE)
SOI_2 <- SOI_2 %>% 
  left_join(SOI, by=join_by(YearMonthStartDate==Date)) %>% 
  rename("SOI_Start"="SOI") %>%
  left_join(SOI, by=join_by(YearMonthEndDate==Date)) %>% 
  rename("SOI_End"="SOI") %>%
  mutate(DaysFromStart = as.numeric(Date-YearMonthStartDate)) %>% 
  mutate(DaysUntilEnd = as.numeric(YearMonthEndDate- Date)) %>%
  mutate(SOI = (SOI_Start*DaysUntilEnd+SOI_End*DaysFromStart)/(DaysUntilEnd+DaysFromStart)) %>%
  select(Date,SOI)

### D.Add flag for days with bushfire events in each state
#### Filter bushfire data
ICA_Bushfire <- ICA_CAT_Hist %>%
  filter(Type == "Bushfire") %>%
  select(c("CAT_Name","CAT_Event_Start","CAT_Event_Finish","State")) %>% 
  separate_rows(State, sep=",") %>%
  separate_rows(State, sep=" ") %>%
  filter(State != "") %>%
  mutate(Bushfire_Flag = T)

#### Update state value to be compatible with FWI data - will need this to merge datasets
ICA_Bushfire <- ICA_Bushfire %>%
  mutate(State = case_when(
    State == "NSW" ~ "New South Wales",
    State == "NT" ~ "Northern Territory",
    State == "QLD" ~ "Queensland",
    State == "SA" ~ "South Australia",
    State == "TAS" ~ "Tasmania",
    State == "VIC" ~ "Victoria",
    State == "WA" ~ "Western Australia",
    TRUE ~ "Others"
  ))

#### Add flag for days with bushfires in each state
# Create a base table for every combination of date + state
Date <- seq(ymd("1980-04-01"), ymd("2023-06-30"), "days")
State <- c("New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")
date_state <- crossing(Date, State)

# Add flag
ICA_Bushfire <- date_state %>% 
  left_join(ICA_Bushfire, by=join_by("Date">="CAT_Event_Start", "Date"<="CAT_Event_Finish", "State" == "State")) %>% 
  select(-c("CAT_Name", "CAT_Event_Start", "CAT_Event_Finish")) %>%
  mutate(Bushfire_Flag = case_when(is.na(Bushfire_Flag)~ FALSE, TRUE ~ TRUE))

### E.Convert land cover into % of land area covered by a. Artificial_surfaces, b. Cultivated_terrestrial_vegetated, c. Natural_terrestrial_vegetated and d. Water.
Land_cover_state <- Land_cover %>% 
  select(c("State.Code"
           ,"State.Name"
           ,"Open.Year"
           ,"Artificial.surfaces"
           ,"Cultivated.terrestrial.vegetated..herbaceous"
           ,"Natural.terrestrial.vegetated..herbaceous"
           ,"Natural.terrestrial.vegetated..woody"
           ,"Natural.surfaces"
           ,"Natural.aquatic.vegetated..herbaceous"
           ,"Natural.aquatic.vegetated..woody"
           ,"Water..perennial"
           ,"Water..non.perennial"
           ,"Tidal.area"
           ,"Total")) %>% 
  mutate_at(c("Artificial.surfaces"
              ,"Cultivated.terrestrial.vegetated..herbaceous"
              ,"Natural.terrestrial.vegetated..herbaceous"
              ,"Natural.terrestrial.vegetated..woody"
              ,"Natural.surfaces"
              ,"Natural.aquatic.vegetated..herbaceous"
              ,"Natural.aquatic.vegetated..woody"
              ,"Water..perennial"
              ,"Water..non.perennial"
              ,"Tidal.area"
              ,"Total"), ~replace_na(as.numeric(.),0)) %>% 
  group_by(State.Code, State.Name) %>% 
  summarise(across(c("Artificial.surfaces"
                     ,"Cultivated.terrestrial.vegetated..herbaceous"
                     ,"Natural.terrestrial.vegetated..herbaceous"
                     ,"Natural.terrestrial.vegetated..woody"
                     ,"Natural.surfaces"
                     ,"Natural.aquatic.vegetated..herbaceous"
                     ,"Natural.aquatic.vegetated..woody"
                     ,"Water..perennial"
                     ,"Water..non.perennial"
                     ,"Tidal.area"
                     ,"Total"), sum)) %>% 
  mutate(across(c("Artificial.surfaces"
                  ,"Cultivated.terrestrial.vegetated..herbaceous"
                  ,"Natural.terrestrial.vegetated..herbaceous"
                  ,"Natural.terrestrial.vegetated..woody"
                  ,"Natural.surfaces"
                  ,"Natural.aquatic.vegetated..herbaceous"
                  ,"Natural.aquatic.vegetated..woody"
                  ,"Water..perennial"
                  ,"Water..non.perennial"
                  ,"Tidal.area"), ~./Total)) %>% 
  select(-Total) %>% 
  mutate(Artificial_surfaces = Artificial.surfaces) %>% 
  mutate(Cultivated_terrestrial_vegetated = Cultivated.terrestrial.vegetated..herbaceous) %>% 
  mutate(Natural_terrestrial_vegetated = Natural.terrestrial.vegetated..herbaceous + Natural.terrestrial.vegetated..woody + Natural.surfaces) %>% 
  mutate(Water = Natural.aquatic.vegetated..herbaceous + Natural.aquatic.vegetated..woody + Water..perennial + Water..non.perennial + Tidal.area) %>% 
  select(-c("State.Code",
            "Artificial.surfaces", 
            "Cultivated.terrestrial.vegetated..herbaceous", 
            "Natural.terrestrial.vegetated..herbaceous",
            "Natural.terrestrial.vegetated..woody",
            "Natural.surfaces",
            "Natural.aquatic.vegetated..herbaceous",
            "Natural.aquatic.vegetated..woody",
            "Water..perennial",
            "Water..non.perennial",
            "Tidal.area"))

## Merge datasets
df <- FWI_state %>% 
  left_join(IOD_2, join_by(date==Date)) %>% 
  left_join(SOI_2, join_by(date==Date)) %>% 
  left_join(Land_cover_state, join_by(state==State.Name)) %>%
  left_join(ICA_Bushfire, join_by(state==State,date==Date)) %>%
  select(-c("State.Code")) %>%
  rename("State" = "state",
         "Date" = "date")

## Export dataset
write.csv(df,file="C:/Users/Callista Surjadi/Documents/Academics/4305 Sandbox/df.csv",row.names = F)
