
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
pacman::p_load("dplyr", "rgdal", "ggplot2", "tmap", "ggmap", "sf", "ggspatial", "rlang", "broom", "tidyverse", "raustats", "purr", "readxl", "wesanderson", "lubridate")

# Load data
# Source: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
ica_cat_event_history <- read_excel("./Data/Climate Data/ICA-Historical-Normalised-Catastrophe-August-2023.xlsx", range="ICA_CAT_Historical!A10:X742")

aus_sa1_shp <- read_sf("./Data/Geospatial Data/SA1_2021_AUST_SHP_GDA2020/SA1_2021_AUST_GDA2020.shp")
aus_sa2_shp <- read_sf("./Data/Geospatial Data/SA2_2021_AUST_SHP_GDA2020/SA2_2021_AUST_GDA2020.shp")
aus_sa3_shp <- read_sf("./Data/Geospatial Data/SA3_2021_AUST_SHP_GDA2020/SA3_2021_AUST_GDA2020.shp")
aus_sa4_shp <- read_sf("./Data/Geospatial Data/SA4_2021_AUST_SHP_GDA2020/SA4_2021_AUST_GDA2020.shp")
aus_poa_shp <- read_sf("./Data/Geospatial Data/POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp")

aus_postcodes <- read_csv("./Data/Geospatial Data/australian_postcodes.csv")

# Clean data

## Format postcode columns
aus_poa_shp$POA_CODE21 <- as.character(as.numeric(aus_poa_shp$POA_CODE21))
aus_postcodes$postcode <- as.character(as.numeric(aus_postcodes$postcode))


## Format date columns
ica_cat_event_history$CAT_Event_Start <- ymd(ica_cat_event_history$CAT_Event_Start)
ica_cat_event_history$CAT_Event_Finish <- ymd(ica_cat_event_history$CAT_Event_Finish)

## A. Filter for bushfire event entries with postcodes
ica_bushfire_event_history <- ica_cat_event_history %>% 
  filter(Type == "Bushfire") 

## A1. Transform table as desired 
# ica_bushfire_event_postcode <- ica_bushfire_event_history %>%
#   filter(!is.na(Postcode)) %>%
#   select(c("CAT_Name","CAT_Event_Start","CAT_Event_Finish","Postcode")) %>%
#   separate_rows(Postcode, sep=",") %>%
#   mutate(Postcode = as.numeric(Postcode)) %>%
#   filter(!is.na(Postcode)) %>%
#   mutate(Postcode = as.character(Postcode)) %>%
#   mutate(Bushfire_Days = CAT_Event_Finish-CAT_Event_Start)

ica_bushfire_event_state <- ica_bushfire_event_history %>%
  # filter(!is.na(Postcode)) %>%
  select(c("CAT_Name","CAT_Event_Start","CAT_Event_Finish","State")) %>%
  separate_rows(State, sep=",") %>%
  mutate(State = str_trim(State)) %>% 
  mutate(State = case_when(
    State == "NSW" ~ "New South Wales",
    State == "NT" ~ "Northern Territory",
    State == "QLD" ~ "Queensland",
    State == "SA" ~ "South Australia",
    State == "TAS" ~ "Tasmania",
    State == "VIC" ~ "Victoria",
    State == "WA" ~ "Western Australia",
    TRUE ~ "Others"
  )) %>%
  mutate(Bushfire_Flag = TRUE) %>% 
  select(-"CAT_Name") %>% 
  distinct()


#%>% 
  # mutate(Bushfire_Days = CAT_Event_Finish-CAT_Event_Start)
# 
# ## B. Filter for bushfire event entries without postcodes
# ica_bushfire_event_no_postcode <- ica_bushfire_event_history %>% 
#   filter(is.na(Postcode) | Postcode == "TBA") 
# 
# ### B1. Define a function to search for locality in any given string
# search_for_locality <- function(string, list_locality = unique(aus_postcodes$locality)){
#   search_result <- sapply(list_locality, grepl, toupper(string))
#   locality_found <- attributes(search_result)$names[as.vector(search_result)]
#   locality_found_concat <- paste(locality_found, collapse = ", ")
#   return(locality_found_concat)
# }
# 
# ### B2. Apply the function to search across CAT_Description
# locality_found <- lapply(ica_bushfire_event_no_postcode$CAT_Description, search_for_locality)
# 
# ### B3. Store the result in column Locality_Found
# ica_bushfire_event_no_postcode$Locality_Found <- locality_found
# 
# 
# ### B4. Create a mapping table between Postcodes & Locality
# aus_postcodes_mapping <- aus_postcodes %>% 
#   group_by(locality) %>% 
#   summarise(postcode = paste(postcode, collapse = ", "))
# 
# aus_locality_mapping <- aus_postcodes %>% 
#   group_by(postcode) %>% 
#   summarise(locality = paste(locality, collapse = ", "))
# 
# 
# 
# ### B5. Trasnform table as desired
# ica_bushfire_event_no_postcode <- ica_bushfire_event_no_postcode %>% 
#   select(c("CAT_Name","CAT_Event_Start","CAT_Event_Finish","Locality_Found")) %>% 
#   separate_rows(Locality_Found, sep=", ") %>%
#   left_join(aus_postcodes_mapping, by=c("Locality_Found" = "locality")) %>% 
#   rename(Postcode = postcode) %>% 
#   select(-"Locality_Found") %>% 
#   separate_rows(Postcode, sep=", ") %>% 
#   mutate(Postcode = as.character(as.numeric(Postcode))) %>% 
#   mutate(Bushfire_Days = CAT_Event_Finish-CAT_Event_Start) # calculate the number of 
# 
# 
# 
# 
# 
# ## Create summary 
# # detach(package:plyr) # Make sure to detatch plyr package before running group_by + summarise!
# 
# ica_bushfire_event_summary <- rbind(ica_bushfire_event_postcode,ica_bushfire_event_no_postcode) %>% 
#   group_by(Postcode) %>% 
#   summarise(Total_Bushfire_Days = sum(Bushfire_Days), Total_Bushfire_Count=n()) %>% 
#   mutate(Bushfire_Frequency = as.numeric(Total_Bushfire_Days)/as.numeric((as.Date("2023-06-30")-as.Date("1980-04-01")))) %>% 
#   arrange(desc(Bushfire_Frequency)) %>% 
#   left_join(aus_locality_mapping, by=c("Postcode" = "postcode"))
# 
# # Examine & compare the list of postcodes in aus_poa_shp (shape file) & aus_postcodes (mapping file)
# postcode_list_1 <- unique(aus_postcodes$postcode) # aus_poa_shp (shape file)
# postcode_list_2 <- unique(aus_poa_shp$POA_CODE21) # aus_postcodes (mapping file)
# 
# postcode_list_2[!(postcode_list_2 %in% postcode_list_1)] # List of postcodes that exists in list 2 but not in list 1
# postcode_list_1[!(postcode_list_1 %in% postcode_list_2)] # List of postcodes that exists in list 1 but not in list 2
# 
# # List of postcodes that exists in list 2 but not in list 1 are: 9494, 9797, ZZZZ
# # According to ABS (https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/non-abs-structures/postal-areas):
# # - 9494 is reserved for cases where people are coded to No usual address Mesh Blocks
# # - 9797 is reserved for cases where people are coded to Migratory, Offshore and Shipping Mesh Blocks.
# # - ZZZZ is reserved for cases where people are coded to Outside Australia.
# 
# postcode_list_from_ica_bushfire_event_postcode <- unique(ica_bushfire_event_postcode$Postcode)
# postcode_list_from_ica_bushfire_event_no_postcode <- unique(ica_bushfire_event_no_postcode$Postcode)
# 
# postcode_list_from_ica_bushfire_event_postcode[!(postcode_list_from_ica_bushfire_event_postcode %in% postcode_list_2)] # List of postcodes provided by ICA but not in list 2 (7189)
# postcode_list_from_ica_bushfire_event_no_postcode[!(postcode_list_from_ica_bushfire_event_no_postcode %in% postcode_list_2)] # List of postcodes mapped via aus_postcodes (mapping file) but not in list 2
# 
# aus_postcodes_longlat_full <- aus_postcodes %>% 
#   select(c("postcode", "long", "lat")) %>%
#   distinct() 
# 
# aus_postcodes_longlat_filtered <- aus_postcodes_longlat_full %>% 
#   filter(postcode %in% postcode_list_2) #%>% 
# # group_by(postcode) %>% 
# # summarise(long = mean(long), lat = mean(lat))
# 
# # write.csv(aus_postcodes_longlat_filtered, "./Data/Cleaned Data/aus_postcodes_longlat_mapping.csv", row.names=FALSE)
# 
# # Define a function that searches for 
# search_for_replacement_postcode <- function(postcode_0, full_list=aus_postcodes_longlat_full, replacement_list=aus_postcodes_longlat_filtered){
#   
#   long_0 <- full_list$long[match(postcode_0,full_list$postcode)]
#   lat_0 <- full_list$lat[match(postcode_0,full_list$postcode)]
#   
#   aus_postcodes_dist <- replacement_list %>% 
#     mutate(dist = (long-long_0)^2+ (lat-lat_0)^2) %>% 
#     arrange(dist)
#   
#   postcode_1 <- aus_postcodes_dist$postcode[1]
#   
#   return(postcode_1)
# }
# 
# # Apply the function to find replacement postcodes
# postcode_list_to_replace <- postcode_list_from_ica_bushfire_event_no_postcode[!(postcode_list_from_ica_bushfire_event_no_postcode %in% postcode_list_2)]
# replacement_postcodes_found <- as.character(lapply(postcode_list_to_replace, search_for_replacement_postcode))
# replacement_postcodes_mapping <- as.data.frame(cbind(postcode_list_to_replace,replacement_postcodes_found))
# 
# 
# # Create output table
# ## Create a ica_bushfire_event table that documents every bushfire event
# ica_bushfire_event <- rbind(ica_bushfire_event_postcode,ica_bushfire_event_no_postcode) %>% 
#   left_join(replacement_postcodes_mapping, by=c("Postcode" = "postcode_list_to_replace")) %>% 
#   mutate(Postcode = case_when(is.na(replacement_postcodes_found) ~ Postcode, TRUE ~ replacement_postcodes_found)) %>% 
#   filter(Postcode != "7189") %>% # filter out "7189" as it does not exist
#   select(c("CAT_Event_Start","CAT_Event_Finish","Postcode")) %>% 
#   mutate(Bushfire_Flag = TRUE)
# 
# ## Confirm that all postcodes listed in ica_bushfire_event is listed in postcode_list_2
# ica_bushfire_event$Postcode[!(ica_bushfire_event$Postcode %in% postcode_list_2)]
# 
# 
# aus_postcodes_state_mapping <- aus_postcodes %>% 
#   select(c("postcode","state")) %>% 
  # mutate(state = case_when(
  #   state == "NSW" ~ "New South Wales",
  #   state == "NT" ~ "Northern Territory",
  #   state == "QLD" ~ "Queensland",
  #   state == "SA" ~ "South Australia",
  #   state == "TAS" ~ "Tasmania",
  #   state == "VIC" ~ "Victoria",
  #   state == "WA" ~ "Western Australia",
  #   TRUE ~ "Others"
  # )) %>%
#   mutate(state = case_when(
#     postcode == 872 ~ "Northern Territory",
#     postcode == 4825 ~ "Queensland",
#     postcode == 4385 ~ "Queensland",
#     postcode == 4383 ~ "Queensland",
#     postcode == 4380 ~ "Queensland",
#     postcode == 4377 ~ "Queensland",
#     postcode == 4375 ~ "Queensland",
#     postcode == 3707 ~ "Victoria",
#     postcode == 3691 ~ "Victoria",
#     postcode == 3644 ~ "Victoria",
#     postcode == 3586 ~ "Victoria",
#     postcode == 3500 ~ "Victoria",
#     postcode == 2620 ~ "New South Wales",
#     postcode == 2618 ~ "New South Wales",
#     postcode == 2611 ~ "New South Wales",
#     postcode == 2540 ~ "New South Wales",
#     postcode == 2406 ~ "New South Wales",
#     TRUE ~ state
#   )) %>% 
#   distinct()
# 
# ica_bushfire_event_state <- ica_bushfire_event %>% 
#   left_join(aus_postcodes_state_mapping, join_by(Postcode==postcode)) %>% 
#   filter(state != "Others") %>% 
#   select(-"Postcode") %>% 
#   distinct()
# 
state_list <- c("New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")

# Create a base table for every combination of date + state
Date <- seq(ymd("1980-04-01"), ymd("2023-06-30"), "days")
State <- state_list
date_state <- crossing(Date, State)

# Join date_postcode base table with ica_bushfire_event table and create the output table
ica_bushfire_event_output <- date_state %>% 
  left_join(ica_bushfire_event_state, by=join_by("Date">="CAT_Event_Start", "Date"<="CAT_Event_Finish", "State" == "State")) %>% 
  select(-c("CAT_Event_Start", "CAT_Event_Finish")) %>%
  mutate(Bushfire_Flag = case_when(is.na(Bushfire_Flag)~ FALSE, TRUE ~ TRUE))

write.csv(ica_bushfire_event_output, "./Data/Cleaned Data/y.csv", row.names=FALSE)

