## Set working directory
# setwd()

## Initialise packages
# install.packages("pacman")
pacman::p_load("tidyverse")

## Import dataset
data <- read.csv("df.csv")

## Fix variable formats
data$Date <- ymd(data$Date)
data$State <- as.factor(data$State)
data$FWI_90th_flag <- as.factor(data$FWI_90th_flag)
data$FWI_95th_flag <- as.factor(data$FWI_95th_flag)
data$FWI_99th_flag <- as.factor(data$FWI_99th_flag)
data$Bushfire_Flag <- as.factor(data$Bushfire_Flag)

## Compare different thresholds for FWI flag against Bushfire flag
### A.90th percentile
ggplot(data=data)+
  geom_bar(mapping=aes(x=Bushfire_Flag,fill=FWI_90th_flag),
           position="fill")+
  facet_wrap(~State,nrow=3)+
  scale_fill_manual(values=c("#008B92","#FFE781"))+
  theme(panel.background = element_rect(fill="white"))+
  ggtitle("FWI 90th Percentile Threshold", "Redundant flag")

### B.95th percentile
ggplot(data=data)+
  geom_bar(mapping=aes(x=Bushfire_Flag,fill=FWI_95th_flag),
           position="fill")+
  facet_wrap(~State,nrow=3)+
  scale_fill_manual(values=c("#008B92","#FFE781"))+
  theme(panel.background = element_rect(fill="white"))+
  ggtitle("FWI 95th Percentile Threshold", "Contradictory effect on WA")

### C.99th percentile
ggplot(data=data)+
  geom_bar(mapping=aes(x=Bushfire_Flag,fill=FWI_99th_flag),
           position="fill")+
  facet_wrap(~State,nrow=3)+
  scale_fill_manual(values=c("#008B92","#FFE781"))+
  theme(panel.background = element_rect(fill="white"))+
  ggtitle("FWI 99th Percentile Threshold", "Captures some positive correlation")

# use 99th percentile as threshold since it best captures relation between flag and bushfire event
data <- data %>% 
  select(-c("FWI_90th","FWI_95th","FWI_99th","FWI_max","FWI_90th_flag","FWI_95th_flag"))

## Add flags for SOI and IOD
data <- data %>% 
  mutate(SOI_Condition = case_when(SOI <= 7 & SOI >= -7 ~ "Normal SOI",
                                   SOI > 7 ~ "El Nino",
                                   SOI < -7 ~ "La Nina")) %>%
  mutate(IOD_Phase = case_when(IOD <= 0.4 & IOD >= -0.4 ~ "Normal IOD",
                               IOD > 0.4 ~ "Positive",
                               IOD < -0.4 ~ "Negative")) %>% 
  select(-c("IOD", "SOI"))

# Format as factor
data$SOI_Condition <- as.factor(data$SOI_Condition)
data$IOD_Phase <- as.factor(data$IOD_Phase)

# Set baseline
data <- within(data, SOI_Condition <- relevel(SOI_Condition, ref = "Normal SOI"))
data <- within(data, IOD_Phase <- relevel(IOD_Phase, ref = "Normal IOD"))

## Compare bushfire to different SOI conditions and IOD phases
### A.SOI condition
ggplot(data=data)+
  geom_bar(mapping=aes(x=Bushfire_Flag,fill=SOI_Condition),
           position="fill")+
  facet_wrap(~State,nrow=3)+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(values=c("#F9F9F9","#008B92","#FFE781"))+
  ggtitle("SOI Condition")

### B.IOD phase
ggplot(data=data)+
  geom_bar(mapping=aes(x=Bushfire_Flag,fill=IOD_Phase),
           position="fill")+
  facet_wrap(~State,nrow=3)+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(values=c("#F9F9F9","#008B92","#FFE781"))+
  ggtitle("IOD Phase")

## Export updated dataset
write.csv(data,file="df_updated.csv",row.names = F)
