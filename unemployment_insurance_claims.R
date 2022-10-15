library(fredr)
library(tidyverse)
library(ggplot2)
library(zoo)

rm(list = ls())

theme_set(theme_bw())

fredr_set_key("2348b36f80a0d112550c91699d8a309f")

#1. Get Data from FRED----

continued.claims.US = fredr(series_id ="CCSA",
                          observation_start = as.Date("2001-02-01"),
                          observation_end = as.Date("2021-12-01"),
                          frequency = "w")

continued.claims.PA = fredr(series_id = "PACCLAIMS",
                          observation_start = as.Date("2001-02-01"),
                          observation_end = as.Date("2021-12-01"),
                          frequency = "w")

#Clean/Transform Data----
#Aggregate ----

#PA
continued.claims.PA$month = format(continued.claims.PA$date, "%b")
continued.claims.PA$year = format(continued.claims.PA$date, "%Y")

continued.claims.PA.m <- continued.claims.PA  %>%
  group_by(year,month) %>%
  summarize(MValue = value[length(value)])

#US
continued.claims.US$month = format(continued.claims.US$date, "%b")
continued.claims.US$year = format(continued.claims.US$date, "%Y")

continued.claims.US.m <- continued.claims.US  %>%
  group_by(year,month) %>%
  summarize(MValue = value[length(value)])   


#creating a date variable from Month and Year
continued.claims.PA.m$date <-  as.Date(as.yearmon(paste(continued.claims.PA.m$year, continued.claims.PA.m$month, sep="-"), "%Y-%b")) 
continued.claims.US.m$date <-  as.Date(as.yearmon(paste(continued.claims.US.m$year, continued.claims.US.m$month, sep="-"), "%Y-%b")) 


#seasonal adjustment of PA data----

#Need to find monthly and annual averages before 2020

MonMean <- continued.claims.PA.m %>%
  filter(year<2020) %>%
  group_by(month) %>% 
  summarize(MonMean = mean(MValue))
MonMean

continued.claims.PA.mAdj <- merge(continued.claims.PA.m,MonMean)

YearMean <- continued.claims.PA.m %>%
  filter(year<2020) %>%
  group_by() %>% 
  summarize(YearMean = mean(MValue))
YearMean

continued.claims.PA.mAdj <- merge(continued.claims.PA.mAdj,YearMean)

continued.claims.PA.mAdj$adjMValue <-  continued.claims.PA.mAdj$MValue - (continued.claims.PA.mAdj$MonMean -continued.claims.PA.mAdj$YearMean)


# Create 2019 averages for scaling----

US2019 <- continued.claims.US.m %>%
  filter(year==2019)
mean_US2019 <- mean(US2019$MValue)


PA2019 <- continued.claims.PA.mAdj %>%
  filter(year==2019)
mean_PA2019 <-mean(PA2019$adjMValue)


continued.claims.PA.mAdj$scValue <- continued.claims.PA.mAdj$adjMValue/mean_PA2019
continued.claims.US.m$scValue <- continued.claims.US.m$MValue/mean_US2019


#Plot ----

ggplot() +
  geom_bar(data = continued.claims.US.m, aes(y = scValue, x = date, fill = "US"), stat="identity") +
  geom_bar(data = continued.claims.PA.mAdj, aes(y = scValue, x = date, fill = "PA"), stat="identity", alpha = 0.5) +
  scale_fill_manual(name = "",
                    values = c("US" = "gray60", "PA" = "darkred")) +
  scale_x_date(date_breaks = "3 month",
               date_labels = "%b-%y",
               limits = as.Date(c("2019-11-01", "2021-05-01"))) +
  labs(title = "Continued Unemployment Insurance Claims",
       y = "Indexed Value") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = c(.9, .8),
    legend.background = element_rect(fill = NA, color = NA)
  )+
  guides(fill = guide_legend(override.aes = list(alpha = 0.5))) 

ggsave("ContinuedClaims.pdf", height = 7.26,  width = 11.35, units = "cm")
