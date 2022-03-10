#---- Load libraries ----

#install.packages(c("maps", "mapdata", "mapproj", "tidycensus","viridis" , "sf", "tigris"))


library(tidyverse)
library(ggplot2)
library(readxl)

#The maps package contains a lot of outlines of continents, countries, states,
#and counties that have been with R for a long time.
#The mapdata package contains a few more, higher-resolution outlines.
#The mapproj package allows you to change projection
library(mapproj)
library(maps)
library(mapdata)


#Packages to plot census data
library(tidycensus)
library(tigris)
library(sf)

## another R color scheme
library(viridis) 
census_api_key("f3597a399ec74504184e101c1963dae871717157")
options(tigris_use_cache = TRUE)


data <- read_xlsx("PA_Net_Migrations_Calcs.xlsx")

names(data)[names(data) == "...9"] <- "estimate9"
names(data)[names(data) == "Table with column headers in rows 2 through 3."] <- "code"
names(data)[names(data) == "...3"] <- "MSA"

data <-na.omit(data)

net <- data %>%
  group_by(MSA) %>%
  summarize(net = sum(as.numeric(estimate9)))


census_api_key("f3597a399ec74504184e101c1963dae871717157")
options(tigris_use_cache = TRUE)


msa_population <-  get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                   variables = "B01003_001",
                   year = 2018,
                   survey = "acs5",  #ACS 5-year survey
                   geometry = TRUE)

pa_population <- get_acs(geography = "state", 
                 state = "PA",
                 variables = "B01003_001",
                 year = 2018,
                 survey = "acs5", 
                 geometry = TRUE)


migration <- read_csv("PA_net_migrations.csv")

code <- msa_population %>% 
  filter(GEOID == 14100|GEOID == 16540|GEOID ==20700|GEOID == 21500|GEOID == 23900
         |GEOID == 25420|GEOID == 27780|GEOID == 29540|GEOID == 30140|GEOID == 49620
         |GEOID == 48700|GEOID == 44300|GEOID == 42540|GEOID == 37980|GEOID == 38300
         |GEOID == 39740|GEOID == 10900|GEOID == 11020)  

migration$GEOID <- migration$MSA_Code
data_final <- merge(code,migration,by = "GEOID")



PIT <- data.frame(
  long = c(-79.995888),
  lat = c(40.440624),
  names = c("Pittsburgh, PA")
) 
PHIL <- data.frame(
  long = c(-75.1652),
  lat = c(39.9526),
  names = c("Philadelphia, PA")
)


plot1<-  ggplot() +
  geom_sf(data = pa_population, fill = "grey60") +
  geom_sf(data = data_final, 
          aes(fill = `Net Migration`), color = "black") +
  geom_sf_text(data = data_final, aes(label = `Net Migration`), size = 4) + 
  scale_fill_gradient2(na.value = "black", low = "steelblue", mid="white",high="dark red")+
  labs(title = "Net Migration for PA MSAs 2014-2018 (number of people)",
       subtitle = element_blank(),
       x = "Longitude",
       y = "Latitude") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  geom_point(data = PIT, aes(x = long, y = lat-0.1), shape=8, color = "black", size = 1)+
  geom_text(data = PIT,aes(x=long+0.2, y=lat-0.2, label = names, fontface=2)) +
  geom_point(data = PHIL, aes(x = long, y = lat-0.1), shape=8, color = "black", size = 1)+
  geom_text(data = PHIL,aes(x=long+0.1, y=lat-0.2, label = names, fontface=2))

plot1

ggsave("net_migration_PA.pdf", height = 7.26*2,  width = 11.35*2, units = "cm")
