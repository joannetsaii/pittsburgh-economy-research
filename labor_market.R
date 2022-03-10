library(fredr)      
library(tidyverse) 
library(ggplot2)    
library(ggthemes)  
library(knitr)      
library(zoo)        
library(readxl)      
library(ggpubr)

fredr_set_key("2348b36f80a0d112550c91699d8a309f")
theme_set(theme_bw())

# Importing data about US Job Openings from FRED
US.job.openings <-  fredr(series_id="JTSJOL", observation_start=as.Date("2001-02-01"),
                          observation_end=as.Date("2020-12-01"),
                          frequency="m")

#Importing data about NE Job Openings from FRED
ne.job.openings <-  fredr(series_id="JTS00NEJOL", observation_start=as.Date("2001-02-01"),
                          observation_end=as.Date("2020-12-01"),
                          frequency="m")

# Importing data about PA Job Openings from excel sheet
PA.job.openings <- read_xlsx("stateJobData2020.xlsx")
# Cleaning and processing data from excel in the required format
colnames(PA.job.openings)[c(1,2,4)] = c("date", "state", "value")
PA.job.openings = PA.job.openings %>%
  filter(state == "Pennsylvania") %>%
  select(date, value)
PA.job.openings$date <- paste(substr(PA.job.openings$date, 1, 4),
                              substr(PA.job.openings$date, 5, 6),
                              "01",
                              sep = "-")
PA.job.openings$date <- as.Date(PA.job.openings$date)
PA.job.openings$value <- as.numeric(PA.job.openings$value)
PA.job.openings <- PA.job.openings[order(PA.job.openings$date),]

# Importing data about US unemployed person from FRED
us.unemploy.person <-  fredr(series_id="UNEMPLOY", observation_start=as.Date("2001-02-01"),
                          observation_end=as.Date("2020-12-01"),
                          frequency="m")


# data about PA unemployed person from FRED
pa.unemploy.person <-  fredr(series_id="LASST420000000000004", observation_start=as.Date("2001-02-01"),
                             observation_end=as.Date("2020-12-01"),
                             frequency="m")

#data about NE unemployed person from FRED
ne.unemploy.person <-  fredr(series_id="LASRD830000000000004", observation_start=as.Date("2001-02-01"),
                             observation_end=as.Date("2020-12-01"),
                             frequency="m")

us.job.capita <- tibble(date = us.unemploy.person$date,
                        value = ((US.job.openings$value/us.unemploy.person$value)*100))

pa.job.capita <- tibble(date = pa.unemploy.person$date,
                        value = ((PA.job.openings$value/((pa.unemploy.person$value)/1000)*100)))

ne.job.capita <- tibble(date = ne.unemploy.person$date,
                        value = ((ne.job.openings$value/((ne.unemploy.person$value)/1000)*100)))


us.job.capita <- subset(us.job.capita, us.job.capita$date >= as.Date("2019-03-01"))
pa.job.capita <- subset(pa.job.capita, pa.job.capita$date >= as.Date("2019-03-01"))
ne.job.capita <- subset(ne.job.capita, ne.job.capita$date >= as.Date("2019-03-01"))


graph1 <- ggplot() + 
  geom_vline(xintercept = as.Date("2020-03-01"), color="black", size = 0.5, linetype = "dashed")+
  geom_line(data = us.job.capita, aes(x=date, y=value, color="US")) +  #moving averages
  geom_line(data = pa.job.capita, aes(x=date, y=value, color="PA")) +
  geom_line(data = ne.job.capita, aes(x = date, y = value, color = "North-East")) +
  scale_color_manual(name = "", 
                     values = c("US" = "blue", "PA" = "red", "North-East" = "black")) +
  xlim(as.Date("2019-03-01"), as.Date("2020-12-01")) + 
  labs(x="",
       y="Job openings over unemployed persons (%)",
       title = "Labor Market Tightness: Job Openings/Number of Unemployed Persons") +  #labeling in the plot
  theme_bw()+ #change the graph theme
  theme(legend.position = "bottom", legend.text = element_text(size=12),
        plot.margin = margin(1, 1, 0.5, 0.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        axis.text.y = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        plot.title = element_text(color = "black", size = 14, hjust = 0.5)) #title color

graph1

ggsave("laborMarketTightness.png",  width = 22.70, height = 14.52, units = "cm", scale=0.8)


pa.ur <-  fredr(series_id="PAUR", observation_start=as.Date("2000-02-01"),
                             observation_end=as.Date("2020-12-01"),
                             frequency="m")

pa.employment <- fredr(series_id="PANA", observation_start=as.Date("2000-02-01"),
                       observation_end=as.Date("2020-12-01"),
                       frequency="m")

pa.vacancy<-tibble(date = pa.ur$date,
                  unemploy = pa.ur$value,
                  value = ((PA.job.openings$value) / (PA.job.openings$value+pa.employment$value))*100)


pa.vacancy.2007 <- subset(pa.vacancy, pa.vacancy$date >= as.Date("2000-02-01") & pa.vacancy$date <= as.Date("2007-12-01"))
pa.vacancy.2010 <- subset(pa.vacancy, pa.vacancy$date >= as.Date("2008-01-01") & pa.vacancy$date <= as.Date("2010-12-01"))
pa.vacancy.2019 <- subset(pa.vacancy, pa.vacancy$date >= as.Date("2011-01-01") & pa.vacancy$date <= as.Date("2019-12-01"))
pa.vacancy.2021 <- subset(pa.vacancy, pa.vacancy$date >= as.Date("2020-01-01") & pa.vacancy$date <= as.Date("2021-12-01"))

pa.vacancy.date <- slice(pa.vacancy.2021, c(8,5,3,12,4))

graph2 <-ggplot()+
  geom_point(data = pa.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_line(data = pa.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_point(data = pa.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_line(data = pa.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_point(data = pa.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_line(data = pa.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_point(data = pa.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_line(data = pa.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_text(data = pa.vacancy.date,aes(x=unemploy-0.25, y=value - 0.15, label = date))+
  scale_color_manual(name = "", 
                     values = c("2000-2007" = "black", "2008-2010" = "#82da8e", "2011-2019" = "blue", "2020-2021" = "red"))+
  labs(x="Unemployment rate (%)",
       y="Vacancy rate (%)",
       title = "Beveridge Curve - Pennsylvania",
       caption = "") +
  theme_bw()+ #change the graph theme
  theme(legend.position = "bottom", legend.text = element_text(size=12),
        plot.margin = margin(1, 0.5, 0, 0.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        axis.text.y = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5))

graph2

ggsave("beveridgePA.png", width = 22.70, height = 14.5, units = "cm", scale=0.8)



##############################


us.ur <-  fredr(series_id="UNRATE", observation_start=as.Date("2001-02-01"),
                observation_end=as.Date("2020-12-01"),
                frequency="m")

us.employment <- fredr(series_id="PAYEMS", observation_start=as.Date("2001-02-01"),
                       observation_end=as.Date("2020-12-01"),
                       frequency="m")

us.vacancy<-tibble(date = us.ur$date,
                   unemploy = us.ur$value,
                   value = ((US.job.openings$value) / (US.job.openings$value+us.employment$value))*100)

us.vacancy.2007 <- subset(us.vacancy, us.vacancy$date >= as.Date("2000-02-01") & us.vacancy$date <= as.Date("2007-12-01"))
us.vacancy.2010 <- subset(us.vacancy, us.vacancy$date >= as.Date("2008-01-01") & us.vacancy$date <= as.Date("2010-12-01"))
us.vacancy.2019 <- subset(us.vacancy, us.vacancy$date >= as.Date("2011-01-01") & us.vacancy$date <= as.Date("2019-12-01"))
us.vacancy.2021 <- subset(us.vacancy, us.vacancy$date >= as.Date("2020-01-01") & us.vacancy$date <= as.Date("2021-12-01"))

us.vacancy.date <- slice(us.vacancy.2021, c(8,5,3,12,4))

graph3 <-ggplot()+
  geom_point(data = us.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_line(data = us.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_point(data = us.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_line(data = us.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_point(data = us.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_line(data = us.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_point(data = us.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_line(data = us.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_text(data = us.vacancy.date,aes(x=unemploy-0.2, y=value - 0.15, label = date))+
  scale_color_manual(name = "", 
                     values = c("2000-2007" = "black", "2008-2010" = "#82da8e", "2011-2019" = "blue", "2020-2021" = "red"))+
  labs(x="Unemployment rate (%)",
       y="Vacancy rate (%)",
       title = "Beveridge Curve - National",
       caption = "") + 
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size=12),
        plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        axis.text.y = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5))

graph3
ggsave("beveridgeUS.png", width = 22.70, height = 14.5, units = "cm", scale=0.8)

###################################

ne.ur <-  fredr(series_id="CNERUR", observation_start=as.Date("2001-02-01"),
                observation_end=as.Date("2020-12-01"),
                frequency="m")

ne.employment <- fredr(series_id="LASRD910000000000005", observation_start=as.Date("2001-02-01"),
                       observation_end=as.Date("2020-12-01"),
                       frequency="m")

ne.vacancy<-tibble(date = ne.ur$date,
                   unemploy = ne.ur$value,
                   value = ((ne.job.openings$value) / (ne.job.openings$value+(ne.employment$value/1000))*100))

ne.vacancy.2007 <- subset(ne.vacancy, ne.vacancy$date >= as.Date("2000-02-01") & ne.vacancy$date <= as.Date("2007-12-01"))
ne.vacancy.2010 <- subset(ne.vacancy, ne.vacancy$date >= as.Date("2008-01-01") & ne.vacancy$date <= as.Date("2010-12-01"))
ne.vacancy.2019 <- subset(ne.vacancy, ne.vacancy$date >= as.Date("2011-01-01") & ne.vacancy$date <= as.Date("2019-12-01"))
ne.vacancy.2021 <- subset(ne.vacancy, ne.vacancy$date >= as.Date("2020-01-01") & ne.vacancy$date <= as.Date("2021-12-01"))


ne.vacancy.date <- slice(ne.vacancy.2021, c(8,7,12,4)) #selecting months to display data
ne.vacancy.date.March <- slice(ne.vacancy.2021, c(3)) #selecting months to display data

graph4 <-ggplot()+
  geom_point(data = ne.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_line(data = ne.vacancy.2007, aes(x=unemploy, y=value, color="2000-2007"))+
  geom_point(data = ne.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_line(data = ne.vacancy.2010, aes(x=unemploy, y=value, color="2008-2010"))+
  geom_point(data = ne.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_line(data = ne.vacancy.2019, aes(x=unemploy, y=value, color="2011-2019"))+
  geom_point(data = ne.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_line(data = ne.vacancy.2021, aes(x=unemploy, y=value, color="2020-2021"))+
  geom_text(data = ne.vacancy.date,aes(x=unemploy-0.3, y=value - 0.1, label = date))+
  geom_text(data = ne.vacancy.date.March,aes(x=unemploy+0.03, y=value - 0.1, label = date))+
  scale_color_manual(name = "", 
                     values = c("2000-2007" = "black", "2008-2010" = "#82da8e", "2011-2019" = "blue", "2020-2021" = "red"))+
  labs(x="Unemployment rate (%)",
       y="Vacancy rate (%)",
       title = "Beveridge Curve - Northeast",
       caption = "") + 
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size=12),
        plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        axis.text.y = element_text(color = "black",  #Adjust/rotate the labels
                                   size = 12, angle = 360),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5))

graph4
ggsave("beveridgeNE.png", width = 22.70, height = 14.5, units = "cm", scale=0.8)

