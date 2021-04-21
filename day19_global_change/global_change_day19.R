

# Global Change Day 19------------------------------------------

# load libraries -------------------------------------
library(streamgraph)

library(rnaturalearth)
library(rnaturalearthdata)
ls("package:rnaturalearthdata")
library(rgdal)
library(sp)


library(tidyverse)
library(janitor)
library(scales)
library(magick)
library(here)
library(sf)


# load data --------------------------------------
library(downloader)
url="https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip"
download(url, dest="ihme-covid19_latest.zip", mode="wb") 
unzip("ihme-covid19_latest.zip")

latest_IHME <- read.csv("reference_hospitalization_all_locs.csv")
deaths_IHME <- latest_IHME%>%select(date,location_name,contains("dea"),-deaths_data_type)
names(deaths_IHME)

deaths_long <- deaths_IHME%>%
  mutate(name=location_name)%>%
  select(-location_name)%>%
  pivot_longer(cols=contains("dea"),
               names_to="deaths",
               values_to="value")

deaths_long$value[is.na(deaths_long$value)] <- 0
sum(is.na(deaths_long))
utils::write.csv(deaths_long,"deaths_IHME.csv")


###################################

df <-read.csv("deaths_IHME.csv")
head(df)



plyr::count(df$deaths)

only_deaths <- df%>%filter(deaths==c("deaths_lower","deaths_mean","deaths_upper"))

# use rnaturalearthdata -------------------------------------

# ?ne_countries() - Get natural earth world country polygons
world_raw <- ne_countries(scale = 50, returnclass = "sf") %>% 
  clean_names()

names(world_raw)
glimpse(world_raw)

# select desired variables
w_df <-world_raw%>%
  select(name,geometry,pop_est,gdp_md_est,gdp_year)

# set NAs as 0 values
w_df$gdp_year[is.na(w_df$gdp_year)] <- 0

glimpse(w_df)

# CRS coordinate ref systems:
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf

df_join <- df %>% 
  inner_join(w_df, by = c("name")) %>%
  st_as_sf(crs = 4326) %>% 
  st_transform(
    "+proj=vandg +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs"
  )

head(df)
library(forcats)

top_15 <- df_join %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  mutate(name = fct_lump(name, 15, w = pop_est)) %>% 
  filter(as.character(entity) != "Other")


  


# plotting -------------------------


ggplot(data= deaths_long, 
       aes(x=date, y=value)) + 
  geom_point()  


plyr::count(only_deaths$name)
my_df <-subset(only_deaths,name==c("China","United States of America","Brazil"))



range(my_df$value)


my_df$date<-as.Date(my_df$date)
class(my_df$date)
library(lubridate)

plot_df<-my_df%>%mutate(month=month(date),
               year=year(date),
               month.year=paste(month,year,sep="-"))%>%
  arrange(month.year)%>%
  group_by(month.year,date,name,deaths)%>%
  summarize(avg_value=mean(value))%>%
              ungroup()

glimpse(plot_df)

plot_df$month.year<-as.factor(as.Date(plot_df$month.year,"%m%Y"))

plot_df_up <-plot_df%>%filter(deaths=="deaths_upper")

library(xkcd)
xrange <-range(plot_df_up$month.year)
yrange <- range(plot_df_up$avg_value)
ggplot(plot_df_up) +
  geom_point(aes(x=factor(month.year),y=avg_value))+
xkcdaxis(xrange,yrange)

streamgraph(data = plot_df, 
            key="name", 
            value="avg_value", 
            date="month.year", 
            height="300px", 
            width="1000px") %>%
  sg_legend(show=TRUE, label="deaths: ")





