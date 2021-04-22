
# http://www.bdlc.umontreal.ca/CHMD/prov/que/que.htm
# https://github.com/ZainulArifin1/WeeklyPlot/tree/main/Week%202%2030%20DAY%20CHART


library(tidyverse)
library(ggpol)
library(gganimate)
library(gifski)
library(extrafont)
library(showtext)
showtext_auto(enable=TRUE)

options(scipen = 999)



df <- read.delim("https://www.prdh.umontreal.ca/BDLC/data/que/Exposures_5x1.txt",sep="",skip=1)

head(df)

df%>%ggplot(aes(x=Year,y=Total,group=Age,color=factor(Age)))+
  geom_line()

plyr::count(my_df$Age)


my_df <- df%>%
  mutate(Age = as.factor(case_when(Age =="1-4" ~ "00-04" ,TRUE ~ Age)))

my_df <- my_df %>%
  dplyr::mutate(Age = as.factor(as.character(case_when(Age == "5-9" ~ "05-09",TRUE ~ Age))))

#%>%
  arrange(Year,desc(Age))%>%
  filter(!Age=="110+" & !Age=="100-104" & !Age=="105-109" & !Age=="0")%>%
  pivot_longer(cols=c("Female","Male"),names_to="Gender",values_to="Exposure")%>%
  select(-Total)


my_df %>%
  ggplot(aes(x = Age, y = Exposure, fill = Gender)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#29ffc6', '#d07be0')) +
  scale_y_continuous(
    breaks = c(0, 10000000, 20000000, 30000000, 40000000),
    label = c("0M", "10M", "20M","30M", "40M")
  ) +
  scale_y_continuous(
    breaks = c(0, 10000000, 20000000, 30000000, 40000000),
    label = c("0M", "10M", "20M","30M", "40M")
  ) +
  coord_flip(clip = "off") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#5e5556") ,
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(family="World of Water"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 14),
    legend.key.size = unit(0.75, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(
      family="World of Water",
      size = 15,
      face = 'bold',
      color = 'white'
    ),
    plot.title = element_text(
      family="World of Water",
      size = 22,
      hjust = 0.5,
      face = 'bold',
      color = 'white'
    ),
    plot.subtitle = element_text(
      family="World of Water",
      size = 8,
      hjust = 0,
      face = 'bold',
      color = 'white'
    ),
    axis.title.x = element_text(
      family="World of Water",
      size = 16,
      face = 'bold',
      color = '#242422'
    ),
    axis.text.x = element_text(
      family="World of Water",
      size = 16,
      face = 'bold',
      color = 'white'
    ),
    axis.text.y = element_text(
      family="World of Water",
      size = 10,
      face = 'bold',
      color = 'white'
    ),
    plot.caption = element_text(
      family="World of Water",
      size = 8,
      hjust = 1,
      face = 'bold',
      color = '#c426c9'
    ),
    plot.margin = margin(10,10,10,10)
  )


PopPyramid <- my_df +
  labs(
    title = 'Quebec Exposure-to-risk Change\n\n{1921-2016}',
    subtitle = '',
    y = '\n\nExposure-to-risk',
    caption = '@fgazzelloni | \n\nData Source: http://www.bdlc.umontreal.ca/CHMD/prov/que/que.htm'
  )

