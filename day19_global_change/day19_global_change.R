library(tidyverse)
library(streamgraph)
library(lubridate)


excess_of_mortality<-read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv")
head(excess_of_mortality)
names(head(excess_of_mortality))


plyr::count(excess_of_mortality$location)

plot <-excess_of_mortality%>%
  group_by(date, location) %>%
  tally(wt=p_scores_all_ages) %>%
  streamgraph("location", "n", "date", offset="zero", interpolate="linear") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Location: ")


sg_title(plot, title = "How is excess mortality changed Globally")



plot + theme_bw()








