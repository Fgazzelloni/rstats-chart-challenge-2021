# Statistics Day9 ------------------------------------

library(tidyverse)
library(ggthemes)
library(ggrepel)


# load data --------------------

# url = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv"

df_backup <- readxl::read_excel("download.xlsx")

############################################################
subtitle = "from Feb 2020 to April 2021"
caption = "Viz Federica Gazzelloni | Datasource: ECDC | Statistics - Day9"

max<-max(df$weekly_count)
df <- read.csv("selected_countries.csv")
df <- tibble(df)

df$country[1]<- "US"

png("stats.png", width = 350, height = 350)
my_bar <- barplot(height=df$weekly_count,
          names=df$country,
          density=c(5,10,20,30,7),
          angle=c(0,45,90,11,36) ,
          xlab="",font.axis=1,
          col="brown",las=2 ,ylim=c(0,5500),
        main = "Selected countries by higher Covid19 Deaths")
text(my_bar,df$weekly_count+114,labels=df$weekly_count,cex=0.9)
mtext(side=3,"2021-week 13",line=-2,cex=1.5)
mtext(side=3,"Viz Federica Gazzelloni | Datasource: ECDC | Statistics - Day9",
      line=-5,cex=0.8)

dev.off()


