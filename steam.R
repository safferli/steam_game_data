rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx4096m")#8192
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)
#library(bbbi)
library(ggplot2)
#library(RODBC)
library(dplyr)
library(tidyr)
#library(data.table)

# Define your workspace: "X:/xxx/"
wd <- "D:/github/steam_game_data/"
setwd(wd)

raw <- read.csv(file = "steam_stats.csv", stringsAsFactors = FALSE) %>% 
  # hourly data, top 99 games it seems
  mutate(date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))

summary(raw$date)

games <- unique(raw$game)%>% sort()

dta.days <- unique(raw$date) 

max(raw$date) - min(raw$date)


dta <- raw %>% 
  mutate(
    CCU.relative.to.peak = current_players/peak_today
  )

hourly <- raw %>% 
  group_by(date) %>% 
  summarise(
    CCU.top99 = sum(current_players),
    CCU.top1.relative.to.top99 = max(current_players)/sum(current_players),
    top1.game = first(game)
  )

# DOTA2 is the most-played game
hourly %>% group_by(top1.game) %>% tally()


# god, I hate Windows...
Sys.setlocale("LC_TIME", "English")

hourly %>% 
  # needs Sys.setlocale() to match the correct name here
  filter(months(date)=="June") %>% 
  ggplot()+
  geom_line(aes(x=date, y=CCU.top1.relative.to.top99))+
  xlim(c(as.POSIXct("2015-06-01"), as.POSIXct("2015-06-03")))



  


##### API

# http://stackoverflow.com/questions/30382196/use-mashape-with-r

#imports an http R library
library(httr)
# set_config(use_proxy(url="10.26.0.19", port=3128))
set_config(use_proxy(url="proxy.ubisoft.org", port=3128))

#perform a GET request on the URL, with two headers and store in a resp variable
resp <- GET("https://metacritic-2.p.mashape.com/find/game?platform=pc&title=Portal", 
            add_headers("X-Mashape-Key" = "JAhfTs60KOmshsg6uhdjU5uM05vgp1Z0edDjsnjK68v0TLoQMm",
                        "Accept" = "application/json"))

#Prints the headers
headers(resp)

#Prints the content of the response
str(content(resp))

# curl --get --include 'https://metacritic-2.p.mashape.com/find/game?platform=pc&title=Portal' \
# -H 'X-Mashape-Key: JAhfTs60KOmshsg6uhdjU5uM05vgp1Z0edDjsnjK68v0TLoQMm' \
# -H 'Accept: application/json'

library(jsonlite)

test <- as.data.frame(unlist(fromJSON(content(resp, as = "text"))))


  





