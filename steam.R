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


## load Steam data
raw <- read.csv(unz("steam_stats.csv.zip", "steam_stats.csv"), stringsAsFactors = FALSE, encoding="UTF-8") %>% 
  # hourly data, top 99 games it seems
  mutate(date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  # no price information in this dataset, remove empty columns
  select(-contains("price"))

## if metacritic data does not exist, pull from Mashape API
if (!file.exists("metacritic-games.csv")) {
  source(metacritic-games.R)
}
## load metacritic data
metacritic <- read.csv("metacritic-games-clean.csv", stringsAsFactors = FALSE)


# gamenames.steam <- data.frame(name = unique(raw$game), steam = 1L, stringsAsFactors = FALSE)
# gamenames.metacritic <- data.frame(name = metacritic$name, metacritic = 1L, stringsAsFactors = FALSE)
# 
# gamenames <- gamenames.steam %>% 
#   merge(gamenames.metacritic, all = TRUE) %>% 
#   arrange(name)

## create mapping from Steamname to Metacriticname of each game
mapping <- data.frame(steamname = unique(raw$game), stringsAsFactors = FALSE) %>% 
  # automatically clean names: remove "symbol" unicode characters (trademark TM, (R), (C), etc)
  mutate(
    metacriticname = gsub(pattern = "\\pS", replacement = "", steamname, perl = TRUE) 
  ) %>% 
  # add manually cleaned/changed game names
  merge(
    read.csv(file = "missing-games.csv", sep = ";", stringsAsFactors = FALSE), 
    all.x = TRUE, by.x = "steamname", by.y = "name"
  ) %>% 
  # correct metacritic name is either the automatically cleaned name, or the manually cleaned one
  mutate(
    temp = ifelse((is.na(metacritic) | metacritic == ""), metacriticname, metacritic)
  ) %>% 
  # keep only the two name columns
  select(steamname, metacriticname = temp) %>% 
  arrange(steamname) 
# remove duplicates
mapping <- mapping[!duplicated(mapping),]


## generate dataset to work from
dta <- raw %>% 
  select(timestamp = date, game, current_players, peak_today) %>% 
  merge(
    # get metacritic data, but map to actual Steam game names
    merge(metacritic, mapping, by.x = "name", by.y = "metacriticname") %>% 
      select(name = steamname, score, userscore, genre, publisher, developer, rating, rlsdate),
    by.x = "game", by.y = "name" 
  ) %>% 
  mutate(
    rlsdate = as.Date(rlsdate),
    CCU.relative.to.peak = current_players/peak_today
    )

## we lose some data from metacritic and double-setups 
nrow(dta)/nrow(raw) # 96% 

## check which games we lose
compare <- merge(
  # x = steam
  raw %>% group_by(game) %>% tally(),
  # y = combined
  dta %>% group_by(game) %>% tally(),
  by = "game", all.x = TRUE
  ) %>% 
  filter((is.na(n.y) | n.x != n.y)) %>% 
  arrange(game)







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












