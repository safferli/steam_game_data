rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx4096m")#8192
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)

#library(bbbi)
library(readr)
library(dplyr)
#library(tidyr)
#library(data.table)
library(ggplot2)

# Define your workspace: "X:/xxx/"
wd <- "~/Documents/github/steam_game_data/"
setwd(wd)

ratings <- read_csv("../steam_game_data/metacritic-games-clean.csv")

ratings %<>% filter(!is.na(score) & !is.na(userscore)) %>% 
  mutate(
    userscore = userscore*10,
    score.diff = userscore - score,
    score.diff.perc = score.diff/score
    ) %>% 
  select(name, score, userscore, score.diff, score.diff.perc, everything())

ratings %>% ggplot()+
  geom_histogram(aes(x=score.diff.perc, fill=genre))

ratings %>% ggplot()+
  geom_point(aes(x=rlsdate, y=score.diff.perc, colour = genre))+
  geom_text(data = ratings %>% filter(abs(score.diff.perc)>0.33), 
            aes(
              x = rlsdate, 
              y = score.diff.perc, 
              label = name, 
              angle = 45,
              hjust = 1, vjust = 0.5
              ), alpha = 0.9, size = 2.5
            )


