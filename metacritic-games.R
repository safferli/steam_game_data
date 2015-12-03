library(httr)
# set_config(use_proxy(url="10.26.0.19", port=3128))
set_config(use_proxy(url="proxy.ubisoft.org", port=3128))
library(jsonlite)
library(magrittr)
library(dplyr)
library(data.table)


## set your own Mashape key in your ~/.Renviron 
# http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
# option 4: In a .Renviron file
Mashape.Key <- Sys.getenv("Mashape.key")


f.call.metacritic.API <- function(game, key = Mashape.Key, platform = "pc") {
  ## Mashape API to get metacritic data: https://market.mashape.com/byroredux/metacritic-v2
  
  # http://stackoverflow.com/questions/30382196/use-mashape-with-r
  resp <- GET(paste0("https://metacritic-2.p.mashape.com/find/game?platform=", platform, "&title=", game), 
              add_headers("X-Mashape-Key" = key,
                          "Accept" = "application/json"))
  # prints the headers
  #headers(resp)
  # prints the content of the response
  #str(content(resp))
  
  ## bind results into a dataframe
  metacritic <- as.data.frame(
    fromJSON(
      # replace NULL with empty strings in json, so that we can coerce into a dataframe without errors
      # jsonlite has no nullValue option, see issue #70: https://github.com/jeroenooms/jsonlite/issues/70
      gsub(":null,", ":\"\",", content(resp, as = "text"))
      ), stringsAsFactors = FALSE)
  # this is far nicer, do.call makes better dfs
  # but resulting df has content as list of length 1... not good :( 
  #metacritic <- data.frame(do.call(rbind, fromJSON(content(resp, as = "text")))))
  
  ## return data 
  return(metacritic)
}


f.generate.metacritic.data <- function(games.vector) {
  ## generate a dataframe of metacritic API data for a given vector of game names
  # games.vector needs to be a character vector! 
  if(!is.character(games.vector)) stop("f.generate.metacritic.data(): input is not a character vector")
  
  # clean the namelist -- API does not recognise spaces, and metacritic removes & and ndashes in titles
  # replace spaces
  games.web <- gsub(" ", "%20", 
                    # remove "-" ndash
                    gsub(" - ", "  ", 
                         # remove "&" ampersand
                         gsub("&", "", games.vector)))
  
  # number of games in vector
  num.of.games <- length(games.web)

  ## initialise dataset
  mtac <- vector("list", num.of.games) 
  ## fill list with API call results
  for (i in seq_len(num.of.games)) {
    mtac[[i]] <- f.call.metacritic.API(games.web[i])
  }

  ## bind into one dataframe
  metacritic <- rbindlist(mtac, fill = TRUE) 
    # remove last stray column (from the empty/missing results of the API calls)
    #select(-result) %>% # this breaks if there are no invalid calls
    # remove leading "result." in variable names
  metacritic %<>%  setNames(gsub("^result.", "", names(metacritic)))
  
  # generate clean dataset from original games list
  metacritic <- data.frame(name = games.vector) %>% 
    merge(metacritic, all.x = TRUE)

  return(metacritic)
}


## get vector of game names in dataset
games <- unique(raw$game) %>% 
  # remove "symbol" unicode characters (trademark TM, (R), (C), etc)
  # \pS: http://www.regular-expressions.info/unicode.html
  gsub(pattern = "\\pS", replacement = "", perl = TRUE) %>% 
  sort()
# testing: ® and ™ are both in Warhammer titles
#games[grepl("Warhammer", games)]


## generate dataset
metacritic <- f.generate.metacritic.data(games)
## export to avoid future querying of API
write.csv(metacritic, file="metacritic-games-unclean.csv", row.names = FALSE)


## find "missing" games -- where the direct search on API did not return anything

## these games we already have
good <- metacritic[!is.na(metacritic$thumbnail), ]

## missing
bad <- metacritic[is.na(metacritic$thumbnail), c(1,2)] %>% 
  # remove beta games
  filter(!grepl(name, pattern = "beta", ignore.case = TRUE)) 

## export for manual cleaning
write.csv(bad, file="missing-from-API.csv", row.names = FALSE)

## manually clean the list...

## import manual list of clean game names
clean <- read.csv(file = "missing-games.csv", sep = ";", stringsAsFactors = FALSE)

## get vector of missing game names
missing.games <- clean %>% 
  # imported data does not have NA, but rather empty strings
  filter(metacritic != "") %>% 
  # keep only the proper metacritic name
  select(name, metacritic)  %>% 
  mutate(
    # Warhammer 40,000k :(
    metacritic = gsub("0,0", "00", metacritic)
  )

# get vector of proper metacritic names
missing.games.metacritic <- as.vector(missing.games[,2]) 


## get remaining games
missing <- f.generate.metacritic.data(unique(missing.games.metacritic))


## merge full dataset
metacritic.clean <- rbind(good, missing) %>% 
  arrange(name)


## export for future use
write.csv(metacritic.clean, file = "metacritic-games-clean.csv", row.names = FALSE)










