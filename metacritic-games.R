library(httr)
# set_config(use_proxy(url="10.26.0.19", port=3128))
set_config(use_proxy(url="proxy.ubisoft.org", port=3128))
library(jsonlite)
library(data.table)


## set your own Mashape key in your ~/.Renviron 
# http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html
# option 4: In a .Renviron file
Mashape.Key <- Sys.getenv("Mashape.key")


## get list of games in dataset
games <- unique(raw$game) %>% 
  # remove "symbol" unicode characters (trademark TM, (R), (C), etc)
  # \pS: http://www.regular-expressions.info/unicode.html
  gsub(pattern = "\\pS", replacement = "", perl = TRUE) %>% 
  sort()
# testing: ® and ™ are both in Warhammer titles
#games[grepl("Warhammer", games)]

## replace spaces with "%20" for url handling
games.web <- gsub(" ", "%20", games)
# smaller dataset for testing
games.web <- head(games.web)


f.get.metacritic <- function(game, key = Mashape.Key) {
  ## get metacritic data from mashape API: https://market.mashape.com/byroredux/metacritic-v2
  # http://stackoverflow.com/questions/30382196/use-mashape-with-r
  resp <- GET(paste0("https://metacritic-2.p.mashape.com/find/game?platform=pc&title=", game), 
              add_headers("X-Mashape-Key" = key,
                          "Accept" = "application/json"))
  # prints the headers
  #headers(resp)
  # prints the content of the response
  #str(content(resp))
  
  ## bind results into a dataframe
  # replace NULL with empty strings in json, so that we can coerce into a dataframe without errors
  metacritic <- as.data.frame(fromJSON(gsub(":null,", ":\"\",", content(resp, as = "text"))), stringsAsFactors = FALSE)
  # this is far nicer, do.call makes better dfs
  #metacritic <- data.frame(do.call(rbind, fromJSON(content(resp, as = "text")))))
  
  ## return data 
  return(metacritic)
}



num.of.games <- length(games.web)

## initialise dataset
mtac <- vector("list", num.of.games) 
## fill list with API call results
for (i in seq_len(num.of.games)) {
  mtac[[i]] <- f.get.metacritic(games.web[i])
}

## bind into one dataframe
metacritic <- rbindlist(mtac, fill = TRUE) %>% 
  # remove last stray column (from the empty/missing results of the API calls)
  select(-result) %>% 
  # remove leading "result." in variable names
  setNames(gsub("^result.", "", names(metacritic)))


## generate clean dataset from original games list
metacritic <- data.frame(name = games) %>% 
  merge(metacritic, all.x = TRUE)


## export to avoid future querying of API
write.csv(metacritic, file="metacritic-games.csv", row.names = FALSE)









