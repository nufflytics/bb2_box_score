library(tidyverse)
library(magrittr)
library(httr)
library(rvest)
library(stringr)
webhook <- "https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ"

last_seen <- readRDS("data/last_seen.Rds") 

#Colours from Harringzord
# REL was R 126, G 0, B 0
# GMAN was R 0, G 14, B 119
# Big O was R 145, G 124, B 6
colours <- list(REL = 0x7e0000, Gman = 0x000e77, BigO = 0x917c06, Spins = 0x51bf38)
thumbnails <- list(BigO = "https://i.redd.it/m7lj05c8hcby.png", Spins = "http://i.imgur.com/Vn51r9z.png", REL = "https://i.redd.it/m7lj05c8hcby.png", Gman = "https://i.redd.it/m7lj05c8hcby.png")

league_search_strings <- list(Spins = "Post_Season_Spin", BigO = "The+Big+O")

##
#Check if md5 hashes are different for leagues
##

league_html_response <- map(league_search_strings, ~POST(paste0("http://web.cyanide-studio.com/ws/bb2/?league=",.,"&platform=pc&ajax=entries")))

check_md5s <- function(old_md5, new_response) {
  new_md5 = new_response %>% content(as = "text") %>% openssl::md5()
  new_md5 != old_md5
}

leagues_with_new_data <- map2_lgl(last_seen$md5, league_html_response, check_md5s) %>% 
  extract( . == TRUE ) %>% 
  names()

##
#For leagues that pass above, find out which game uuids are new
##

#First, build up game info table, since we already have the data from the request
get_league_data <- function(league_response) {
  response_content <- content(league_response) 
  
  league_games <- response_content %>% 
    html_table %>% 
    extract2(1) %>% # Get first html table in response
    set_colnames(c("comp","round","h_coach","h_team","h_img","score","a_img","a_team","a_coach")) %>% 
    separate(score,c("h_score","a_score"))
  
  league_games$uuid <- response_content %>% 
    html_nodes("[data]") %>% 
    html_attr("data") %>% 
    unique
  
  league_games
}


league_data <- map(leagues_with_new_data, ~ get_league_data(league_html_response[[.]])) %>% 
  set_names(leagues_with_new_data)

# Then, filter data based on which games come after (above) the last seen uuid
filter_league_table <- function(league_table, last_game, league) {
  league_table %>% 
    filter(cumsum(uuid %in% last_game) == 0) %>% # cumsum gives 0 down to old uuid, then 1
    mutate(league = league) # adds league name for channel identification
}

new_games <- map_df(
  leagues_with_new_data, 
  ~filter_league_table(league_data[[.]],last_seen[['uuid']][[.]], .)
  )

##
#For new games, gather competition info/game stats
##
get_stats <- function(uuid) {
  POST(paste0("http://web.cyanide-studio.com/ws/bb2/?platform=pc&ajax=entry&id=",uuid)) %>% 
    content %>% 
    html_table %>% 
    extract2(1) %>% 
    set_colnames(c("stat","h_stat","a_stat")) %>% 
    gather(team,value,-stat) %>% 
    spread(stat,value) %>% 
    set_rownames(c("away","home"))
}

##
#Post messages to channel
##
post_message <- function(g) {
  league = g[['league']]
  #Testing
  s <- test_stats[[g[['uuid']]]]
  #stats <- get_stats(g['uuid'])
  
  #Prepare the fields portion of the Discord embed
  # top bits will be: blank (or 'stat'), Short version of h_team, short a_team  
  # bottom bits will be stats, with 'best' one bolded
  #This is still ugly. Try some knitr::kable style formatting
  
  stat_ids <- list(name="Stat",value = paste("TD","CAS","RIP","P/C", sep = "\n"), inline = T)
  

  h_stats <- list(
    name = g[['h_team']] %>% str_replace_all("[.!]",'') %>%  abbreviate(1) %>% as.character(),
    value = paste(s['home','touchdowns'], s['home','casualties'], s['home',"dead"],paste(s['home','passes'],s['home',"catches"],sep = "/"), sep = "\n"),
    inline = T
    )
  
  a_stats <- list(
    name = g[['a_team']] %>% str_replace_all("[.!]",'') %>%  abbreviate(1) %>% as.character(),
    value = paste(s['away','touchdowns'], s['away','casualties'], s['away',"dead"],paste(s['away','passes'],s['away',"catches"],sep = "/"), sep = "\n"),
    inline = T
  )
  
  embed_fields <- list(
    stat_ids, h_stats, a_stats
  )
  
  embed = list(
    list(
      title = paste0(g[['h_coach']], " V ",g[['a_coach']]),
      description = paste0(g[['h_team']], " V ",g[['a_team']], "\n*", g[['comp']],"*"),
      url = paste0("http://www.mordrek.com/goblinSpy/web/game.html?mid=", g[['uuid']]),
      thumbnail = list(url = thumbnails[[league]] ),
      color = colours[[league]],
      #timestamp = Sys.time() %>% lubridate::ymd_hms(tz = "Australia/Sydney"),
      fields = embed_fields
    )
  )
  
  print(paste("Posting update for",embed[[1]]$title))
  #print(embed)
  response = POST(webhook, body = list(username = "REBBL Updates", avatar_url = "https://fullmetalcos.teemill.co.uk/uploaded/thumbnails/B64-WEjBTk_10057021_autox120.png", embeds = embed),encode = "json")

  if (response$status_code == 429) { #rate-limited
    wait_time <- content(response)$retry_after
    print(paste("Rate limited, pausing for",wait_time,"seconds."))
    Sys.sleep(wait_time)
  }
  
}

posting_result <- new_games %>% 
  by_row(post_message, .to = "response")

##
#Update last seen information
##


