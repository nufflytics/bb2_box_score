suppressMessages(library(tidyverse))
suppressMessages(library(purrrlyr))
suppressMessages(library(magrittr))
suppressMessages(library(httr))
suppressMessages(library(rvest))
suppressMessages(library(stringr))

##Setup
log_message = function(message) { write_lines(paste(Sys.time(),message, sep = "\t"), "data/log.txt", append = TRUE)}
#load webhook info and last processed game from file
load("data/webhook.Rda")
load("data/last_seen.Rda")

#webhooks for testing in private server
webhook <- list(
  REL = "https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ",
  Gman = "https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ",
  BigO = "https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ"
)

colours <- list(
  REL = 0x7e0000, 
  Gman = 0x000e77, 
  BigO = 0x917c06
#  Spins = 0x51bf38
)

thumbnails <- list(
#  Spins = "http://www.nufflytics.com/img/main/REBBL.png", 
  REL = "http://www.nufflytics.com/img/main/REL_s.png", 
  Gman = "http://www.nufflytics.com/img/main/Gman_s.png",
  BigO = "http://www.nufflytics.com/img/main/BigO_s.png" 
)

league_search_strings <- list(
  #Spins = "Post_Season_Spin", 
  REL = str_c("REL>Season+6+Div+",1:7),
  Gman = str_c("Gman+Cup>Season+6+Div+", 1:5),
  BigO = str_c("The+Big+O>Season+6+Div+", 1:3)
)

##
#Get game data for all leagues
##
api_query <- function(search_strings) {
  map(search_strings, ~POST(paste0("http://web.cyanide-studio.com/ws/bb2/?league=",.,"&platform=pc&ajax=entries")))
}

league_html_response <- map(league_search_strings, api_query)


##
#Find out which game uuids are new
##

#First, build up game info table, since we already have the data from the request
get_league_data <- function(league_response) {
  response_content <- content(league_response) 
  
  #Parse basic table information
  league_games <- response_content %>% 
    html_table %>% 
    extract2(1) %>% # Get first html table in response
    set_colnames(c("comp","round","h_coach","h_team","h_img","score","a_img","a_team","a_coach")) %>% 
    separate(score,c("h_score","a_score")) %>% 
    filter(!is.na(round)) # remove competition headers
  
  #Add uuids from the [data] attribute of html nodes
  league_games$uuid <- response_content %>% 
    html_nodes("[data]") %>% 
    html_attr("data") %>% 
    extract(seq(1,length(.),by=10)) %>% # have the uuid listed 10 times per table row, so just take one
    str_replace_all("^1","") # strip initial 1 from uuid so unrecorded games are 0
  
  # Perform final conversions (filtering for correct leagues -- thanks REL, getting numeric uuids, etc)
  league_games %>% 
    mutate(ID = strtoi(uuid, base = 16)) 
}

#For each league, process the division data into a df and bind them all together
league_data <- map(league_html_response, ~ map_df(.,get_league_data))

# Then, filter data based on which games come after (above) the last seen uuid
filter_league_table <- function(league_table, last_game, league) {
  league_table %>% 
    filter(ID > strtoi(last_game, base=16)) %>% # Assume uuid are assigned in increasing order (appears to be the case)
    mutate(league = league) # adds league name for channel identification
}

new_games <- map_df(
  c("REL","Gman","BigO"), 
  ~filter_league_table(league_data[[.]],last_seen[[.]], .)
)

##
#For new games, gather competition info/game stats
##
get_stats <- function(uuid, hometeam, awayteam) {

  keep_stats <- c(TD = "touchdowns",BLK = "tackles", AVBr = "injuries", CAS = "casualties", KO = "ko", RIP = "dead", PASS = "passes", CATCH = "catches", INT = "interceptions")
  stat_order <- c("TD", "BLK", "AVBr","KO","CAS","RIP","INT","PASS","CATCH")
  
  conv_name <- function(n) {keep_stats %>% extract(.==n) %>% names}
  
  #Get raw stats
  stats <- POST(paste0("http://web.cyanide-studio.com/ws/bb2/?platform=pc&ajax=entry&id=1",uuid)) %>% #replacing leading 1 in uuid for url
    content %>% 
    html_table %>% 
    extract2(1) 
  
  #Check for admin concession by seeing if home team gets 1 MVP
  was_completed <- stats %>% filter(STAT == "mvp") %>% extract2(2) %>% magrittr::equals(1)
  if (!was_completed) return(NULL)
  
  #filter and convert
  stats <- stats %>% 
    filter(STAT %in% keep_stats) %>% 
    mutate(STAT = map_chr(STAT, conv_name) %>% factor(levels = stat_order)) %>% 
    arrange(STAT) %>% 
    set_colnames(c("STAT","home","away"))
  
  #Combine pass/catch stats for if they are needed
  pass_catch = data_frame(STAT = "P/C", home = paste0(stats[8,2],"/",stats[9,2]), away = paste0(stats[8,3],"/",stats[9,3]))
  
  #Filter out stats with no entries (keeping TDs always)
  filter = c(TRUE, rowSums(stats[-1, 2:3]) > 0)
  stats = stats[filter,]
  
  if (any(c("PASS","CATCH") %in% stats$STAT)) {
    stats %<>% mutate_all(as.character) %>% 
      filter(!STAT %in% c("PASS","CATCH")) %>% 
      bind_rows(pass_catch)
  }
  
  #Format it correctly
  abbr <- function(name) {
    name %>% str_replace_all("[_]"," ") %>%  str_replace_all("[.!,'()]",'') %>%  abbreviate(1)
  }
  
  #Have to pad away team name to prevent ugly linebreaks on some devices (and introduce some ugly scroll bars on others, but oh well)
  stats %>% 
    knitr::kable(row.names = F, col.names = c("", abbr(hometeam), str_c(abbr(awayteam),"    ")), format = "pandoc", align = "lrl") %>% 
    extract(-2) %>% #remove the underlines
    paste0(collapse = "\n") %>% 
    paste0("```R\n",.,"\n```") %>% 
    as.character()
}

##
#Post messages to channel
##
post_message <- function(g) {
  league = g[['league']]
  
  stats <- get_stats(g[['uuid']], g[['h_team']],g[['a_team']])
  
  #stats will return null if came conceeded. Don't post those.
  if (is.null(stats)) return(NULL)
  
  embed = list(
    list(
      title = paste0(g[['h_coach']], " V ",g[['a_coach']]),
      description = paste0("**",g[['h_team']], "** V **",g[['a_team']], "**\n*", g[['comp']],"*"),
      url = paste0("http://www.mordrek.com/goblinSpy/web/game.html?mid=", g[['uuid']]),
      thumbnail = list(url = thumbnails[[league]] ),
      color = colours[[league]],
      #timestamp = Sys.time() %>% lubridate::ymd_hms(tz = "Australia/Sydney"),
      fields = list(
        list(name = "Game Stats", value = stats, inline = F)
      )
    )
  )
  
  log_message(paste("Posting update for",embed[[1]]$title, "uuid:", g[['uuid']]))
  #print(embed)
  response = POST(webhook[[g[['league']]]], body = list(username = paste0(g[['league']]," Updates"), embeds = embed),encode = "json")
  
  if (response$status_code == 429) { #rate-limited
    wait_time <- content(response)$retry_after
    print(paste("Rate limited, pausing for",wait_time,"seconds."))
    Sys.sleep(wait_time)
  }
  
  log_message(paste("Returned:", response$status_code))
  
  return(response$status_code)
}

if (nrow(new_games) > 0 ) {
  posting_result <- new_games %>% 
    by_row(post_message, .to = "status_code")
} else {
  posting_result <- tibble()
}

##
#Update last seen information
##
if (nrow(posting_result) > 0) {
  most_recent <- posting_result %>% 
    filter(status_code == '204') %>% 
    group_by(league) %>% 
    filter(ID == max(ID))
  
  last_seen[most_recent$league] <- most_recent$uuid
}

save(last_seen, file = "./data/last_seen.Rda")
