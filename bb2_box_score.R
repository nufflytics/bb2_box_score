#Call with name of league as argument eg. Rscript bb2_box_score.R REBBL

suppressMessages(require(tidyverse))
suppressMessages(require(purrrlyr))
suppressMessages(require(magrittr))
suppressMessages(require(httr))
suppressMessages(require(rvest))
suppressMessages(require(stringr))

log_message = function(message) { write_lines(paste(Sys.time(),message, sep = "\t"), "data/log.txt", append = TRUE)}

##Setup
league_file <- commandArgs(trailingOnly = T)[1]
testing = length(commandArgs(trailingOnly = T)) > 1

#load webhook info, last processed game, and API calls from file
load(paste0("data/",league_file,"_parameters.Rda"))
load(paste0("data/",league_file,"_last_seen.Rda"))
load("data/api.Rda")

if (testing) {
  webhook <- webhook %>% map(~inset(.,"https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ"))
  last_seen <- last_seen %>% map(~ strtoi(., base=16) %>% subtract(1) %>% as.hexmode() %>% format(width = 9))  
}

#Get data for leagues
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
    str_replace_all("^1","") # strip initial 1 from uuid so unrecorded games have ID = 0
  
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
  names(league_search_strings), 
  ~filter_league_table(league_data[[.]],last_seen[[.]], .)
)

##
#For new games, gather competition info/game stats
##
get_stats <- function(uuid, hometeam, awayteam, clan = FALSE) {
  
  keep_stats <- c(TD = "touchdowns",BLK = "tackles", AVBr = "injuries", CAS = "casualties", KO = "ko", RIP = "dead", PASS = "passes", CATCH = "catches", INT = "interceptions")
  stat_order <- c("TD", "BLK", "AVBr","KO","CAS","RIP","INT","PASS","CATCH")
  
  conv_name <- function(n) {keep_stats %>% extract(.==n) %>% names}
  
  #Get raw stats
  stats <- stats_api_query(uuid) %>% 
    content %>% 
    html_table %>% 
    extract2(1) 
  
  #Check for admin results by seeing if any fans turned up
  had_fans <- stats %>% filter(STAT == "supporters") %>% extract(,2:3) %>% sum %>% magrittr::is_greater_than(0)
  if (!had_fans) return(NULL)
  
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
  abbr <- function(name, clan) {
    if(!clan) {
      name %>% 
        str_replace_all("[_.-]([A-Z])"," \\1") %>% # if a replace [_.-] followed by a capital letter with a space  
        str_replace_all("[!,'()]",'') %>% # delete these characters
        str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%  #add a space before mid-word capitals
        abbreviate(1)
    } else {
      name %>% 
        str_extract("\\[.*\\]")
    }
  }
  
  #Have to pad away team name to prevent ugly linebreaks on some devices (and introduce some ugly scroll bars on others, but oh well)
  stats %>% 
    knitr::kable(row.names = F, col.names = c("", abbr(hometeam, clan), str_c(abbr(awayteam, clan),"    ")), format = "pandoc", align = "lrl") %>% 
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
  
  stats <- get_stats(g[['uuid']], g[['h_team']],g[['a_team']], clan = league=="Clan")
  
  #stats will return null if came conceeded. Don't post those.
  if (is.null(stats)) return(NULL)
  
  #Make the winning team's name bold
  if (g[['h_score']] > g[['a_score']]) g[['h_team']] <- paste0("**", g[['h_team']], "**")
  if (g[['a_score']] > g[['h_score']]) g[['a_team']] <- paste0("**", g[['a_team']], "**")
  
  #Work out team race from image urls
  g[['h_race']] <- g[["h_img"]] %>% str_replace(".*Picto_","") %>% str_replace("\\.png","") %>% str_replace("(.)([A-Z])", "\\1 \\2")
  g[['a_race']] <- g[["a_img"]] %>% str_replace(".*Picto_","") %>% str_replace("\\.png","") %>% str_replace("(.)([A-Z])", "\\1 \\2")
  
  # Embed format is 
  # title: 'coach v coach'
  # description: 'team v team'
  #              'race v race'
  #              'comp name'
  embed = list(
    list(
      title = paste0(g[['h_coach']], " V ",g[['a_coach']]),
      description = paste0(g[['h_team']], " V ", g[['a_team']], "\n",
                           g[['h_race']], " V ", g[['a_race']], "\n",
                           "*", g[['comp']],"*"),
      url = paste0("http://www.mordrek.com/goblinSpy/web/game.html?mid=", g[['uuid']]),
      thumbnail = list(url = thumbnails[[league]] ),
      color = colours[[league]],
      #timestamp = Sys.time() %>% lubridate::ymd_hms(tz = "Australia/Sydney"),
      fields = list(
        list(name = "Game Stats", value = stats, inline = F)
      )
    )
  )
  
  # #Notify @here and users who have requested it
  # mention = function(user_id) {paste0("<@",user_id,">")}
  # 
  # if (exists("notifications")) {
  #   mentions = ""
  #   #if( "at_here" %in% names(notifications) ) mentions = str_c(mentions,"@here")
  #   #if( "at_role" %in% names(notifications) ) mentions = str_c(mentions,"<@&324372474680705034>")
  #   #if( g[['h_coach']] %in% names(notifications) ) mentions = str_c(mentions,mention(notifications[[g[['h_coach']]]]))
  #   if( g[['a_coach']] %in% names(notifications) ) mentions = str_c(mentions,mention(notifications[[g[['a_coach']]]]))
  # } else {
  #   mentions = ""
  # }
  
  
  log_message(paste("Posting update for",embed[[1]]$title, "uuid:", g[['uuid']], "competition:", g[['comp']], "league:",league))
  #print(embed)
  response = POST(webhook[[league]], body = list(username = bot_usernames[[league]], avatar_url = bot_avatar[[league]], embeds = embed), encode = "json")
  
  if (response$status_code == 429) { #rate-limited
    wait_time <- content(response)$retry_after
    print(paste("Rate limited, pausing for",wait_time,"seconds."))
    Sys.sleep(wait_time)
  }
  
  log_message(paste("Returned:", response$status_code))
  
  return(response$status_code)
}


#If new games, post them
if (nrow(new_games) > 0 ) {
  posting_result <- new_games %>% 
    by_row(post_message, .to = "status_code")
} else {
  posting_result <- tibble()
}

##
#Update last seen information if a game has been posted and we are not testing
##
if (nrow(posting_result) > 0) {
  most_recent <- posting_result %>% 
    filter(status_code == '204') %>% 
    group_by(league) %>% 
    filter(ID == max(ID))
  
  last_seen[most_recent$league] <- most_recent$uuid
}

if (!testing) save(last_seen, file = paste0("./data/", league_file, "_last_seen.Rda"))
