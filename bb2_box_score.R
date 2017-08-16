#Call with name of league as argument eg. Rscript bb2_box_score.R REBBL

suppressMessages(require(tidyverse))
suppressMessages(require(purrrlyr))
suppressMessages(require(magrittr))
suppressMessages(require(httr))
suppressMessages(require(rvest))
suppressMessages(require(stringr))
suppressMessages(require(nufflytics))


log_message = function(message) { write_lines(paste(Sys.time(),message, sep = "\t"), "data/log.txt", append = TRUE)}

##Setup
league_file <- commandArgs(trailingOnly = T)[1]
testing = length(commandArgs(trailingOnly = T)) > 1

platform_code <- c(pc=0,ps4=1,xb1=2)

#load webhook info and API calls from file
load(paste0("data/",league_file,"_parameters.Rda"))
load("data/api.Rda")

#if parameters file doesn't have a platform, assume it's PC
#temporary fix until I go back and add parameters for leagues added early on

if (!exists("platform")) {
  platform = list()
  platform[names(league_search_strings)] <- "pc"
}

#Get data for leagues
league_html_response <- map2(league_search_strings, platform[names(league_search_strings)], api_query)

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
    filter(a_coach != "Coach 2")
  
  if(nrow(league_games)==0) return(NULL) # No games, don't process further
  
  #Add uuids from the [data] attribute of html nodes
  league_games$uuid <- response_content %>% 
    html_nodes("[data]") %>% 
    html_attr("data") %>% 
    magrittr::extract(seq(1,length(.),by=10)) %>% # have the uuid listed 10 times per table row, so just take one
    str_replace_all("^1[012]","") # strip initial 1<platform_code> from uuid so unrecorded games have ID = 0
  
  # add numeric ID for easy comparison and remove concedes (a_score is NA after above processing)
  league_games %>% 
    mutate(ID = strtoi(uuid, base = 16)) #%>% 
  #filter(!is.na(a_score))
}

#For each league, process the division data into a df and bind them all together
league_data <- map(league_html_response, ~ map_df(.,get_league_data))

# Then, filter data based on which games come after (above) the last seen uuid
filter_league_table <- function(league_table, last_game, league) {
  league_table %>% 
    filter(ID > strtoi(last_game, base=16)) %>% # Assume uuid are assigned in increasing order (appears to be the case)
    mutate(league = league) # adds league name for channel identification
}

#load last seen game and apply filter
load(paste0("data/",league_file,"_last_seen.Rda"))

# if we are only testing, adjust webhook and last_seen params as needed
if (testing) {
  type <- commandArgs(trailingOnly = T)[2]
  
  if( type=="public")  { #test on my server where people can see it
    webhook <- webhook %>% map(~inset(.,"https://discordapp.com/api/webhooks/314984670569693188/ySNOjupzvZIsielocZBqy7dJ2vKee6NjEqQ9zJdG226Os8TSNbx5OBlvBMOuAARVelgZ"))
  } else { # default assumption is that this is private testing
    webhook <- webhook %>% map(~inset(.,"https://discordapp.com/api/webhooks/326519810739666945/LVgkxHSSd_vs3d4To9chThqPyl-TLyN_smaKuc2WyBvwJtZ29AYXI9UbrW1hFGnh-ttk"))
  }
  last_seen[last_seen %>% strtoi(base=16) %>% equals(0) %>% not] %<>% map(~ strtoi(., base=16) %>% subtract(1) %>% as.hexmode() %>% format(width = 8))  
}

new_games <- map_df(
  names(league_search_strings), 
  ~filter_league_table(league_data[[.]],last_seen[[.]], .)
)

if(testing) {
  type = commandArgs(trailingOnly = T)[2]
  
  if (type == "update") {
    new_games %<>% group_by(league) %>% filter(ID == max(ID))
  }
}

##
#For new games, gather competition info/game stats
##
#Abbreviate team names
abbr <- function(name, clan = FALSE) {
  if(!clan) {
    name %>%
      str_replace_all("\\[(.*)\\]","") %>% # strip out 'clan' tags if not clan league
      str_replace_all("\\((.*)\\)", " ( \\1 )") %>% # Put spaces around brackets, so eg. USS Sulaco (REL Chapter) is abbreviated to US(RC)
      str_replace_all("([a-z_.-])([A-Z])", "\\1 \\2") %>%  # add a space before mid-word capitals and 'word separator' punctuation (_.-) followed by a capital
      str_replace_all("[&!,'\"*]",'') %>% # delete these characters
      abbreviate(1)
  } else {
    name %>%
      str_extract("\\[.*\\]")
  }
}


get_match_summary <- function(uuid, platform) {
  if(platform == "pc") return(get_match_summary_pc(uuid, "pc"))
  
  full_match_stats <- POST(modify_url("http://bb2leaguemanager.com/Leaderboard/get_matchdata.php", query = list("match_uuid" = str_c("1", platform_code[platform], uuid), "platform" = platform))) %>% content(as="parsed",type="application/json")
  
  stats_to_collect <- c(
    TD = "inflictedtouchdowns",
    BLK = "inflictedtackles", 
    AVBr = "inflictedinjuries", 
    KO = "inflictedko",
    CAS = "inflictedcasualties", 
    KILL = "inflicteddead", 
    SURF = "inflictedpushouts", 
    INT = "inflictedinterceptions", 
    PASS = "inflictedpasses", 
    CATCH = "inflictedcatches"
  )
  
  stats <- data_frame(
    stat = names(stats_to_collect),
    home = extract(full_match_stats$match$teams[[1]],stats_to_collect) %>% as.integer(),
    away = extract(full_match_stats$match$teams[[2]],stats_to_collect) %>% as.integer()
  )
  
  list(stats=stats,injuries=data_frame(), level_ups = data_frame())
}

get_match_summary_pc <- function(uuid, platform) {
  full_match_stats <- nufflytics::get_game_stats(uuid, platform)
  
  #homeNbSupporters == 0 is an admin concede. idMatchCompletionStatus != 0 is a regular concede
  if (full_match_stats$RowMatch$homeNbSupporters == 0 | full_match_stats$RowMatch$idMatchCompletionStatus != 0) return(NULL) 
  
  #Main stats table
  stats_to_collect <- c(
    BLK = "inflictedTackles", 
    AVBr = "inflictedInjuries", 
    KO = "inflictedKO",
    CAS = "inflictedCasualties", 
    KILL = "inflictedDead", 
    SURF = "inflictedPushOuts", 
    INT = "inflictedInterceptions", 
    PASS = "inflictedPasses", 
    CATCH = "inflictedCatches"
  )
  
  scores <- data_frame(stat="TD", home = score(full_match_stats, "home"), away = score(full_match_stats, "away"))
  
  stats <- data_frame(
    stat = names(stats_to_collect),
    home = pmap_dbl(list(list(full_match_stats), stats_to_collect , "home"), stat_total),
    away = pmap_dbl(list(list(full_match_stats), stats_to_collect , "away"), stat_total)
  )
  
  stats <- bind_rows(scores, stats)
  
  #Work out injuries / level ups
  players <- map(c("home","away"), ~player_data(full_match_stats, .)) %>% set_names(c("home","away"))
  
  injuries <- players %>% map(~filter(., ! injuries %in% c(NA, "BH")))
  
  level_ups <- players %>% map(~filter(., lvlup))
  
  list(stats = stats, injuries=injuries, level_ups=level_ups, TV = list(home = full_match_stats$RowMatch$homeValue, away = full_match_stats$RowMatch$awayValue))
}

format_embed_fields <- function(match_summary, hometeam, awayteam, clan = F) {
  #extract specific stat from stats table
  get_stat <- function(data, s, t) {
    data %>% filter(stat == s) %>% extract2(t)
  }
  
  #Combine pass/catch stats for if they are needed
  pass_catch = data_frame(stat = "P/C", 
                          home = paste0(get_stat(match_summary$stats, "PASS", "home"),"/",get_stat(match_summary$stats, "CATCH", "home")), 
                          away = paste0(get_stat(match_summary$stats, "PASS", "away"),"/",get_stat(match_summary$stats, "CATCH", "away")))
  
  #Filter out stats with no entries (keeping TDs always)
  filter = c(TRUE, rowSums(match_summary$stats[-1, 2:3]) > 0)
  match_summary$stats = match_summary$stats[filter,]
  
  #Replace the PASS & CATCH stats with P/C if present
  if (any(c("PASS","CATCH") %in% match_summary$stats$stat)) {
    match_summary$stats %<>% mutate_all(as.character) %>%
      filter(!stat %in% c("PASS","CATCH")) %>%
      bind_rows(pass_catch)
  }
  
  #Construct stats embed table
  #Have to pad away team abbreviation to avoid lines wrapping
  stat_block <- match_summary$stats %>%
    knitr::kable(row.names = F, col.names = c("", abbr(hometeam, clan), str_c(abbr(awayteam, clan),"   ")), format = "pandoc", align = "lrl") %>%
    extract(-2) %>% #remove the underlines
    paste0(collapse = "\n") %>%
    paste0("```R\n",.,"\n```") %>%
    as.character()
  
  #Construct injuries embed summary
  summarise_injury <- function(player) {
    if(nrow(player) == 0) return("")
    
    #Add permanent injuries to skills list if they didn't happen in this game 
    player %<>% mutate(
      old_perms = stringr::str_replace(perms, injuries, "")  %>% stringr::str_replace(", $", ""),
      skills_with_perms = ifelse(!is.na(perms) & old_perms != "", stringr::str_c(skills, ", *",old_perms,"*"), skills)
    )

    if(league_file == "REBBL" & !testing) {
      player$injuries %<>% str_replace_all("Dead","<:Dead:311936561069555712>")
    } else {
      player$injuries %<>% str_replace_all("Dead",":skull:")
    }
    
    inj_sum <- sprintf("__%s__ *(%s)* : **%s**\n%s  (%s SPP)", player$name, player$type, player$injuries, player$skills_with_perms, player$SPP+player$SPP_gain) %>%
      str_replace_all("\\*\\(Star Player\\)\\*", ":star:") %>% 
      paste0(collapse="\n\n")
    
    
    inj_sum
  }
  
  injury_block = ""
  if (match_summary$injuries %>% map_int(nrow) %>% sum %>% is_greater_than(0)) {
    inj_summary <- match_summary$injuries %>% map(summarise_injury)
    
    if(nchar(inj_summary$home)>0) {
      home_text <- sprintf("**%s**\n%s", hometeam, inj_summary$home)
      injury_block <- str_c(injury_block, home_text)
    }
    if(nchar(inj_summary$away)>0) {
      away_text <- sprintf("**%s**\n%s", awayteam, inj_summary$away)
      if(nchar(injury_block)>0) away_text <- str_c("\n\n",away_text)
      injury_block <- str_c(injury_block, away_text)
    }
  }
  
  
  #Construct level ups embed summary
  summarise_lvlups <- function(player) {
    if(nrow(player) == 0) return("")
    
    #Add permanent injuries to skills list 
    player %<>% mutate(
      skills_with_perms = ifelse(!is.na(perms), stringr::str_c(skills, ", *",perms,"*"), skills)
    )
    
    sprintf("__%s__ *(%s)* : **%i :arrow_right: %i SPP**\n%s", player$name, player$type, player$SPP, player$SPP+player$SPP_gain, player$skills_with_perms) %>% 
      str_replace_all("\\*\\(Star Player\\)\\*", ":star:") %>% 
      paste0(collapse="\n\n") 
  }
  
  lvlup_block = ""
  if (match_summary$level_ups %>% map_int(nrow) %>% sum %>% is_greater_than(0)) {
    lvl_summary <- match_summary$level_ups %>% map(summarise_lvlups)
    if(nchar(lvl_summary$home)>0) {
      home_text <- sprintf("**%s**\n%s", hometeam, lvl_summary$home)
      lvlup_block <- str_c(lvlup_block, home_text)
    }
    if(nchar(lvl_summary$away)>0) {
      away_text <- sprintf("**%s**\n%s", awayteam, lvl_summary$away)
      if(nchar(lvlup_block)>0) away_text <- str_c("\n\n",away_text)
      lvlup_block <- str_c(lvlup_block, away_text)
    }
  }
  
  fields <- list(list(name = "__**Game Stats**__", value = stat_block, inline = T))
  if(nchar(injury_block)>0) fields %<>% append(list(list(name = "__**Injury Report**__", value = injury_block, inline = T)))
  if(nchar(lvlup_block)>0) fields %<>% append(list(list(name = "__**Player Development**__", value = lvlup_block, inline = T)))
  
  fields
}

format_embed <- function(g, stats_summary, clan = F) {
  
  if (stats_summary$stats[[1,"home"]] > stats_summary$stats[[1,"away"]]) g[['h_team']] <- paste0("**", g[['h_team']], "**")
  if (stats_summary$stats[[1,"away"]] > stats_summary$stats[[1,"home"]]) g[['a_team']] <- paste0("**", g[['a_team']], "**")
  
  #Work out team race from image urls
  g[['h_race']] <- g[["h_img"]] %>% str_replace(".*Picto_","") %>% str_replace("\\.png","") %>% str_replace("(.)([A-Z])", "\\1 \\2")
  g[['a_race']] <- g[["a_img"]] %>% str_replace(".*Picto_","") %>% str_replace("\\.png","") %>% str_replace("(.)([A-Z])", "\\1 \\2")
  
  REBBL_races =  function(r) {switch(r,
    "Amazon" = "<:Zon:344918598286049281>",
    "Bretonnia" = "<:Bret:344918238976802826>",
    "Chaos" = "<:Chaos:344918252155305984>",
    "Chaos Dwarf" = "<:Chorf:344918276121427968>",
    "Dark Elf" = "<:Delf:344918286888337409>",
    "Dwarf" = "<:Dorf:344918297084559360>",
    "Elf" = "<:Pro:344918515817644033>",
    "Goblin" = "<:Gobbo:344918318685224975>",
    "Halfling" = "<:Fling:344918306236530698>",
    "High Elf" = "<:Helf:344918331930705921>",
    "Human" = "<:Human:344918344841035777>",
    "Khemri" = "<:Khemri:344918363438579714>",
    "Kislev" = "<:Kislev:344918385542299648>",
    "Lizardmen" = "<:Lizard:344918404471455744>",
    "Necromantic" = "<:Necro:344918417712611328>",
    "Norse" = "<:Norse:344918434867314691>",
    "Nurgle" = "<:Nurgle:344918450977898501>",
    "Ogre" = "<:Ogre:344918473832660992>",
    "Orc" = "<:Orc:344918500583800845>",
    "Skaven" = "<:Rats:344918530531131403>",
    "Undead" = "<:Undead:344918543974006788>",
    "Underworld" = "<:UW:344918559417171970>",
    "Vampire" = "<:Vamp:344918571853414400>",
    "Wood Elf" = "<:Welf:344918583236755485>",
    r
  )}
  
  if (league_file == "REBBL") {
    g[['h_race']] <- map_chr(g[['h_race']], REBBL_races)
    g[['a_race']] <- map_chr(g[['a_race']], REBBL_races)
  }
  
  author = ""
  if (exists("BB2LM")) {
    if(BB2LM[[g[['league']]]]) {
      author = list(name  ="More details at BB2LM", url = paste0("http://www.bb2leaguemanager.com/Leaderboard/match_detail.php?match_uuid=1",platform_code[platform[[g[['league']]]]], g[['uuid']]))
    }
  }
  
  
  format_description = function(g, stats, is_REBBL) {
    if (is_REBBL) {
      paste0(g[['h_race']]," ",g[['h_team']], " V ", g[['a_team']], " ", g[['a_race']], "\n",
             stats$TV$home," V ",stats$TV$away, " TV\n",
             "*", g[['comp']],"*")
    } else {
      paste0(g[['h_team']], " V ", g[['a_team']], "\n",
             "TV ",stats$TV$home," ",g[['h_race']], " V ", g[['a_race']], " ",stats$TV$away," TV", "\n",
             "*", g[['comp']],"*")
    }
  }
  
  embed = list(
    list(
      title = paste0(g[['h_coach']], " V ",g[['a_coach']]),
      description = format_description(g, stats_summary, is_REBBL = league_file == "REBBL"),
      url = paste0("http://www.mordrek.com/goblinSpy/web/game.html?mid=1",platform_code[platform[[g[['league']]]]], g[['uuid']]),
      #thumbnail = list(url = thumbnails[[g[['league']]]] ),
      author = author,
      #footer = list(icon_url= thumbnails[[g[['league']]]], text = "ico"),
      #image = list(url = thumbnails[[g[['league']]]], height = 50, width = 50),
      color = colours[[g[['league']]]],
      #timestamp = Sys.time() %>% lubridate::ymd_hms(tz = "Australia/Sydney"),
      fields = format_embed_fields(stats_summary, str_replace_all(g[['h_team']], "[*]",""), str_replace_all(g[['a_team']], "[*]",""), clan)
    )
  )
  
  embed
}
##
#Post messages to channel
##
post_message <- function(g) {
  league = g[['league']]
  is_clan <- league == "Clan"
  match_summary <- get_match_summary(g[['uuid']], platform[[league]])
  
  #summary will return null if game decided by admin result or concede. Don't post those.
  if (is.null(match_summary)) return(NULL)
  
  embed <- format_embed(g, match_summary, is_clan)
  
  message = ""
  
  #For clan league, add clan results to the posting as the 'content' field
  # if (is_clan) {
  #   clan_1 <- g[['h_team']] %>% str_extract("\\[.*\\]")
  #   clan_2 <- g[['a_team']] %>% str_extract("\\[.*\\]")
  #   
  #   clan_summary <- league_data$Clan %>% 
  #     filter(ID > 0, round == g[['round']]) %>% 
  #     mutate(h_clan = h_team %>% str_extract("\\[.*\\]"), a_clan = a_team %>% str_extract("\\[.*\\]"), winner = case_when(.$h_score>.$a_score ~ toupper(h_clan), .$a_score>.$h_score ~ toupper(a_clan), TRUE ~ "")) %>% 
  #     group_by(winner) %>% 
  #     summarise(n = n())
  #   
  #   clan_1_wins <- filter(clan_summary, winner == toupper(clan_1)) %>% extract2("n")
  #   if(length(clan_1_wins) == 0) clan_1_wins = 0
  #   
  #   clan_2_wins <- filter(clan_summary, winner == toupper(clan_2)) %>% extract2("n")
  #   if(length(clan_2_wins) == 0) clan_2_wins = 0
  #   
  #   clan_1_msg <- paste(clan_1,clan_1_wins)
  #   if (length(clan_1_wins)>0 & clan_1_wins >= 3) clan_1_msg <- paste0("**",clan_1_msg,"**")
  #   
  #   clan_2_msg <- paste(clan_2_wins,clan_2)
  #   if (length(clan_2_wins)>0 & clan_2_wins >= 3) clan_2_msg <- paste0("**",clan_2_msg,"**")
  #   
  #   message = paste0("__Clan Results__: ",clan_1_msg," V ",clan_2_msg)
  # }
  
  # #Notify @here and users who have requested it
  # mention = function(user_id) {paste0("<@",user_id,">")}
  # Check if clan summary has been posted first, and add a linebreak if so
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
  
  
  
  log_message(paste("Posting update for",embed[[1]]$title, "uuid:", g[['uuid']], "competition:", g[['comp']], "grouping:",league, "league:", league_file))
  #print(embed)
  response = POST(webhook[[league]], body = list(username = bot_usernames[[league]], avatar_url = bot_avatar[[league]], content = message, embeds = embed), encode = "json")
  
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

if (!testing | commandArgs(trailingOnly = T)[2] == "update" ) save(last_seen, file = paste0("./data/", league_file, "_last_seen.Rda"))
