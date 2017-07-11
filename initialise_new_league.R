suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

new_league <- function(league_abbreviation, file) {
  if(file.exists(paste0("data/",league_abbreviation,"_parameters.Rda"))) stop("League ", league_abbreviation, " already exists. Please delete and retry")
  
  info <- read_csv(file, col_types = "ccccnccc")
  
  map_info <- function(values,keys) {
    values %>% as.list %>% set_names(keys)
  }
  
  league_search_strings <- with(info, map_info(league_search_strings, id))
  webhook <- with(info, map_info(webhook, id))
  thumbnails <- with(info, map_info(thumbnails, id))
  colours <- with(info, map_info(colours, id))
  bot_usernames <- with(info, map_info(bot_usernames, id))
  bot_avatar <- with(info, map_info(bot_avatar, id))
  platform <- with(info, map_info(platform, id))

  save(list = c("league_search_strings","webhook","thumbnails","colours", "bot_usernames", "bot_avatar", "platform"), file = paste0("data/",league_abbreviation,"_parameters.Rda"))
  cat(paste0("Saving league parameters to 'data/",league_abbreviation,"_parameters.Rda'\n"))
  
  last_seen <- list()
  last_seen[info$id] <- "00001000"
  
  save(last_seen, file = paste0("data/",league_abbreviation,"_last_seen.Rda") )
  
  cat(paste0("Initialising last seen game to 'data/",league_abbreviation,"_last_seen.Rda' \n Recommend running 'Rscript bb2_box_score.R ",league_abbreviation," update' before using\n"))
}

sys_arg <- commandArgs(trailingOnly = TRUE)

new_league(sys_arg[1], sys_arg[2])
