pacman::p_load(knitr, rmarkdown, RCurl, readr, futile.logger, DBI, officer, httr,
               xml2, tidyverse, keyring, googlesheets4, yaml, fs, magrittr, hms,
               lubridate, zip, stringr)

source("src/shared_functions.R", encoding = "UTF-8") 

postids_to_reset <-
  read_delim(
    "C:/cz_salsa/postids_to_reset.txt",
    delim = "\t",
    escape_double = FALSE,
    col_types = cols(id = col_character(),
                     post_date = col_character()),
    trim_ws = TRUE
  ) %>% 
  mutate(pl_ts = ymd_hms(post_date)) %>% 
  select(-post_date)

# + connect to DB ----
ns_con <- get_ns_conn("DEV")

stopifnot("WP-database is niet beschikbaar, zie C:/cz_salsa/Logs/nipperstudio_backend.log" = typeof(ns_con) == "S4")

playlists_db <- dbGetQuery(conn = ns_con, 
                           statement = "select * from wp_nipper_main_playlists") %>% 
  filter(deleted == 0)

dbDisconnect(ns_con)

playlists_his_replace <- read_rds("C:/cz_salsa/cz_exchange/Copy (1) nipper_main_playlists.RDS") %>% 
  mutate(pl_ts = ymd_h(paste0(program_date, " ", time_start))) %>% 
  left_join(postids_to_reset) %>% 
  filter(!is.na(id)) %>% 
  mutate(post_id = id) %>% 
  select(-pl_ts, -id)

write_rds(playlists_his_replace, paste0(rds_home, "nipper_main_playlists.RDS"))
