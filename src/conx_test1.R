pacman::p_load(readr, futile.logger, DBI, yaml, fs, magrittr, lubridate, stringr)

source(file = "src/conx_test2.R", encoding = "UTF-8")

ns_con <- get_ns_conn("DEV")

try(if (typeof(ns_con) != "S4")
  stop("geen verbinding met WP-database")
)

sqlstmt <- "select * from wp_nipper_main_playlists;"
wp_main_pls <- dbGetQuery(ns_con, sqlstmt)

sqlstmt <- "select * from wp_nipper_tracklists;"
df_tracks <- tracks(sqlstmt) %>% select(recording_no)
