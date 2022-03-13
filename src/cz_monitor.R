# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
#
# Script: WP-on-steroids 
# Version 1.0: CZ/LvdA, 2021-05-01
# Version 2.0: CZ/LvdA, 2022-01-22 
# - replaced DB-connect class (now MariaDB) 
# - added recovery from loss of db-connection
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  
library(digest)
library(readr)
library(fs)
library(futile.logger)
library(yaml)
library(lubridate)
library(magrittr)
library(stringr)
library(keyring)
library(tibble)
library(dplyr)
library(DBI)

# init functions ----
source("src/wpos_utilities.R", encoding = "UTF-8")

# init cfg ----
wpos_config <- read_yaml("config.yaml")

# init logging ----
path.wpos_log <- paste(wpos_config$log_home, "wpos.log", sep = "/")
lg_ini <- flog.appender(appender.file(path.wpos_log), "wpos")
flog.info("= = = = = WP-monitor START = = = = =", name = "wpos")

# init file to watch ----
wpos_today <- now(tzone = "Europe/Amsterdam")

# change hour to start-of-broadcast
hour(wpos_today) <- wpos_config$broadcast_start_hour

# TEST
# wpos_today <- ymd_hms("2019-05-20 05:00:00", tz = "Europe/Amsterdam")
# TEST

# build doc-name that will hold the playlist tracks
pl_name <- format_ISO8601(wpos_today, precision = "ymdh") %>%
  str_remove_all("-") %>%
  str_replace("T", "_") %>%
  paste0(".txt")

path.playlist <- paste(wpos_config$dir_to_watch, pl_name, sep = "/")

if (!file_exists(path = path.playlist)) {
  file_create(path = path.playlist)
}

# init file hash ----
playlist <- read_lines(file = path.playlist)
playlist.md5 <- digest(object = playlist)
playlist.md5_sav <- playlist.md5

# init polling interval ----
POLL_INTERVAL <- wpos_config$polling_interval_in_secs

# init stop-time ----
wpos_stop <- wpos_today
hour(wpos_stop) <- wpos_config$mon_stop_h
minute(wpos_stop) <- wpos_config$mon_stop_m
second(wpos_stop) <- 0L

# TEST
# wpos_stop <- now(tzone = "Europe/Amsterdam") + minutes(5)
# TEST

# Connect to database ----
wp_conn <- get_wp_conn(wpos_config$wp_env)

# which webpage to update?
sql_post_key <- str_remove(pl_name, "\\.txt")

sel_stmt01 <-
  sprintf("SELECT cz_id FROM salsa_plws_gidsweek where pgmStart = '%s';",
          sql_post_key)

dsSql01 <- dbGetQuery(wp_conn, sel_stmt01)

# # TEST
# # make sure to have 1 row in dsSql01, so there is a row to replace,
# # so use a date from this week's rows in salsa_plws_gidsweek
# sel_stmt01 <- "SELECT cz_id FROM salsa_plws_gidsweek where pgmStart = '20220120_18';"
# dsSql01 <- dbGetQuery(wp_conn, sel_stmt01)
# 
# # now you can safely replace the id, 
# # set it to the one of Framework 2018-02-26 05:00:00
# dsSql01$cz_id <- 432782L
# # TEST

# start monitoring the .txt-file ----
flog.info("watching %s until %s, id = %s",
          path.playlist,
          format_ISO8601(wpos_stop, precision = "ymdhm"),
          as.character(dsSql01$cz_id),
          name = "wpos")

# let the show begin ----
while (TRUE) {
  
  # + keep running to end of show ----
  if (now(tzone = "Europe/Amsterdam") > wpos_stop) {
    flog.info("game over ...", name = "wpos")
    break
  }
  
  # + get current playlist content ----
  playlist <- read_lines(file = path.playlist)
  playlist.md5 <- digest(object = playlist)
  
  # + snooze if no change ----
  if (file_size(path = path.playlist) == 0 ||
      playlist.md5 == playlist.md5_sav) {
    # + . wait a few moments ----
    Sys.sleep(time = POLL_INTERVAL)
    next
  }
  
  # file changed > save MD5
  playlist.md5_sav <- playlist.md5
  
  # TEST - simulate loss of connection
  # dbDisconnect(wp_conn)
  # TEST
  
  # make sure db-connection is OK
  if (!cur_conn_sts_valid()) {
    flog.info("lost connection to WordPress-DB on %s; retry...",
              wpos_config$wp_env,
              name = "wpos")
    wp_conn <- get_wp_conn(wpos_config$wp_env)
  }
  
  if (is.null(wp_conn)) {
    break
  }
  
  update_wp()
}

# disconnect from DB
if (!is.null(wp_conn)) {
  dbDisconnect(wp_conn)
  flog.info("disconnected from WordPress-DB", name = "wpos")
}

# monitoring stopped----
flog.info("= = = = = WP-monitor STOP  = = = = =", name = "wpos")
