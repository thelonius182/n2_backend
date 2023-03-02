playlist2postdate <- function(playlist) {
  # TEST! # playlist <- "20200106_ma07.180_ochtendeditie"
  tmp_date <- playlist %>% str_sub(0, 8)
  tmp_time <- playlist %>% str_sub(12, 13) %>% paste0(":00:00")
  result <- paste0(tmp_date, " ", tmp_time) %>% ymd_hms(tz = "Europe/Amsterdam")
}

np_sec2hms <- function(duur_sec) {
  # flog.info("@np_sec2hms in : %s", duur_sec, name = "nipperlog")
  result <- paste0("00:00:", duur_sec) %>% 
    hms(roll = TRUE) 
  result <- sprintf("%02d:%02d:%02d", result@hour, result@minute, result@.Data)
  # flog.info("@np_sec2hms out: %s", result, name = "nipperlog")
}

get_wallclock <- function(pm_cum_tijd, pm_playlist) {
  # flog.info("@wallclck in : %s, %s", pm_cum_tijd, pm_playlist, name = "nipperlog")
  pm_cum_tijd <- if_else(is.na(pm_cum_tijd), paste0(str_sub(pm_playlist, 12, 13), ":00:00"), pm_cum_tijd)
  cum_tijd_ts <- paste0("2019-01-01 ", pm_cum_tijd) %>% ymd_hms
  start_clock <- pm_playlist %>% str_sub(12, 13) %>% as.integer
  wallclcok_ts <- cum_tijd_ts + hours(start_clock)
  wallclock_ts_rounded <- wallclcok_ts %>% round_date("minute")
  wallclock <- wallclock_ts_rounded %>% as.character %>% str_sub(12, 16)
  # flog.info("@wallclck out: %s", wallclock, name = "nipperlog")
}

get_wp_conn <- function() {
  
  db_env <- "wpprd_mariadb"

  ### TEST
  # db_env <- "wpdev_mariadb"
  ### TEST
  
  flog.appender(appender.file("C:/Users/gergiev/Logs/nipper_uzm_two.log"), name = "nipperlog")
  
  result <- tryCatch( {
    grh_conn <- dbConnect(odbc::odbc(), db_env, timeout = 10)
  },
  error = function(cond) {
    flog.error("Wordpress database onbereikbaar (dev: check PuTTY)", name = "nipperlog")
    return("connection-error")
  }
  )
  return(result)
}
