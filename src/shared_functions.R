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

get_ns_conn <- function(db_env) {
  
  # fa <- flog.appender(appender.file("c:/cz_salsa/Logs/nipperstudio_backend.log"), name = "nsbe_log")
  
  if (db_env == "DEV") {
    db_env <- "wpdev_mariadb"
  } else if (db_env == "PRD") {
    db_env <- "wpprd_mariadb"
  } 
  
  result <- tryCatch( {
    dbConnect(odbc::odbc(), db_env, timeout = 10, encoding = "CP850")
  },
  error = function(cond) {
    flog.error(sprintf("Verbinding mislukt: %s", cond$message), name = "nsbe_log")
    return("Verbinding is mislukt")
  }
  )
  return(result)
}

random_select <- function(L1, n1) {
  
  m1 <- length(L1)
  
  if (n1 > m1) {
    # Select all items from L1 and reorder them randomly
    selected_items <- sample(L1)
    
    # add the remaining elements, making sure they don't match the previous 3
    for (i1 in (m1 + 1):n1) {
      last_three_items <- selected_items[(i1-3):(i1-1)]
      available_items <- setdiff(L1, last_three_items)
      # n_available_items >= 2 because m1 >= 5
      selected_items[i1] <- sample(available_items, 1)
    }
    
  } else {
    selected_items <- vector("integer", n1)
    selected_items <- sample(L1, size = n1)
  }
  
  return(selected_items)
}

get_bumper_audio <- function(pm_playlist, pm_blok, pm_stack) {
  
  if (pm_blok == "RL_BLK_1") {
    bum_aan_df <- bum_aan %>% filter(pl_name == pm_playlist)
    bum_obs <- bum_aan_df$ns_dir[1]
  } else if (pm_blok == "SLOT") {
    bum_af_df <- bum_af %>% filter(pl_name == pm_playlist)
    bum_obs <- bum_af_df$ns_dir[1]
  } else {
    bum_over_df <- bum_over %>% filter(pl_name == pm_playlist)
    obs <- pm_stack$pop()
    bum_obs <- bum_over_df$ns_dir[obs]
  }
  
  fnam_raw <- dir_ls(path = bum_obs, type = "file")
  fnam <- str_replace(string = fnam_raw, pattern = "//uitzendmac-2", replacement = "file:///Volumes")
  
  return(fnam)
}

create_form <- function(arg_playlist) {
  
  order_form_name <- paste0("g:/salsa/muziekweb_aanvragen/", arg_playlist, ".txt")
  
  if (file_exists(order_form_name)) {
    file_delete(order_form_name)
  }
  
  cur_audio_ids <- ns_tracks %>% filter(pl_name == arg_playlist) %>% select(recording_no)
  
  cur_audio_ids_sep <- cur_audio_ids %>% 
    separate(recording_no, into = paste0("muw_track", 1:150), sep = ", ", fill = "right") %>% 
    pivot_longer(names_to = "track_key", values_to = "track_value", cols = starts_with("muw_")) %>% 
    filter(!is.na(track_value)) %>% select(track_value) %>% 
    separate(track_value, into = c("album_id", "track_id"), sep = "-") %>% 
    mutate(track_id = as.character(as.integer(track_id))) %>% 
    pivot_longer(names_to = "df_name", values_to = "order_line", cols = c("album_id", "track_id")) %>% 
    select(order_line)
  
    write_lines(x = cur_audio_ids_sep$order_line, file = order_form_name, append = F)
    flog.info(sprintf("Audio-bestelformulier gereed: %s", arg_playlist), name = "nsbe_log")
}

gd_wp_gidsinfo <- function(arg_sheet) {
  result <- tryCatch( {
    gds <- read_sheet(ss = "16DrvLEXi3mEa9AbSw28YBkYpCvyO1wXwBwaNi7HpBkA", sheet = arg_sheet)
  },
  error = function(cond) {
    flog.error(sprintf("Verbinding met GoogleDrive (WP-gidsinfo) mislukt: %s", cond$message), name = "nsbe_log")
    return("GD-error")
  }
  )
  return(result)
}

get_replay_playlist <- function(arg_pl) {
  
  pl_home <- paste0(home_prop("home_radiologik_win"), "Programs/")
    
  result <- arg_pl
  
  # preferably the one that is 6 months old 
  pl_date <- playlist2postdate(arg_pl)
  suppressMessages(stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a"))
  pl_his <- paste0(stamped_format(pl_date - days(168L)), str_sub(arg_pl, 12))
  pl_candidate <- dir_ls(pl_home, regexp = str_sub(pl_his, 1, 13))
  
  if (length(pl_candidate) == 1) {
    result <- path_file(pl_candidate) %>% str_replace("\\.rlprg", "")
  }
  
  return(result)
}
