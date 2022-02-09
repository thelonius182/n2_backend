playlist2postdate <- function(playlist) {
  tmp_date <- playlist %>% str_sub(0, 8)
  tmp_time <- playlist %>% str_sub(11, 13) %>% paste0(":00:00")
  result <- paste0(tmp_date, " ", tmp_time) %>% ymd_hms
}

get_wallclock <- function(pm_cum_tijd, pm_playlist) {
  cum_tijd_ts <- paste0("2019-01-01 ", pm_cum_tijd) %>% ymd_hms
  start_clock <- pm_playlist %>% str_sub(12, 13) %>% as.integer
  wallclcok_ts <- cum_tijd_ts + hours(start_clock)
  wallclock_ts_rounded <- wallclcok_ts %>% round_date("minute")
  wallclock <- wallclock_ts_rounded %>% as.character %>% str_sub(12, 16)
}

get_wp_conn <- function() {
  db_type <- "prd"
  if (config$host == "logmac") {
    db_type <- "dev"
  }
  db_host <- key_get(service = paste0("sql-wp", db_type, "_host"))
  db_user <- key_get(service = paste0("sql-wp", db_type, "_user"))
  db_password <- key_get(service = paste0("sql-wp", db_type, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", db_type, "_db"))
  db_port <- 3306
  db_table <- "cz.wp_posts"
  flog.appender(appender.file("/Users/nipper/Logs/nipper.log"), name = "nipperlog")
  
  result <- tryCatch( {
      grh_conn <- dbConnect(drv = MySQL(), user = db_user, password = db_password,
                            dbname = db_name, host = db_host, port = db_port)
    },
    error = function(cond) {
      flog.error("Wordpress database onbereikbaar (dev: check PuTTY)", name = "nipperlog")
      return("connection-error")
    }
  )
  return(result)
}

for (seg2 in 1:1) {
  # Connect to database -----------------------------------------------------
  wp_conn <- get_wp_conn()
  
  # connection type S4 indicates a valid connection; other types indicate failure
  if (typeof(wp_conn) != "S4") { 
    break
  }
  
  # gidsgegevens klaarzetten ------------------------------------------------
  dummy_vt <- pl_werken %>% filter(vt_blok_nr == 1) %>% 
    mutate(vt_blok_nr = 0, lengte = hms::as.hms(40))
  
  drb_gids <- rbind(pl_werken, dummy_vt) %>%
    arrange(playlist, vt_blok_letter, vt_blok_nr) %>% group_by(playlist) %>%
    mutate(
      cum_tijd = np_sec2hms(cumsum(as.duration(lengte))),
      # cum_tijd_secs = seconds(cumsum(as.duration(lengte)) %% 60),
      # cum_tijd_secs2min = ifelse(cum_tijd_secs > 30, 1, 0),
      cum_tijd = lag(cum_tijd, n = 1),
      # cum_tijd_secs = lag(cum_tijd_secs, n = 1),
      # cum_tijd_secs2min = lag(cum_tijd_secs2min, n = 1)
      wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = playlist)
    ) %>% filter(vt_blok_nr != 0) %>% 
    select(-bezetting, -album, -opnameNr, -starts_with("vt_"))
  
  # update gids -------------------------------------------------------------
  
  for (cur_pl in pl_nieuw$playlist) {
    sql_post_date <- playlist2postdate(cur_pl) %>% as.character
    drb_gids_pl <- drb_gids %>% filter(playlist == cur_pl)
    
    koptekst <- drb_gids_pl %>% select(playlist, componist_lbl) %>% distinct %>% 
      group_by(playlist) %>% summarise(werken_van = paste(componist_lbl, collapse = ", "))
    regel <- sprintf('Werken van %s.\n<!--more-->\n', koptekst$werken_van)
    sql_gidstekst <- paste0(dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    regel <- '<style>td {padding: 6px; text-align: left;}</style>\n<table style="width: 100%;"><tbody>'
    sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    for (q1 in 1:nrow(drb_gids_pl)) {
      regel <-
        sprintf(
          '<tr>\n<td>[track tijd="%s" text="%s %s"]\n<span>',
          drb_gids_pl$cum_tijd[q1],
          drb_gids_pl$wallclock[q1],
          drb_gids_pl$titel[q1]
        )
      sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)))
      
      regel <- drb_gids_pl$componist_lbl[q1]
      sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
      
      regel <- sprintf('%s</span></td>\n</tr>',
                       drb_gids_pl$uitvoerenden[q1])
      sql_gidstekst <-
        paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    }
    
    regel <- '</tbody>\n</table>'
    sql_gidstekst <- paste0(sql_gidstekst, dbEscapeStrings(wp_conn, enc2native(regel)), "\n")
    
    upd_stmt01 <- sprintf(
      "select id from wp_posts where post_date = '%s' and post_type = 'programma';",
      sql_post_date
    )
    dsSql01 <- dbGetQuery(wp_conn, upd_stmt01)
    
    for (u1 in 1:nrow(dsSql01)) {
      upd_stmt02 <- sprintf(
        "update wp_posts set post_content = '%s' where id = %i;",
        sql_gidstekst,
        dsSql01$id[u1]
      )
      dbGetQuery(wp_conn, upd_stmt02)
      
      upd_stmt03 <- sprintf(
        "insert into wp_postmeta (post_id, meta_key, meta_value) values(%i, '_thumbnail_id', %i);",
        dsSql01$id[u1],
        463848
      )
      dbGetQuery(wp_conn, upd_stmt03)
    }
    
    flog.info("Gids bijgewerkt: %s", cur_pl, name = "nipperlog")
  }
  
  on.exit(dbDisconnect(wp_conn))
}
# a_post_date <- "2019-01-01 17:00:00"
  # 
  # gidstekst <- paste("Werken van Haydn, Grieg en Goebajdoelina<!--more--><br>\n",
  #                    "<em>[track tijd=\"01:49\" text=\"01:49 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "German: Falsches Üben von Xylophonmusik quält jeden größeren Zwerg.",
  #                    "Koninklijke Academie voor Muziek te Kopenhagen. \nHij is wat je noemt “een groot pleitbezorger van hedendaagse muziek”.<br>",
  #                    "<em>[track tijd=\"23:46\" text=\"23:46 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "Finnish: (5) Törkylempijävongahdus\nPolish: Pchnąć w tę łódź jeża lub osiem skrzyń fig.<br>",
  #                    "<em>[track tijd=\"45:19\" text=\"45:19 - Sofia Goebajdoelina: Hell und Dunkel\"]</em>",
  #                    "Finnish: (5) Törkylempijävongahdus\nPolish: Pchnąć w tę łódź jeża lub osiem skrzyń fig.\n",
  #                    "Esperanto: Eĥoŝanĝo ĉiuĵaŭde.\n",
  #                    "Euro Symbol: €."
  # ) 
  
  # mydb <-  dbConnect(drv = MySQL(), user = db_user, password = db_password,
  # dbname = db_name, host = db_host, port = db_port)
# tmp <- sprintf("SELECT * FROM emp WHERE lname = %s", "O'Reilly")
# stmt <- "update wp_posts set post_content = 'Hello CZ-World trial!' where id = 405929;"
# stmt <- sprintf("update wp_posts set post_content = '%s' where id = %s", content_upd, 405929)

# stmt_esc <- dbEscapeStrings(mydb, stmt)
# rs <- dbSendQuery(mydb, stmt)
# 
# 
# df <-  fetch(rs, n = -1)
# 
# pd1 <- playlist2postdate(c("20181231_ma14.060_liederen", "20180213_ma14.060_liederen")) %>% as.character
# = = = = = = = = = = = = = = = = = = = = = = = = wpProd = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 1. Library
# library(RMySQL)

# 2. Settings
# db_user <- key_get(service = "sql-wpprd_user")
# db_password <- key_get(service = "sql-wpprd_pwd")
# db_name <- key_get(service = "sql-wpprd_db")
# db_host <-  key_get(service = "sql-wpprd_host")
# db_port <- 3306
# db_table <- "cz.wp_posts"

# 3. Read data from db
# mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
# dbname = db_name, host = db_host, port = db_port)
# s2 <- "SELECT * FROM wp_posts where lower(post_title) = 'ochtendeditie' and post_type = 'programma';"
# rs2 <- dbSendQuery(mydb, s2)
# df2 <-  fetch(rs2, n = -1)
# on.exit(dbDisconnect(mydb))
