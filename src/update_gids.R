# source("src/shared_functions.R", encoding = "UTF-8")

for (seg2 in 1:1) {
  # + connect to DB ----
  # ns_con <- dbConnect(odbc::odbc(), "wpdev_mariadb", timeout = 10, encoding = "CP850")
  # ns_con <- get_ns_conn("DEV")
  # 
  # stopifnot("WP-database is niet beschikbaar, zie C:/cz_salsa/Logs/nipperstudio_backend.log" = typeof(ns_con) == "S4")
  # flog.info("Verbonden!", name = "nsbe_log")
  
  # gidsgegevens klaarzetten ------------------------------------------------
  ns_tracks <- playlists.6 %>% filter(pl_name %in% bum.3$pl_name)
  
  dummy_bumpers <- ns_tracks %>% 
    filter(block_order == 1 & track_order == 1) %>% 
    mutate(block_order = 0, length = 30L)
  
  # draaiboeken gids
  drb_gids <- rbind(dummy_bumpers, ns_tracks) %>% 
    # filter(track_order == 1) %>% 
    arrange(pl_name, block_order, track_order) %>% 
    group_by(pl_name) %>%
    mutate(
      cum_tijd = np_sec2hms(cumsum(as.duration(length))),
      # cum_tijd_secs = seconds(cumsum(as.duration(lengte)) %% 60),
      # cum_tijd_secs2min = ifelse(cum_tijd_secs > 30, 1, 0),
      cum_tijd = lag(cum_tijd, n = 1)) %>% 
    mutate(
      # cum_tijd_secs = lag(cum_tijd_secs, n = 1),
      # cum_tijd_secs2min = lag(cum_tijd_secs2min, n = 1)
      wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = pl_name)) %>% 
    filter(block_order != 0) %>% 
    select(-album, -recording_no, -c(pl_id:user_id), -c(block_order:track_id))
  
  # update gids ----
  
  for (cur_pl in unique(ns_tracks$pl_name)) {

    sql_post_date <- playlist2postdate(cur_pl) %>% as.character
    
    ### TEST
    # sql_post_date <- "2022-03-08 19:00:00"
    ### TEST
    
    drb_gids_pl <- drb_gids %>% filter(pl_name == cur_pl)
    
    koptekst <- drb_gids_pl %>% select(pl_name, componist) %>% distinct %>% 
      group_by(pl_name) %>% summarise(werken_van = paste(componist, collapse = ", "))
    
    # sql_gidstekst <- sprintf("Werken van %s.\n<!--more-->\n\n", koptekst$werken_van)
    sql_gidstekst <- "$#HEADER#$\n<!--more-->\n\n"
    
    regel <- '<style>td {padding: 6px; text-align: left;}</style>\n<table style="width: 100%;"><tbody>'
    sql_gidstekst <- paste0(sql_gidstekst, regel, "\n")
    
    for (q1 in 1:nrow(drb_gids_pl)) {
      regel <-
        sprintf(
          '<tr>\n<td>[track tijd="%s" text="%s %s"]\n<span>',
          drb_gids_pl$cum_tijd[q1],
          drb_gids_pl$wallclock[q1],
          drb_gids_pl$titel[q1] %>% 
            str_replace_all(pattern = '"',  "'") %>% 
            str_replace_all(pattern = "\\x5B", replacement = "(") %>% 
            str_replace_all(pattern = "\\x5D", replacement = ")") 
        )
      sql_gidstekst <- paste0(sql_gidstekst, regel)
      
      regel <- drb_gids_pl$componist[q1]
      sql_gidstekst <- paste0(sql_gidstekst, regel, "\n")
      
      regel <- sprintf('%s</span></td>\n</tr>',
                       drb_gids_pl$uitvoerenden[q1])
      sql_gidstekst <- paste0(sql_gidstekst, regel, "\n")
    }
    
    regel <- '</tbody>\n</table>'
    sql_gidstekst <- paste0(sql_gidstekst, regel, "\n") %>% str_replace_all("[']", "&#39;")
    
    upd_stmt01 <- sprintf(
      "select id from wp_posts where post_date = '%s' and post_type = 'programma' order by 1;",
      sql_post_date
    )
    dsSql01 <- dbGetQuery(ns_con, upd_stmt01)
    
    for (u1 in 1:nrow(dsSql01)) {
      # upd_stmt02 <- sprintf(
      #   "update wp_posts set post_content = convert(cast('%s' as binary) using utf8mb4) where id = %s;",
      #   sql_gidstekst,
      #   as.character(dsSql01$id[u1])
      # )
      sql_gidstekst <- sql_gidstekst %>% if_else(u1 == 1, 
                                                 str_replace("$#HEADER#$", 
                                                             "Een fijne mix met gestoofde ingrediënten."),
                                                 str_replace("$#HEADER#$", 
                                                             "A vegetable stew with different ingredients."))
      upd_stmt02 <- sprintf(
        "update wp_posts set post_content = '%s' where id = %s;",
        sql_gidstekst,
        as.character(dsSql01$id[u1])
      )
      
      dbExecute(ns_con, upd_stmt02)
      
      upd_stmt03 <- sprintf(
        "insert into wp_postmeta (post_id, meta_key, meta_value) values(%s, '_thumbnail_id', %s);",
        as.character(dsSql01$id[u1]),
        as.character(463848L)
      )
      dbExecute(ns_con, upd_stmt03)
    }
    
    flog.info("Gids bijgewerkt: %s", cur_pl, name = "nsbe_log")
  
    #....+ replace replay-posts with recycled OE's ---- 
    for (seg_oe in 1:1) {
      ### TEST
      # cur_pl <- "20220328_ma07.180_ochtendeditie"
      ### TEST
      
      if (!str_detect(string = cur_pl, pattern = "_ochtendeditie")){
        break
      }
      
      #....+ . get recycle-OE's date ----
      # details are on GD: kringloopherhalingen ochtendeditie
      cur_pl_date <- playlist2postdate(cur_pl)
      
      replay_date <- cur_pl_date + days(7)
      
      oe_offset <- 
        case_when(cur_pl_date >= ymd_hms("2020-06-22 07:00:00")              ~ 175L,
                  str_detect(string = cur_pl, pattern = "_(ma|di|wo|do)\\d") ~ 175L, 
                  TRUE                                                       ~ 182L)
      
      oe_date <- replay_date - days(oe_offset)
      
      tmp_log <- sprintf("Kringloopherhaling: op %s klinkt die van %s", 
                         replay_date, oe_date)
      flog.info(tmp_log, name = "nsbe_log")
      
      #....+ . get replay_date's post-id ----
      upd_stmt06 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        replay_date
      )
      
      replay_pgm_id <- dbGetQuery(ns_con, upd_stmt06)
      
      #....+ . get recycle-OE's post-id ----
      upd_stmt04 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        oe_date
      )
      
      oe_pgm_id <- dbGetQuery(ns_con, upd_stmt04) 
      
      #....+ . update post-id's NL/EN ----
      for (r1 in 1:2) {
        oe_pgm_id_chr <- as.character(oe_pgm_id$min_id + r1 - 1L)
        replay_pgm_id_chr <- as.character(replay_pgm_id$min_id + r1 - 1L)
        upd_stmt05 <-
          sprintf(
            "update wp_postmeta set meta_value = %s where post_id = %s and meta_key = 'pr_metadata_orig';",
            oe_pgm_id_chr,
            replay_pgm_id_chr
          )
        
        flog.info("SQL: %s", upd_stmt05, name = "nsbe_log")
        
        dbExecute(ns_con, upd_stmt05)
      }
      
      suppressMessages(stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a"))
      dummy_pl <- paste0(stamped_format(cur_pl_date + days(7L)),
                         "07-180_ochtendeditie")
      flog.info("Gids bijgewerkt: %s", dummy_pl, name = "nsbe_log")
    }
  }
  # on.exit(dbDisconnect(ns_con))
}
