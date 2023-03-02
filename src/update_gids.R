source("src/shared_functions.R", encoding = "UTF-8")

for (seg2 in 1:1) {
  # Connect to database -----------------------------------------------------
  wp_conn <- get_wp_conn()
  
  # connection type S4 indicates a valid connection; other types indicate failure
  if (typeof(wp_conn) != "S4") { 
    flog.error("Nipper: invalid db-connection (non-S4)", name = "nipperlog")
    break
  }
  
  # gidsgegevens klaarzetten ------------------------------------------------
  dummy_vt <- pl_werken %>% filter(vt_blok_nr == 1) %>% 
    mutate(vt_blok_nr = 0, lengte_sec = as_hms(40))
  
  drb_gids <- rbind(pl_werken, dummy_vt) %>%
    arrange(playlist, vt_blok_letter, vt_blok_nr) %>% group_by(playlist) %>%
    mutate(
      cum_tijd = np_sec2hms(cumsum(as.duration(lengte_sec))),
      # cum_tijd_secs = seconds(cumsum(as.duration(lengte)) %% 60),
      # cum_tijd_secs2min = ifelse(cum_tijd_secs > 30, 1, 0),
      cum_tijd = lag(cum_tijd, n = 1)) %>% 
    mutate(
      # cum_tijd_secs = lag(cum_tijd_secs, n = 1),
      # cum_tijd_secs2min = lag(cum_tijd_secs2min, n = 1)
      wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = playlist)) %>% 
    filter(vt_blok_nr != 0) %>% 
    select(-album, -opnameNr, -starts_with("vt_"))
  
  # update gids -------------------------------------------------------------
  
  for (cur_pl in pl_nieuw$playlist) {
    ### TEST
    # cur_pl = "20220328_ma07.180_ochtendeditie"
    ### TEST
    
    sql_post_date <- playlist2postdate(cur_pl) %>% as.character
    
    ### TEST
    # sql_post_date <- "2022-03-08 19:00:00"
    ### TEST
    
    drb_gids_pl <- drb_gids %>% filter(playlist == cur_pl)
    
    koptekst <- drb_gids_pl %>% select(playlist, componist) %>% distinct %>% 
      group_by(playlist) %>% summarise(werken_van = paste(componist, collapse = ", "))
    sql_gidstekst <- sprintf("Werken van %s.\n<!--more-->\n\n", koptekst$werken_van)
    
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
      "select id from wp_posts where post_date = '%s' and post_type = 'programma';",
      sql_post_date
    )
    dsSql01 <- dbGetQuery(wp_conn, upd_stmt01)
    
    for (u1 in 1:nrow(dsSql01)) {
      upd_stmt02 <- sprintf(
        "update wp_posts set post_content = convert(cast('%s' as binary) using utf8mb4) where id = %s;",
        sql_gidstekst,
        as.character(dsSql01$id[u1])
      )
      
      dbExecute(wp_conn, upd_stmt02)
      
      upd_stmt03 <- sprintf(
        "insert into wp_postmeta (post_id, meta_key, meta_value) values(%s, '_thumbnail_id', %s);",
        as.character(dsSql01$id[u1]),
        as.character(463848L)
      )
      dbExecute(wp_conn, upd_stmt03)
    }
    
    flog.info("Gids bijgewerkt: %s", cur_pl, name = "nipperlog")
  
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
      flog.info(tmp_log, name = "nipperlog")
      
      #....+ . get replay_date's post-id ----
      upd_stmt06 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        replay_date
      )
      
      replay_pgm_id <- dbGetQuery(wp_conn, upd_stmt06)
      
      #....+ . get recycle-OE's post-id ----
      upd_stmt04 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        oe_date
      )
      
      oe_pgm_id <- dbGetQuery(wp_conn, upd_stmt04) 
      
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
        
        flog.info("SQL: %s", upd_stmt05, name = "nipperlog")
        
        dbExecute(wp_conn, upd_stmt05)
      }
      
      suppressMessages(stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a"))
      dummy_pl <- paste0(stamped_format(cur_pl_date + days(7L)),
                         "07-180_ochtendeditie")
      flog.info("Gids bijgewerkt: %s", dummy_pl, name = "nipperlog")
    }
  }
  on.exit(dbDisconnect(wp_conn))
}
