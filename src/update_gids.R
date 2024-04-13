
# BUM klaarzetten ----
if (exists("bum.3") && nrow(bum.3) > 0) {
    
    ns_tracks <- playlists.6 %>% filter(pl_name %in% bum.3$pl_name)
    
    dummy_bumpers <- ns_tracks %>% 
      filter(block_order == 1 & track_order == 1) %>% 
      mutate(block_order = 0, length = 30L)
    
    # + draaiboeken gids ----
    drb_gids <- rbind(dummy_bumpers, ns_tracks) %>% 
      # filter(track_order == 1) %>% 
      arrange(pl_name, block_order, track_order) %>% 
      group_by(pl_name) %>%
      mutate(
        cum_tijd = np_sec2hms(cumsum(as.duration(length))),
        cum_tijd = lag(cum_tijd, n = 1)) %>% 
      mutate(
        wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = pl_name)) %>% 
      filter(block_order != 0) %>% 
      select(-album, -recording_no, -c(pl_id:user_id), -c(block_order:track_id))
    
    # + update gids ----
    for (cur_pl in unique(ns_tracks$pl_name)) {
      
      sql_post_date <- playlist2postdate(cur_pl) %>% as.character

      drb_gids_pl <- drb_gids %>% filter(pl_name == cur_pl)
      
      koptekst <- drb_gids_pl %>% select(pl_name, componist) %>% distinct %>% 
        group_by(pl_name) %>% summarise(werken_van = paste(componist, collapse = ", "))
      
      # sql_gidstekst <- sprintf("Werken van %s.\n<!--more-->\n\n", koptekst$werken_van)
      sql_gidstekst <- "@HEADER\n<!--more-->\n\n"
      
#       regel <- 
#         '<a>
# <img class="aligncenter" src="https://www.concertzender.nl/wp-content/uploads/2023/06/rata_logo.png" />
# </a>
# &nbsp;'
#       sql_gidstekst <- paste0(sql_gidstekst, regel, "\n")
      
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
      
      # + muw logo ----
      regel <- 
        '</tbody>\n</table>
&nbsp;
<a href="https://www.muziekweb.nl">
<img class="aligncenter" src="https://www.concertzender.nl/wp-content/uploads/2023/06/dank_muw_logo.png" />
</a>
&nbsp;
&nbsp;'
      
      sql_gidstekst <- paste0(sql_gidstekst, regel, "\n") %>% str_replace_all("[']", "&#39;")
      
      upd_stmt01 <- sprintf(
        "select id from wp_posts where post_date = '%s' and post_type = 'programma' order by 1;",
        sql_post_date
      )
      dsSql01 <- dbGetQuery(ns_con, upd_stmt01)
      
      # + gidskop bepalen ----
      hdr_nl_df <- bum.3 %>% 
        mutate(hdr_key = sub(".*\\.\\d{3}_(.*)", "\\1", pl_name, perl=TRUE)) %>% 
        left_join(gd_wp_gidsinfo_header_NL) %>% 
        filter(pl_name == cur_pl)
      
      hdr_en_df <- bum.3 %>% 
        mutate(hdr_key = sub(".*\\.\\d{3}_(.*)", "\\1", pl_name, perl=TRUE)) %>% 
        left_join(gd_wp_gidsinfo_header_EN) %>% 
        filter(pl_name == cur_pl)
      
      for (u1 in 1:nrow(dsSql01)) {

        if (u1 == 1) {
          sql_gidstekst1 <- sql_gidstekst %>% str_replace("@HEADER", hdr_nl_df$hdr_txt)
        } else {
          sql_gidstekst1 <- sql_gidstekst %>% str_replace("@HEADER", hdr_en_df$hdr_txt)
        }
        
        upd_stmt02 <- sprintf(
          "update wp_posts set post_content = '%s' where id = %s;",
          sql_gidstekst1,
          as.character(dsSql01$id[u1])
        )
        
        dbExecute(ns_con, upd_stmt02)
      }
      
      flog.info("Gids bijgewerkt: %s", cur_pl, name = "nsbe_log")
      
      # + replace replay-post? ---- 
      #   only if a recycle episode is available
      recycle_pl <- get_replay_playlist(cur_pl)
      
      if (recycle_pl == cur_pl) {
        flog.info("Kringloopherhaling is nog niet beschikbaar", name = "nsbe_log")
        next
      }
      
      cur_pl_date <- playlist2postdate(cur_pl)
      replay_date <- cur_pl_date + days(7L)
      recycle_pl_date <- playlist2postdate(recycle_pl)
      
      tmp_log <- sprintf("Kringloopherhaling: op %s klinkt die van %s", 
                         replay_date, recycle_pl_date)
      flog.info(tmp_log, name = "nsbe_log")
      
      # + . get replay_date's post-id ----
      upd_stmt06 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        replay_date)
      
      replay_pgm_id <- dbGetQuery(ns_con, upd_stmt06)
      
      # + . get recycle-post's id ----
      upd_stmt04 <- sprintf(
        "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
        recycle_pl_date)
      
      recycle_pgm_id <- dbGetQuery(ns_con, upd_stmt04) 
      
      # + . update post-id's NL/EN ----
      for (r1 in 1:2) {
        recycle_pgm_id_chr <- as.character(recycle_pgm_id$min_id + r1 - 1L)
        replay_pgm_id_chr <- as.character(replay_pgm_id$min_id + r1 - 1L)
        upd_stmt05 <-
          sprintf(
            "update wp_postmeta set meta_value = %s where post_id = %s and meta_key = 'pr_metadata_orig';",
            recycle_pgm_id_chr,
            replay_pgm_id_chr
          )
        
        flog.info("SQL: %s", upd_stmt05, name = "nsbe_log")
        
        dbExecute(ns_con, upd_stmt05)
      }
      
      suppressMessages(stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a"))
      dummy_pl <- paste0(stamped_format(cur_pl_date + days(7L)), str_sub(cur_pl, 12))
      flog.info("Gids replay bijgewerkt: %s", dummy_pl, name = "nsbe_log")
    }
  }

#_----

# VOT klaarzetten ----
if (exists("vot.3") && nrow(vot.3) > 0) {
  
  ns_tracks <- playlists.6 %>% filter(pl_name %in% vot.3$pl_name)
  
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
      cum_tijd = lag(cum_tijd, n = 1)) %>% 
    mutate(
      wallclock = get_wallclock(pm_cum_tijd = cum_tijd, pm_playlist = pl_name)) %>% 
    filter(block_order != 0) %>% 
    select(-album, -recording_no, -c(pl_id:user_id), -c(block_order:track_id))
  
  # update gids ----
  for (cur_pl in unique(ns_tracks$pl_name)) {
    
    sql_post_date <- playlist2postdate(cur_pl) %>% as.character
    
    drb_gids_pl <- drb_gids %>% filter(pl_name == cur_pl)
    
    koptekst <- drb_gids_pl %>% select(pl_name, componist) %>% distinct %>% 
      group_by(pl_name) %>% summarise(werken_van = paste(componist, collapse = ", "))
    
    sql_gidstekst <- paste0("Werken van ", koptekst$werken_van, "\n<!--more-->\n\n")
    
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
    
    # + muw logo ----
    regel <- 
      '</tbody>\n</table>
&nbsp;
<a href="https://www.muziekweb.nl">
<img class="aligncenter" src="https://www.concertzender.nl/wp-content/uploads/2023/06/dank_muw_logo.png" />
</a>
&nbsp;
&nbsp;'
    
    sql_gidstekst <- paste0(sql_gidstekst, regel, "\n") %>% str_replace_all("[']", "&#39;")
    
    qry_stmt01 <- sprintf(
      "select id from wp_posts where post_date = '%s' and post_type = 'programma' order by 1;",
      sql_post_date)
    dsSql01 <- dbGetQuery(ns_con, qry_stmt01)
    
    upd_stmt02 <- sprintf(
      "update wp_posts set post_content = '%s' where id in %s;",
      sql_gidstekst,
      paste0("('", str_flatten(as.character(dsSql01[[1]]), "', '"), "')"))
    dbExecute(ns_con, upd_stmt02)
    
    flog.info("Gids origineel bijgewerkt: %s", cur_pl, name = "nsbe_log")
    
    # + replace replay-post? ---- 
    #   only if a recycle episode is available
    recycle_pl <- get_replay_playlist(cur_pl)
    
    if (recycle_pl == cur_pl) {
      flog.info("Kringloopherhaling is nog niet beschikbaar", name = "nsbe_log")
      next
    }
    
    cur_pl_date <- playlist2postdate(cur_pl)
    replay_date <- cur_pl_date + days(7L)
    recycle_pl_date <- playlist2postdate(recycle_pl)
    
    tmp_log <- sprintf("Kringloopherhaling: op %s klinkt die van %s", 
                       replay_date, recycle_pl_date)
    flog.info(tmp_log, name = "nsbe_log")
    
    # + . get replay_date's post-id ----
    upd_stmt06 <- sprintf(
      "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
      replay_date)
    
    replay_pgm_id <- dbGetQuery(ns_con, upd_stmt06)
    
    # + . get recycle-post's id ----
    upd_stmt04 <- sprintf(
      "select min(id) as min_id from wp_posts where post_date = '%s' and post_type = 'programma';",
      recycle_pl_date)
    
    recycle_pgm_id <- dbGetQuery(ns_con, upd_stmt04) 
    
    # + . update post-id's NL/EN ----
    for (r1 in 1:2) {
      recycle_pgm_id_chr <- as.character(recycle_pgm_id$min_id + r1 - 1L)
      replay_pgm_id_chr <- as.character(replay_pgm_id$min_id + r1 - 1L)
      upd_stmt05 <-
        sprintf(
          "update wp_postmeta set meta_value = %s where post_id = %s and meta_key = 'pr_metadata_orig';",
          recycle_pgm_id_chr,
          replay_pgm_id_chr
        )
      
      flog.info("SQL: %s", upd_stmt05, name = "nsbe_log")
      
      dbExecute(ns_con, upd_stmt05)
    }
    
    suppressMessages(stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a"))
    dummy_pl <- paste0(stamped_format(cur_pl_date + days(7L)), str_sub(cur_pl, 12))
    flog.info("Gids replay bijgewerkt: %s", dummy_pl, name = "nsbe_log")
  }
}

#_----

# Samenstellers rechttrekken ----
sql_sel_ed_info <- "
SELECT DISTINCT
    po1.ID AS post_id,
    DATE_FORMAT(po1.post_date, '%a %Y-%m-%d %H:%i') AS post_date,
    po1.post_title,
    CASE WHEN tr1.term_taxonomy_id = 4 THEN 'EN'
         ELSE 'NL'
    END AS post_lang,
    te1.name AS editor_name_nipstu,
    mp1.user_id as editor_id_nipstu,
    tx1.term_id AS editor_id_website
FROM
    wp_term_taxonomy tx1
        JOIN
    wp_terms te1 ON te1.term_id = tx1.term_id
        JOIN
    wp_users u1 ON u1.display_name = te1.name
        JOIN
    wp_nipper_main_playlists mp1 ON mp1.user_id = u1.ID
        JOIN
    wp_posts po1 ON po1.post_date = STR_TO_DATE(CONCAT(mp1.program_date, ' ', mp1.time_start), '%Y-%m-%d %H') 
        JOIN
    wp_term_relationships tr1 ON tr1.object_id = po1.id
        JOIN
    wp_postmeta pm1 ON pm1.post_id = po1.id
WHERE
    tx1.taxonomy = 'programma_maker'
        AND mp1.deleted = 0 
        AND mp1.finished = 1
        AND (tr1.term_taxonomy_id = 5 AND te1.slug NOT REGEXP '.*-en$' OR 
             tr1.term_taxonomy_id = 4 AND te1.slug REGEXP '.*-en$')
ORDER BY 1;
"

ns_editor_info <- dbGetQuery(conn = ns_con, statement = sql_sel_ed_info)
ns_editor_info.1 <- ns_editor_info %>% mutate(post_id_chr = as.character(post_id))

for (pid in ns_editor_info.1$post_id_chr) {
  
  flog.info("update editor in post %s", pid, name = "nsbe_log")
  
  sql_sel_tr1 <- sprintf(
       "select tr1.* from wp_term_relationships tr1
           LEFT JOIN wp_term_taxonomy tx1 
                  ON tx1.term_taxonomy_id = tr1.term_taxonomy_id
           LEFT JOIN wp_terms te1 
                  ON te1.term_id = tr1.term_taxonomy_id
        WHERE tr1.object_id = %s AND tx1.taxonomy = 'programma_maker'", pid)
  
  flog.info("Query to find current editor: %s", sql_sel_tr1, name = "nsbe_log")
  
  ed_term_rel <- dbGetQuery(ns_con, sql_sel_tr1)
  flog.info("current editor found: %s", as.character(ed_term_rel$term_taxonomy_id), name = "nsbe_log")
  
  ns_ed_info <- ns_editor_info.1 %>% filter(post_id_chr == pid)
  flog.info("change web page editor to %s", as.character(ns_ed_info$editor_id_website), name = "nsbe_log")

  sql_upd_tr1 <- sprintf(
       "UPDATE wp_term_relationships
      SET term_taxonomy_id = %s
      WHERE object_id = %s AND term_taxonomy_id = %s;",
      as.character(ns_ed_info$editor_id_website),
      as.character(ed_term_rel$object_id),
      as.character(ed_term_rel$term_taxonomy_id))

  flog.info("Query to update web page editor (taxonomy): %s", sql_upd_tr1, name = "nsbe_log")

  dbExecute(ns_con, sql_upd_tr1)

  sql_upd_pm1 <- 
    sprintf("update wp_postmeta set meta_value = 
                (select te1.name from wp_term_relationships tr1
                                 left join wp_term_taxonomy tx1 
                                        on tx1.term_taxonomy_id = tr1.term_taxonomy_id
                                 left join wp_terms te1 
                                        on te1.term_id = tr1.term_taxonomy_id
                 where tr1.object_id = %s and taxonomy = 'programma_maker')
             where post_id = %s and meta_key = 'pr_metadata_production1_person';", pid, pid)

  flog.info("Query to update web page editor (post_meta): %s", sql_upd_pm1, name = "nsbe_log")

  dbExecute(ns_con, sql_upd_pm1)
}
