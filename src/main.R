# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-playlists, -schedules, draaiboeken en gidsvermeldingen..
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
pacman::p_load(knitr, rmarkdown, RCurl, readr, futile.logger, DBI, officer, httr,
               xml2, tidyverse, keyring, googlesheets4, yaml, fs, magrittr, hms,
               lubridate, zip, stringr, fastmap)

home_prop <- function(prop) {
  prop_name <- paste0(prop, ".", host)
  prop <- config[[prop_name]] |> 
    curlEscape()  |>  
    str_replace_all(pattern = "\\%2F", replacement = "/")
}

# Init ----
config <- read_yaml("config_nip_stu.yaml")
rds_home <- "C:/cz_salsa/cz_exchange/"
filter <- dplyr::filter # voorkom verwarring met stats::filter

# 4 x functions only
source(config$toolbox, encoding = "UTF-8") 
source("src/compile_schedulerscript.R", encoding = "UTF-8")
source("src/compile_hostscript_docx.R", encoding = "UTF-8")
source("src/shared_functions.R", encoding = "UTF-8")

host <- config$host
home_vt_audio_mac <- home_prop("home_vt_audio_mac")
home_vt_audio_win  <- home_prop("home_vt_audio_win") |> str_replace_all(pattern = "%20", replacement = " ")
home_radiologik <- home_prop("home_radiologik_win")
switch_home <- paste0(home_prop("home_schedulerswitch"), "nipper_msg.txt")
RL_scheduler_running <- TRUE

fa <- flog.appender(appender.file("c:/cz_salsa/Logs/ns_bum_vot.log"), name = "nsbe_log")
flog.info("
= = = = = NipperStudio start (versie 2025-01-26) = = = = =", name = "nsbe_log")

# start MCL
repeat { 
  # + connect to DB ----
  ns_con <- get_ns_conn("PRD")
  
  if (typeof(ns_con) != "S4") {
    flog.error("WP-database is niet beschikbaar", name = "nsbe_log")
    break
  }
  
  # + lees pl van WP ----
  query <- "select * from wp_nipper_main_playlists"
  playlists_db <- dbGetQuery(conn = ns_con, statement = query)
  playlists.1 <- playlists_db |> 
    filter(deleted == 0 & finished == 1) |> 
    select(pl_id = id,
           pl_date = program_date,
           pl_start = time_start,
           pl_transit = block_transition,
           pl_state = finished,
           title_id = program_id,
           user_id)
  
  # + lees pl van NS ----
  playlists_his <- read_rds(paste0(rds_home, "nipper_main_playlists.RDS")) |> 
    select(pl_date = program_date, 
           pl_start = time_start, 
           post_id,
           pl_name = playlist_name)
  
  playlists.2 <- playlists.1 |> 
    left_join(playlists_his, by = join_by(pl_date, pl_start))
  
  # + check pl's compleet ----
  playlists.2_err <- playlists.2 |> filter(is.na(pl_name))
  
  # + . pl's niet compleet ----
  if (nrow(playlists.2_err) > 0) {
    dbDisconnect(ns_con)
    flog.error("Playlistweek-taak heeft niet alle WP-playlists aangemeld bij NS-backend", name = "nsbe_log")
    break
  }
  
  # + get WP-gidsinfo ----
  tryCatch(
    {
      gs4_auth(email = "cz.teamservice@gmail.com")
      gd_wp_gidsinfo_header <- read_sheet(ss = config$url_wp_gidsinfo, sheet = "gids-info")
      gd_wp_gidsinfo_slugs_raw <- read_sheet(ss = config$url_wp_gidsinfo, sheet = "nipperstudio_slugs")
      audio_locaties <- read_sheet(ss = config$url_audio_locaties, sheet = "audio_locaties")
    },
    error = function(e1) {
      flog.error("Load error GD-sheet(s): %s", conditionMessage(e1), name = "nsbe_log")
      break
    })
  
  # + lees blokken ----
  query <- "select * from wp_nipper_blocks"
  blocks_db <- dbGetQuery(conn = ns_con, statement = query)
  blocks.1 <- blocks_db |> 
    filter(deleted == 0) |> 
    arrange(playlist_id, sort_order) |> 
    select(pl_id = playlist_id,
           block_order = sort_order,
           block_id = id) 
  
  # + lees tracks ----
  query <- "select * from wp_nipper_tracklists"
  tracks_db <- dbGetQuery(conn = ns_con, statement = query)
  tracks.1 <- tracks_db |> 
    filter(deleted == 0) |> 
    arrange(block_id, sort_order) |> 
    select(block_id,
           track_order = sort_order,
           track_id = id,
           everything())
  
  # + merge ----
  playlists.3 <- playlists.2 |> 
    left_join(blocks.1, by = join_by(pl_id)) |> 
    left_join(tracks.1, by = join_by(block_id)) |> 
    select(-datetime_created, -track_number, -deleted)
  
  # log aangeboden playlists ----
  playlists.4 <- playlists.3 |> filter(pl_state == 1) |> select(pl_name, post_id) |> distinct()
  playlists.5 <- playlists.3 |> filter(pl_state == 1)
  
  # niets aangeboden: stop ----
  if (nrow(playlists.4) == 0) {
    flog.info("Geen uitzendingen aangeboden", name = "nsbe_log")
    break
  }
  
  pl_log_names <- str_flatten(playlists.4$pl_name, collapse = "\n")
  pl_log_posts <- str_flatten(playlists.4$post_id, collapse = ", ")
  flog.info(paste0("Aangeboden playlists:\n", pl_log_names), name = "nsbe_log")
  flog.info(paste0("posts = ", pl_log_posts), name = "nsbe_log")
  
  # check BUM audio ----
  # + get editors ----
  # editors worden afgekondigd na laatste bumperblok
  bum.1 <- playlists.5 |> 
    filter(pl_transit %in% c("LOB", "HIB")) |> 
    select(pl_name, user_id, title_id, pl_transit) |> distinct()
  
  # check VOT ----
  vot.1 <- playlists.5 |> 
    filter(pl_transit == "VOT") |> 
    select(pl_name, user_id, title_id, pl_transit) |> distinct()
  
  # STOP RL-scheduler ----
  flog.info("RL-scheduler stoppen", name = "nsbe_log")
  switch <- read_lines(file = switch_home)
  switch <- "stop RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  Sys.sleep(time = 5)
  flog.info("RL-scheduler is gestopt", name = "nsbe_log")
  RL_scheduler_running <- FALSE
  
  #_----
  # bumper PL's maken ----
  if (nrow(bum.1) > 0) {
    
    bum_editor_list <- paste0("('", bum.1$user_id |> str_flatten(collapse = "', '"), "')")
    sqlstmt <- sprintf("select display_name as user_name, id as user_id from wp_users where id in %s;", 
                       bum_editor_list)
    bum_wp_users <- dbGetQuery(conn = ns_con, statement = sqlstmt)
    bum.2 <- bum_wp_users |> left_join(bum.1, by = join_by(user_id))

    # + . gids koppen ----
    gd_wp_gidsinfo_header_NL <- gd_wp_gidsinfo_header |> 
      select(hdr_key = `key-modelrooster`, hdr_txt = `std.samenvatting-NL`) |> 
      mutate(hdr_key = hdr_key |> str_to_lower() |> str_replace_all(pattern = "[- ]", replacement = "_")
             |> str_replace_all(pattern = "___", replacement = "__"))
    gd_wp_gidsinfo_header_EN <- gd_wp_gidsinfo_header |> 
      select(hdr_key = `key-modelrooster`, hdr_txt = `std.samenvatting-EN`) |> 
      mutate(hdr_key = hdr_key |> str_to_lower() |> str_replace_all(pattern = "[- ]", replacement = "_")
             |> str_replace_all(pattern = "___", replacement = "__"))

    titel_slugs <- gd_wp_gidsinfo_slugs_raw |> 
      select(starts_with("titel")) |> 
      mutate(title_id = as.integer(titel_id)) |> 
      filter(!is.na(title_id))
    
    redacteur_slugs <- gd_wp_gidsinfo_slugs_raw |> 
      select(starts_with("redacteur")) |> 
      mutate(user_id = as.integer(redacteur_id)) |> 
      filter(!is.na(user_id))
    
    bum.3 <- bum.2 |> 
      left_join(titel_slugs, by = join_by(title_id)) |> 
      left_join(redacteur_slugs, by = join_by(user_id)) |> 
      select(pl_name,
             user_id,
             user_name,
             user_slug = redacteur_slug,
             title_id,
             title_name = titel,
             title_slug = titel_slug,
             pl_transit)
    
    bum.3_err <- bum.3 |> 
      filter(is.na(user_slug) | is.na(title_slug)) |> 
      select(pl_name) |> 
      mutate(pl_status = 3L)
    
    if (nrow(bum.3_err) > 0) {
      pl_in_err <- str_flatten(bum.3_err$pl_name, collapse = "\n")
      flog.info(paste0("Registratie van redacteur en/of pgm-titel is incompleet:\n",
                       pl_in_err, "\nZie WP-gidsinfo/nipperstudio_slugs"), name = "nsbe_log")
      bum.3 <- bum.3 |> anti_join(bum.3_err)
    }
    
    bum.4 <- bum.3 |> select(-user_id, -user_name, -title_id, -title_name) |> distinct() |> 
      mutate(dir_transit_aan = if_else(pl_transit == "LOB", "laag/aan/", "hoog/aan/"),
             dir_transit_over = if_else(pl_transit == "LOB", "laag/over/", "hoog/over/"),
             dir_transit_af = if_else(pl_transit == "LOB", 
                                      paste0("laag/af/", user_slug),
                                      paste0("hoog/af/", user_slug)))
    
    # + bumper folders ----
    bf_home <- "//uitzendmac-2/Data/Nipper/studiomontage/bumpers/"
    
    bf_bumpers <- tibble(bkey = "laag/over/", dir_transit_over_bumpers = paste0("bumper0", 1:5, "/")) |> 
      add_row(bkey = "hoog/over/", dir_transit_over_bumpers = paste0("bumper0", 1:5, "/"))
    
    # + . audio = AAN ----
    bum_aan <- bum.4 |> select(pl_name, title_slug, dir_transit_aan) |> 
      mutate(dir_aan = paste0(bf_home, title_slug, "/", dir_transit_aan)) |> 
      select(pl_name, ns_dir = dir_aan)
    
    # skips silently if exists
    dir_create(bum_aan$ns_dir)
    
    # check files
    for (bdir in bum_aan$ns_dir) {
      
      n_bfiles <- length(dir_ls(bdir, type = "file"))
      
      if (n_bfiles != 1L) {
        pl_in_err <- bum_aan |> filter(ns_dir == bdir) |> select(pl_name)
        flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                         str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                         " voor playlist ", pl_in_err), name = "nsbe_log")
        bum.3_err <- bum.3_err |> add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
      }
    }
    
    # + . audio = AF ----
    bum_af <- bum.4 |> select(pl_name, title_slug, dir_transit_af) |> 
      mutate(dir_af = paste0(bf_home, title_slug, "/", dir_transit_af)) |> 
      select(pl_name, ns_dir = dir_af)
    
    # skips silently if exists
    dir_create(bum_af$ns_dir)
    
    # check files
    for (bdir in bum_af$ns_dir) {
      
      n_bfiles <- length(dir_ls(bdir, type = "file"))
      
      if (n_bfiles != 1L) {
        pl_in_err <- bum_af |> filter(ns_dir == bdir) |> select(pl_name)
        flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                         str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                         " voor playlist ", pl_in_err), name = "nsbe_log")
        bum.3_err <- bum.3_err |> add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
      }
    }
    
    # + . audio = OVER ----
    bum_over <- bum.4 |> select(pl_name, title_slug, dir_transit_over) |> 
      left_join(bf_bumpers, by = c("dir_transit_over" = "bkey")) |> 
      mutate(dir_over = paste0(bf_home, title_slug, "/", dir_transit_over, dir_transit_over_bumpers)) |> 
      select(pl_name, ns_dir = dir_over)
    
    # skips silently if exists
    dir_create(bum_over$ns_dir)
    
    # check files
    for (bdir in bum_over$ns_dir) {
      
      n_bfiles <- length(dir_ls(bdir, type = "file"))
      
      if (n_bfiles != 1L) {
        pl_in_err <- bum_over |> filter(ns_dir == bdir) |> select(pl_name)
        flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                         str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                         " voor playlist ", pl_in_err), name = "nsbe_log")
        bum.3_err <- bum.3_err |> add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
      }
    }
    
    # + save err-playlists ----
    bum.3_err <- bum.3_err |> distinct()
    bum.3 <- bum.3 |> anti_join(bum.3_err)
    
    # + Bepaal playlist lengtes ----
    playlists.6 <- playlists.5 |> 
      group_by(pl_id, block_order, block_id) |> 
      mutate(blokduur_sec = sum(length)) |> ungroup()
    
    # + RL-sched's, PL's en draaiboeken maken ----
    tryCatch(
      {
        for (cur_pl in bum.3$pl_name) {
          
          duration_rlprg <- 3600L * as.numeric(str_sub(cur_pl, 15, 17)) / 60L
          
          rlprg_file <- bum.3 |> filter(pl_name == cur_pl) |> 
            mutate(cur_duur_parm = paste0("Duration:", duration_rlprg)) |> 
            select(cur_duur_parm) |> 
            unite(col = regel, sep = "\t")
          
          cur_pl_nieuw <- playlists.6 |> filter(pl_name == cur_pl)
          
          blokken <- cur_pl_nieuw |> distinct(block_order) |> 
            mutate(bid = paste0("RL_BLK_", block_order)) |> 
            select(vt_blok_letter = bid)
          
          # + bumpervolgorde ---- 
          bumper_stack <- faststack()
          
          bu_seq <- random_select(c(1:5), nrow(blokken) - 1)
          print(bu_seq)
          
          for (i1 in length(bu_seq):1) {
            bumper_stack$push(bu_seq[i1])
          }
          
          # + blokken ---- 
          for (blok in blokken$vt_blok_letter) {
            cur_pres <- cur_pl |> as_tibble() |> 
              mutate(
                duur = "",
                audiofile = get_bumper_audio(pm_playlist = cur_pl, pm_blok = blok, pm_stack = bumper_stack),
                const_false = "FALSE",
                start_sec_sinds_middernacht = if_else(blok == "RL_BLK_1",
                                                      cur_pl_nieuw$pl_start[1] * 3600L,
                                                      -1L), # -1 = direct erna afspelen
                fwdtab1 = "",
                fwdtab2 = "",
                fwdtab3 = "",
                speler_regel01 = cur_pl_nieuw$pl_transit[1],
                opname_hfd_sub = "",
                speler_regel02 = blok) |> 
              select(-value) |> 
              unite(col = regel, sep = "\t")
            
            cur_block_order <- match(blok, blokken$vt_blok_letter)
            
            cur_tracks_in_blok <- cur_pl_nieuw |> 
              filter(pl_name == cur_pl & block_order == cur_block_order) |> 
              # if recording_no has several tracks, put each track in a separate row
              separate_rows(recording_no, sep = ", ") |> 
              mutate(
                duur = "",
                audiofile = paste0("file:///Volumes/Data/Nipper/muziekweb_audio/", recording_no, ".wav"),
                const_false = "FALSE",
                start_sec_sinds_middernacht = -1, # "direct erna afspelen"
                fwdtab1 = "",
                fwdtab2 = "",
                fwdtab3 = "",
                speler_regel01 = componist,
                opname_hfd_sub = paste0("NS", post_id[1], 
                                        "-", 
                                        blokken$vt_blok_letter[cur_block_order]),
                speler_regel02 = titel
              ) |> 
              select(duur, audiofile, const_false, start_sec_sinds_middernacht, 
                     fwdtab1, fwdtab2, fwdtab3, speler_regel01, opname_hfd_sub, speler_regel02) |> 
              unite(col = regel, sep = "\t")
            
            rlprg_file <- bind_rows(rlprg_file, cur_pres, cur_tracks_in_blok)
          }
          
          # + AF-blok ----
          cur_pres <- cur_pl |> as_tibble() |> 
            mutate(
              duur = "",
              audiofile = get_bumper_audio(pm_playlist = cur_pl, pm_blok = "SLOT", pm_stack = bumper_stack),
              const_false = "FALSE",
              start_sec_sinds_middernacht = -1L, 
              fwdtab1 = "",
              fwdtab2 = "",
              fwdtab3 = "",
              speler_regel01 = cur_pl_nieuw$pl_transit[1],
              opname_hfd_sub = "",
              speler_regel02 = "SLOT") |> 
            select(-value) |> 
            unite(col = regel, sep = "\t")
          
          rlprg_file <- bind_rows(rlprg_file, cur_pres)
          
          cur_pl <- cur_pl |> str_replace_all(pattern = "[.]", replacement = "-")
          
          # + write RL-playlist ----
          home_radiologik_playlists <- paste0(home_prop("home_radiologik_win"), "Programs/")
          rlprg_file_name <- paste0(home_radiologik_playlists, cur_pl, ".rlprg")
          write.table(x = rlprg_file, file = rlprg_file_name, row.names = FALSE, col.names = FALSE, 
                      sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
          
          flog.info("RL-playlist toegevoegd: %s", rlprg_file_name, name = "nsbe_log")
          
          # + write NEW schedule ----
          build_rl_script(cur_pl)
          
          # + write REPLAY schedule ----
          cur_pl_date <- playlist2postdate(cur_pl)
          stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a", quiet = T)
          replay_sched <- paste0(stamped_format(cur_pl_date + days(7L)), str_sub(cur_pl, 12))
          replay_pl <- get_replay_playlist(cur_pl)
          RL_replay <- c(replay_sched, replay_pl)
          build_rl_script(RL_replay)
        }
        
        # + gids bijwerken ----
        source("src/update_gids.R", encoding = "UTF-8")
        
        # + MuW audio order forms ----
        form_pls <- bum.1 |> anti_join(bum.3_err) |> select(pl_name)
        
        for (cur_form_pl in form_pls$pl_name) {
          create_form(cur_form_pl) 
        }
      },
      error = function(e1) {
        flog.error("Segmentfout bumper-PL's: %s", conditionMessage(e1), name = "nsbe_log")
        break
      }
    )
    
    # + playlists voltooid melden ----
    pl_finshed <- bum.1 |> anti_join(bum.3_err) |> 
      mutate(pl_status = 2) |> select(pl_name, pl_status) |> 
      add_row(bum.3_err)
    
    # + . geslaagd ----
    pl_passed <- pl_finshed |> filter(pl_status == 2) |> select(pl_name)
    
    if (nrow(pl_passed) > 0) {
      
      pl_passed_str <- paste0("('",
                              str_flatten(string = pl_passed$pl_name, collapse = "', '"),
                              "')")
      sql_stmt <- sprintf("update wp_nipper_main_playlists set finished = 2 where playlist_name in %s",
                          pl_passed_str)
      dbExecute(conn = ns_con, statement = sql_stmt)
      flog.info(paste0("Geslaagd: ", pl_passed_str), name = "nsbe_log")
    }
    
    # + . mislukt ----
    pl_failed <- pl_finshed |> filter(pl_status == 3) |> select(pl_name)
    
    if (nrow(pl_failed) > 0) {
      
      pl_failed_str <- paste0("('",
                              str_flatten(string = pl_failed$pl_name, collapse = "', '"),
                              "')")
      sql_stmt <- sprintf("update wp_nipper_main_playlists set finished = 3 where playlist_name in %s",
                          pl_failed_str)
      dbExecute(conn = ns_con, statement = sql_stmt)
      flog.info(paste0("Mislukt: ", pl_failed_str), name = "nsbe_log")
    }
  }
  
  #_----
  # voice track PL's maken ----
  if (nrow(vot.1) > 0) {
    
    vot_editor_list <- paste0("('", vot.1$user_id |> str_flatten(collapse = "', '"), "')")
    sqlstmt <- sprintf("select display_name as user_name, id as user_id from wp_users where id in %s;", 
                       vot_editor_list)
    vot_wp_users <- dbGetQuery(conn = ns_con, statement = sqlstmt)
    vot.2 <- vot_wp_users |> left_join(vot.1)

    # + . gids koppen ----
    gd_wp_gidsinfo_header_NL <- gd_wp_gidsinfo_header |> 
      select(hdr_key = `key-modelrooster`, hdr_txt = `std.samenvatting-NL`) |> 
      mutate(hdr_key = hdr_key |> str_to_lower() |> str_replace_all(pattern = "[- ]", replacement = "_")
             |> str_replace_all(pattern = "___", replacement = "__"))
    gd_wp_gidsinfo_header_EN <- gd_wp_gidsinfo_header |> 
      select(hdr_key = `key-modelrooster`, hdr_txt = `std.samenvatting-EN`) |> 
      mutate(hdr_key = hdr_key |> str_to_lower() |> str_replace_all(pattern = "[- ]", replacement = "_")
             |> str_replace_all(pattern = "___", replacement = "__"))
    
    # + . gids slugs ----
    titel_slugs <- gd_wp_gidsinfo_slugs_raw |> 
      select(starts_with("titel")) |> 
      mutate(title_id = as.integer(titel_id)) |> 
      filter(!is.na(title_id))
    
    redacteur_slugs <- gd_wp_gidsinfo_slugs_raw |> 
      select(starts_with("redacteur")) |> 
      mutate(user_id = as.integer(redacteur_id)) |> 
      filter(!is.na(user_id))
    
    vot.3 <- vot.2 |> 
      left_join(titel_slugs) |> 
      left_join(redacteur_slugs) |> 
      select(pl_name,
             user_id,
             user_name,
             user_slug = redacteur_slug,
             title_id,
             title_name = titel,
             title_slug = titel_slug,
             pl_transit)
    
    vot.3_err <- vot.3 |> 
      filter(is.na(user_slug) | is.na(title_slug)) |> 
      select(pl_name) |> 
      mutate(pl_status = 3L)
    
    if (nrow(vot.3_err) > 0) {
      pl_in_err <- str_flatten(vot.3_err$pl_name, collapse = "\n")
      flog.info(paste0("Registratie van redacteur en/of pgm-titel is incompleet:\n",
                       pl_in_err, "\nZie WP-gidsinfo/nipperstudio_slugs"), name = "nsbe_log")
      vot.3 <- vot.3 |> anti_join(vot.3_err)
    }
    
    # + Bepaal playlist lengtes ----
    playlists.6 <- playlists.5 |> 
      group_by(pl_id, block_order, block_id) |> 
      mutate(blokduur_sec = sum(length)) |> ungroup()

    # + RL-sched's, PL's en draaiboeken maken ----
    tryCatch(
      {
        for (cur_pl in vot.3$pl_name) {
          
          duration_rlprg <- 3600L * as.numeric(str_sub(cur_pl, 15, 17)) / 60L
          
          rlprg_file <- vot.3 |> filter(pl_name == cur_pl) |> 
            mutate(cur_duur_parm = paste0("Duration:", duration_rlprg)) |> 
            select(cur_duur_parm) |> 
            unite(col = regel, sep = "\t")
          
          cur_pl_nieuw <- playlists.6 |> filter(pl_name == cur_pl)
          
          blokken <- cur_pl_nieuw |> select(post_id, block_order) |> distinct() |> 
            mutate(bid = paste0("NS", post_id, "_", block_order)) |> 
            select(vt_blok_letter = bid)
          
          sluit_idx <- paste0("NS", cur_pl_nieuw$post_id, "_", (1 + max(cur_pl_nieuw$block_order))) |> unique()
          sluitblok <- sluit_idx |> as_tibble() |> setNames("vt_blok_letter")
          blokken <- bind_rows(blokken, sluitblok) 
          
          playlist_id_df <- cur_pl_nieuw |> 
            mutate(post_id_chr = as.character(post_id),
                   plid = as.integer(post_id_chr)) |> slice(1)
          
          playlist_id <- playlist_id_df$plid
          
          vt_blok_pad <- audio_locaties |> filter(sleutel == "vt_blok", functie == "pres_blok") |> 
            mutate(locatie = paste0(home_vt_audio_mac, locatie)) |> 
            select(locatie) 
          
          # + blokken ---- 
          for (blok in blokken$vt_blok_letter) {
            cur_pres <- cur_pl |> as_tibble() |> 
              mutate(
                duur = "",
                audiofile = paste0("file://", vt_blok_pad, blok, ".aif"),
                const_false = "FALSE",
                start_sec_sinds_middernacht = if_else(str_detect(blok, pattern = ".*_A$"),
                                                      cur_pl_nieuw$pl_start[1] * 3600L,
                                                      -1L), # -1 = direct erna afspelen
                fwdtab1 = "",
                fwdtab2 = "",
                fwdtab3 = "",
                speler_regel01 = "voicetracking",
                opname_hfd_sub = "",
                speler_regel02 = paste0("blok ", blok)) |> 
              select(-value) |> 
              unite(col = regel, sep = "\t")
            
            cur_block_order <- match(blok, blokken$vt_blok_letter)
            
            cur_tracks_in_blok <- cur_pl_nieuw |> 
              filter(pl_name == cur_pl & block_order == cur_block_order) |> 
              # if recording_no has several tracks, put each track in a separate row
              separate_rows(recording_no, sep = ", ") |> 
              mutate(
                duur = "",
                audiofile = paste0("file:///Volumes/Data/Nipper/muziekweb_audio/", recording_no, ".wav"),
                const_false = "FALSE",
                start_sec_sinds_middernacht = -1, # "direct erna afspelen"
                fwdtab1 = "",
                fwdtab2 = "",
                fwdtab3 = "",
                speler_regel01 = componist,
                opname_hfd_sub = blokken$vt_blok_letter[cur_block_order],
                speler_regel02 = titel
              ) |> 
              select(duur, audiofile, const_false, start_sec_sinds_middernacht, 
                     fwdtab1, fwdtab2, fwdtab3, speler_regel01, opname_hfd_sub, speler_regel02) |> 
              unite(col = regel, sep = "\t")
            
            rlprg_file <- bind_rows(rlprg_file, cur_pres, cur_tracks_in_blok)
          }
          
          cur_pl <- cur_pl |> str_replace_all(pattern = "[.]", replacement = "-")
          
          # + build RL-playlists ----
          home_radiologik_playlists <- paste0(home_prop("home_radiologik_win"), "Programs/")
          rlprg_file_name <- paste0(home_radiologik_playlists, cur_pl, ".rlprg")
          write.table(x = rlprg_file, file = rlprg_file_name, row.names = FALSE, col.names = FALSE, 
                      sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
          
          flog.info("RL-playlist toegevoegd: %s", rlprg_file_name, name = "nsbe_log")
          
          # + build NEW schedule ----
          build_rl_script(cur_pl)
          
          # + build REPLAY schedule ----
          cur_pl_date <- playlist2postdate(cur_pl)
          stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a", quiet = T)
          replay_sched <- paste0(stamped_format(cur_pl_date + days(7L)), str_sub(cur_pl, 12))
          replay_pl <- get_replay_playlist(cur_pl)
          RL_replay <- c(replay_sched, replay_pl)
          build_rl_script(RL_replay)
          
          # + build host scripts ----
          build_host_script(cur_pl_nieuw$pl_name[1])
        }
        
        # + gids bijwerken ----
        source("src/update_gids.R", encoding = "UTF-8")
        
        # + MuW audio order forms ----
        form_pls <- vot.1 |> anti_join(vot.3_err) |> select(pl_name)
        
        for (cur_form_pl in form_pls$pl_name) {
          create_form(cur_form_pl) 
        }
      },
      error = function(e1) {
        flog.error("Segmentfout voice track PL's: %s", conditionMessage(e1), name = "nsbe_log")
        break
      }
    )
    
    # + playlists voltooid melden ----
    pl_finshed <- vot.1 |> anti_join(vot.3_err) |> 
      mutate(pl_status = 2) |> select(pl_name, pl_status) |> 
      add_row(vot.3_err)
    
    # + . geslaagd ----
    pl_passed <- pl_finshed |> filter(pl_status == 2) |> select(pl_name)
    
    if (nrow(pl_passed) > 0) {
      
      pl_passed_str <- paste0("('",
                              str_flatten(string = pl_passed$pl_name, collapse = "', '"),
                              "')")
      sql_stmt <- sprintf("update wp_nipper_main_playlists set finished = 2 where playlist_name in %s",
                          pl_passed_str)
      
      dbExecute(conn = ns_con, statement = sql_stmt)
      
      flog.info(paste0("Geslaagd: ", pl_passed_str), name = "nsbe_log")
    }
    
    # + . mislukt ----
    pl_failed <- pl_finshed |> filter(pl_status == 3) |> select(pl_name)
    
    if (nrow(pl_failed) > 0) {
      
      pl_failed_str <- paste0("('",
                              str_flatten(string = pl_failed$pl_name, collapse = "', '"),
                              "')")
      sql_stmt <- sprintf("update wp_nipper_main_playlists set finished = 3 where playlist_name in %s",
                          pl_failed_str)
      dbExecute(conn = ns_con, statement = sql_stmt)
      flog.info(paste0("Mislukt: ", pl_failed_str), name = "nsbe_log")
    }
  }
  
  # exit MCL
  break
}

if (typeof(ns_con) == "S4") {
  dbDisconnect(ns_con)
}

# + restart RL-scheduler ----
if (!RL_scheduler_running) {
  flog.info("RL-scheduler starten...", name = "nsbe_log")
  switch <- read_lines(file = switch_home)
  switch <- "start RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  flog.info("RL-scheduler draait weer", name = "nsbe_log")
}

flog.info("= = = = = NipperStudio stop = = = = =", name = "nsbe_log")
