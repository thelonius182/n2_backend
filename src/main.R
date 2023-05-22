# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-playlists, -schedules, draaiboeken en gidsvermeldingen..
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
pacman::p_load(knitr, rmarkdown, RCurl, readr, futile.logger, DBI, officer, httr,
               xml2, tidyverse, keyring, googlesheets4, yaml, fs, magrittr, hms,
               lubridate, zip, stringr)

fa <- flog.appender(appender.file("c:/cz_salsa/Logs/nipperstudio_backend.log"), name = "nsbe_log")
flog.info("
= = = = = NipperStudio start = = = = =", name = "nsbe_log")

home_prop <- function(prop) {
  prop_name <- paste0(prop, ".", host)
  prop <- config[[prop_name]] %>% 
    curlEscape %>% 
    str_replace_all(pattern = "\\%2F", replacement = "/")
}

# Init ----
config <- read_yaml("config_nip_nxt.yaml")
rds_home <- "C:/cz_salsa/cz_exchange/"
filter <- dplyr::filter # voorkom verwarring met stats::filter

source(config$toolbox, encoding = "UTF-8") # functions only
source("src/compile_schedulerscript.R", encoding = "UTF-8") # functions only 
source("src/shared_functions.R", encoding = "UTF-8") # functions only 

host <- config$host
home_vt_audio_mac <- home_prop("home_vt_audio_mac")
home_vt_audio_win  <- home_prop("home_vt_audio_win") %>% 
  str_replace_all(pattern = "%20", replacement = " ")
home_radiologik <- home_prop("home_radiologik_win")
switch_home <- paste0(home_prop("home_schedulerswitch"), "nipper_msg.txt")
gs4_auth(email = "cz.teamservice@gmail.com")

# + connect to DB ----
# ns_con <- dbConnect(odbc::odbc(), "wpdev_mariadb", timeout = 10, encoding = "CP850")
ns_con <- get_ns_conn("DEV")

stopifnot("WP-database is niet beschikbaar, zie C:/cz_salsa/Logs/nipperstudio_backend.log" = typeof(ns_con) == "S4")
flog.info("Verbonden!", name = "nsbe_log")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# NipperStudio-versie van de spreadsheet maken
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# + lees pl op WP ----
query <- "select * from wp_nipper_main_playlists"
playlists_db <- dbGetQuery(conn = ns_con, statement = query)
playlists.1 <- playlists_db %>% 
  filter(deleted == 0) %>% 
  select(pl_id = id,
         pl_date = program_date,
         pl_start = time_start,
         pl_transit = block_transition,
         pl_state = finished,
         title_id = program_id,
         user_id)

# + koppel archief ----
playlists_his <- read_rds(paste0(rds_home, "nipper_main_playlists.RDS")) %>% 
  mutate(pl_date = ymd(program_date)) %>% 
  select(pl_date, 
         pl_start = time_start, 
         post_id,
         pl_name = playlist_name)

playlists.2 <- playlists.1 %>% 
  left_join(playlists_his)

# + lees blokken ----
query <- "select * from wp_nipper_blocks"
blocks_db <- dbGetQuery(conn = ns_con, statement = query)
blocks.1 <- blocks_db %>% 
  filter(deleted == 0) %>% 
  arrange(playlist_id, sort_order) %>% 
  select(pl_id = playlist_id,
         block_order = sort_order,
         block_id = id) 

# + lees tracks ----
query <- "select * from wp_nipper_tracklists"
tracks_db <- dbGetQuery(conn = ns_con, statement = query)
tracks.1 <- tracks_db %>% 
  filter(deleted == 0) %>% 
  arrange(block_id, sort_order) %>% 
  select(block_id,
         track_order = sort_order,
         track_id = id,
         everything())

# + merge ----
playlists.3 <- playlists.2 %>% 
  left_join(blocks.1) %>% 
  left_join(tracks.1) %>% 
  select(-datetime_created, -track_number, -deleted)

# log aangeboden playlists ----
playlists.4 <- playlists.3 %>% filter(pl_state == 1) %>% select(pl_name, post_id) %>% distinct()
playlists.5 <- playlists.3 %>% filter(pl_state == 1)

# niets aangeboden: stop ----
if (nrow(playlists.4) == 0) {
  flog.info("Geen nieuwe playlists gevonden", name = "nsbw_log")
  flog.info("= = = = = NipperStudio stop = = = = =", name = "nsbe_log")
  dbDisconnect(ns_con)
  stopifnot("Geen nieuwe playlists gevonden" = nrow(playlists.4) > 0)
}

pl_log_names <- str_flatten(playlists.4$pl_name, collapse = "\n")
pl_log_posts <- str_flatten(playlists.4$post_id, collapse = ", ")
flog.info(paste0("Aangeboden playlists:\n", pl_log_names), name = "nsbe_log")
flog.info(paste0("posts = ", pl_log_posts), name = "nsbe_log")

# check BUM audio ----
# + get editors ----
# editors worden afgekondigd na laatste bumperblok
bum.1 <- playlists.5 %>% 
  filter(pl_transit %in% c("LOB", "HIB")) %>% 
  select(pl_name, user_id, title_id, pl_transit) %>% distinct()

# TOT HIER ----

if (nrow(bum.1) > 0) {
  
  bum_editor_list <- paste0("('", bum.1$user_id %>% str_flatten(collapse = "', '"), "')")
  sqlstmt <- "select display_name as user_name, id as user_id from wp_users where id in @EDLST"
  sqlstmt <- str_replace(sqlstmt, "@EDLST", bum_editor_list)
  bum_wp_users <- dbGetQuery(conn = ns_con, statement = sqlstmt)
  bum.2 <- bum_wp_users %>% left_join(bum.1)
  
  # + get ns slugss ----
  gs4_auth(email = "cz.teamservice@gmail.com")
  url_wp_gidsinfo <- "16DrvLEXi3mEa9AbSw28YBkYpCvyO1wXwBwaNi7HpBkA"
  gd_wp_gidsinfo_slugs_raw <- read_sheet(ss = url_wp_gidsinfo, sheet = "nipperstudio_slugs")
  
  titel_slugs <- gd_wp_gidsinfo_slugs_raw %>% 
    select(starts_with("titel")) %>% 
    mutate(title_id = as.integer(titel_id)) %>% 
    filter(!is.na(title_id))
  
  redacteur_slugs <- gd_wp_gidsinfo_slugs_raw %>% 
    select(starts_with("redacteur")) %>% 
    mutate(user_id = as.integer(redacteur_id)) %>% 
    filter(!is.na(user_id))
  
  bum.3 <- bum.2 %>% 
    left_join(titel_slugs) %>% 
    left_join(redacteur_slugs) %>% 
    select(pl_name,
           user_id,
           user_name,
           user_slug = redacteur_slug,
           title_id,
           title_name = titel,
           title_slug = titel_slug,
           pl_transit)
  
  bum.3_err <- bum.3 %>% 
    filter(is.na(user_slug) | is.na(title_slug)) %>% 
    select(pl_name) %>% 
    mutate(pl_status = 3L)
  
  if (nrow(bum.3_err) > 0) {
    pl_in_err <- str_flatten(bum.3_err$pl_name, collapse = "\n")
    flog.info(paste0("Bumperregistratie van redacteur en/of pgm-titel is incompleet:\n",
                     pl_in_err, "\nZie WP-gidsinfo/nipperstudio_slugs"), name = "nsbe_log")
    bum.3 <- bum.3 %>% anti_join(bum.3_err)
  }
  
  bum.4 <- bum.3 %>% select(-user_id, -user_name, -title_id, -title_name) %>% distinct() %>% 
  # bum.4 <- bum.3 %>% select(-pl_name, -user_id, -user_name, -title_id, -title_name) %>% distinct() %>% 
    mutate(dir_transit_aan = if_else(pl_transit == "LOB", "laag/aan/", "hoog/aan/"),
           dir_transit_over = if_else(pl_transit == "LOB", "laag/over/", "hoog/over/"),
           dir_transit_af = if_else(pl_transit == "LOB", 
                                     paste0("laag/af/", user_slug),
                                     paste0("hoog/af/", user_slug)))
  
  # bumper folders ----
  bf_home <- "//uitzendmac-2/Data/Nipper/studiomontage/bumpers/"
  
  bf_bumpers <- tibble(bkey = "laag/over/", dir_transit_over_bumpers = paste0("bumper0", 1:5, "/")) %>% 
    add_row(bkey = "hoog/over/", dir_transit_over_bumpers = paste0("bumper0", 1:5, "/"))
  
  # + audio = AAN ----
  bum_aan <- bum.4 %>% select(pl_name, title_slug, dir_transit_aan) %>% 
    mutate(dir_aan = paste0(bf_home, title_slug, "/", dir_transit_aan)) %>% 
    select(pl_name, ns_dir = dir_aan)
  
  # skips silently if exists
  dir_create(bum_aan$ns_dir)
  
  # check files
  for (bdir in bum_aan$ns_dir) {
    
    n_bfiles <- length(dir_ls(bdir, type = "file"))
    
    if (n_bfiles != 1L) {
      pl_in_err <- bum_aan %>% filter(ns_dir == bdir) %>% select(pl_name)
      flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                       str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                       " voor playlist ", pl_in_err), name = "nsbe_log")
      bum.3_err <- bum.3_err %>% add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
    }
  }
  
  # + audio = AF ----
  bum_af <- bum.4 %>% select(pl_name, title_slug, dir_transit_af) %>% 
    mutate(dir_af = paste0(bf_home, title_slug, "/", dir_transit_af)) %>% 
    select(pl_name, ns_dir = dir_af)
  
  # skips silently if exists
  dir_create(bum_af$ns_dir)
  
  # check files
  for (bdir in bum_af$ns_dir) {
    
    n_bfiles <- length(dir_ls(bdir, type = "file"))
    
    if (n_bfiles != 1L) {
      pl_in_err <- bum_af %>% filter(ns_dir == bdir) %>% select(pl_name)
      flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                       str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                       " voor playlist ", pl_in_err), name = "nsbe_log")
      bum.3_err <- bum.3_err %>% add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
    }
  }
  
  # + audio = OVER ----
  bum_over <- bum.4 %>% select(pl_name, title_slug, dir_transit_over) %>% 
    left_join(bf_bumpers, by = c("dir_transit_over" = "bkey")) %>% 
    mutate(dir_over = paste0(bf_home, title_slug, "/", dir_transit_over, dir_transit_over_bumpers)) %>% 
    select(pl_name, ns_dir = dir_over)
  
  # skips silently if exists
  dir_create(bum_over$ns_dir)
  
  # check files
  for (bdir in bum_over$ns_dir) {
    
    n_bfiles <- length(dir_ls(bdir, type = "file"))
    
    if (n_bfiles != 1L) {
      pl_in_err <- bum_over %>% filter(ns_dir == bdir) %>% select(pl_name)
      flog.info(paste0("Fout: geen of meer dan 1 audiofile in ", 
                       str_replace(string = bdir, pattern = bf_home, replacement = ""), 
                       " voor playlist ", pl_in_err), name = "nsbe_log")
      bum.3_err <- bum.3_err %>% add_row(pl_name = pl_in_err$pl_name, pl_status = 3)
    }
  }
  
  # + save err-playlists ----
  bum.3_err <- bum.3_err %>% distinct()
  bum.3 <- bum.3 %>% anti_join(bum.3_err)

  # Bepaal playlist lengtes ----
  playlists.6 <- playlists.5 %>% 
    group_by(pl_id, block_order, block_id) %>% 
    mutate(blokduur_sec = sum(length)) %>% ungroup()

  for (cur_pl in bum.3$pl_name) {
    duration_rlprg <- 3600L * as.numeric(str_sub(cur_pl, 15, 17)) / 60L
    
    # + - rlprg.1 ----
    rlprg_file <- bum.3 %>% filter(pl_name == cur_pl) %>% 
      mutate(cur_duur_parm = paste0("Duration:", duration_rlprg)) %>% 
      select(cur_duur_parm) %>% 
      unite(col = regel, sep = "\t")
    
    # rlprg_file <- bind_rows(cur_duur) # , cur_tune)
    
    cur_pl_nieuw <- playlists.6 %>% filter(pl_name == cur_pl)
    
    blokken <- cur_pl_nieuw %>% distinct(block_order) %>% 
      mutate(bid = paste0("RL_BLK_", LETTERS[block_order])) %>% 
      select(vt_blok_letter = bid)

    # vt_blok_pad <- audio_locaties %>% filter(sleutel == "vt_blok", functie == "pres_blok") %>% 
    #   mutate(locatie = paste0(home_vt_audio_mac, locatie)) %>% 
    #   select(locatie) 
    
    for (blok in blokken$vt_blok_letter) {
      cur_pres <- cur_pl %>% as_tibble %>% 
        mutate(
          duur = "",
          audiofile = paste0("file://", vt_blok_pad, playlist_id, "_", match(blok, LETTERS), ".aif"),
          const_false = "FALSE",
          start_sec_sinds_middernacht = if_else(blok == "A",
                                                as.integer(cur_pl_nieuw$start) * 3600,
                                                -1), # "direct erna afspelen"
          fwdtab1 = "",
          fwdtab2 = "",
          fwdtab3 = "",
          speler_regel01 = "voicetracking",
          opname_hfd_sub = "",
          speler_regel02 = paste0("blok ", blok)
        ) %>% 
        select(-value) %>% 
        unite(col = regel, sep = "\t")
    
      cur_tracks_in_blok <- pl_tracks %>% filter(playlist == cur_pl, vt_blok_letter == blok) %>% 
        left_join(., pl_werken, by = c("playlist", "vt_blok_letter", "vt_blok_nr")) %>% 
        mutate(
          duur = "",
          audiofile = paste0("file:/", uzm_locatie),
          const_false = "FALSE",
          start_sec_sinds_middernacht = -1, # "direct erna afspelen"
          fwdtab1 = "",
          fwdtab2 = "",
          fwdtab3 = "",
          speler_regel01 = componist,
          opname_hfd_sub = paste0(cur_pl_nieuw$playlist_id[[1]], 
                                  "-", 
                                  vt_blok_letter, vt_blok_nr),
          speler_regel02 = titel
        ) %>% 
        select(duur, audiofile, const_false, start_sec_sinds_middernacht, 
               fwdtab1, fwdtab2, fwdtab3, speler_regel01, opname_hfd_sub, speler_regel02) %>% 
        unite(col = regel, sep = "\t")
      
      rlprg_file %<>% bind_rows(cur_pres, cur_tracks_in_blok)
    }

    cur_pl %<>% str_replace_all(pattern = "[.]", replacement = "-")
    
    # zet de playlist in de programs-map van RL
    home_radiologik_playlists <- paste0(home_prop("home_radiologik_win"), "Programs/")
    rlprg_file_name <- paste0(home_radiologik_playlists, cur_pl, ".rlprg")
    write.table(x = rlprg_file, file = rlprg_file_name, row.names = FALSE, col.names = FALSE, 
                sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
    
    flog.info("RL-program toegevoegd: %s", rlprg_file_name, name = "nsbe_log")
    
    # RL-scheduler samenstellen ----
    build_rl_script(cur_pl)
    
    flog.info("RL-schedulerscript toegevoegd voor %s", cur_pl, name = "nsbe_log")
  }
  
  # Herstart RL-scheduler ----
  # switch <- read_lines(file = switch_home)
  # switch <- "start RL-scheduler"
  # write_lines(switch, file = switch_home, append = FALSE)
  # flog.info("RL-scheduler draait weer", name = "nsbe_log")
  
  # gids samenstellen ----
  flog.info("Gids bijwerken...", name = "nsbe_log")
  source("src/update_gids.R", encoding = "UTF-8")
}

flog.info("= = = = = NipperStudio stop = = = = =", name = "nsbe_log")
