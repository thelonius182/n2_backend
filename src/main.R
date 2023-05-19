# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-playlists, -schedules, draaiboeken en gidsvermeldingen..
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
pacman::p_load(knitr, rmarkdown, RCurl, readr, futile.logger, DBI, officer, httr,
               xml2, tidyverse, keyring, googlesheets4, yaml, fs, magrittr, hms,
               lubridate, zip, stringr)

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

fa <- flog.appender(appender.file("c:/cz_salsa/Logs/nipperstudio_backend.log"), name = "nsbe_log")
flog.info("
= = = = = NipperStudio start = = = = =", name = "nsbe_log")

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

if (nrow(playlists.4) == 0) {
  flog.info("Geen nieuwe playlists gevonden", name = "nsbw_log")
  dbDisconnect(ns_con)
  stopifnot("Geen nieuwe playlists gevonden" = nrow(playlists.4) > 0)

} else {
  pl_log_names <- str_flatten(playlists.4$pl_name, collapse = "\n")
  pl_log_posts <- str_flatten(playlists.4$post_id, collapse = ", ")
  flog.info(paste0("Aangeboden playlists:\n", pl_log_names), name = "nsbe_log")
  flog.info(paste0("posts = ", pl_log_posts), name = "nsbe_log")
}

# check BUM audio ----
# + get editors ----
# editors worden afgekondigd na laatste bumperblok
bum.1 <- playlists.5 %>% 
  filter(pl_transit %in% c("LOB", "HIB")) %>% 
  select(pl_name, user_id, title_id, pl_transit) %>% distinct()

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
  
  bum.4 <- bum.3 %>% select(-pl_name, -user_id, -user_name, -title_id, -title_name) %>% distinct() %>% 
    mutate(dir_transit = if_else(pl_transit == "LOB", 
                                 paste0("laag/af/", user_slug),
                                 paste0("hoog/af/", user_slug)))
  
  # bumper folders ----
  bf_home <- bum.4 %>% select(title_slug) %>% distinct() %>% 
    mutate(ns_dir_name = paste0("//uitzendmac-2/Data/Nipper/studiomontage/bumpers/", 
                                title_slug,
                                "/")) %>% select(-title_slug)
  
  bf_hib_aan <- paste0(bf_home$ns_dir_name, "hoog/aan/")
  bf_hib_af <- paste0(bf_home$ns_dir_name, 
                      bum.4 %>% 
                        filter(str_detect(dir_transit, "hoog")) %>% 
                        select(dir_transit))
  bf_hib_over <- paste0(bf_home$ns_dir_name, "hoog/over/")
  
  
}


# gd_nip_nxt_pl <- read_sheet(ss = config$url_nipper_next, sheet = "playlists")
# gd_nip_nxt_sel_raw <- read_sheet(ss = config$url_nipper_next, sheet = "nipper-select")
# gd_nip_nxt_sel <- gd_nip_nxt_sel_raw %>% 
#   filter(!is.na(lengte)) %>% 
#   mutate(lengte_sec = 3600 * hour(lengte) + 60 * minute(lengte) + second(lengte),
#          lengte_hms = as_hms(lengte)) %>% 
#   select(-lengte)
# gd_nip_nxt_muw <- read_sheet(ss = config$url_nipper_next, sheet = "muziekweb")

for (seg1 in 1:1) { # zorgt voor een script-segment dat met "break" verlaten kan worden

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Kijk in werkblad "playlists" welke nieuwe playlists er moeten komen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # pl_nieuw <- gd_nip_nxt_pl %>% filter(gereed == T & afgeleverd_op == "NULL")
  
  ### TEST
  # pl_nieuw <- gd_nip_nxt_pl %>% filter(anchor == "M17")
  # gd_nip_nxt_pl_tst <- read_sheet(ss = "1opszI9cZi-vLnNp-0vcv2mfzX7Pv9q80nfZYcaVtq2U", sheet = "playlists")
  ### TEST
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de werken op 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # pl_werken <- gd_nip_nxt_sel %>% 
  #   filter(playlist %in% pl_nieuw$playlist) %>% 
  #   filter(keuze == T) %>% 
  #   # splits de voice-tracking blokken in letter en volgnummer, om bij sorteren te verhinderen 
  #   # dat blok A10 meteen na blok A1 komt
  #   mutate(vt_blok_letter = str_sub(vt_blok, start = 1, end = 1), 
  #          vt_blok_nr = as.integer(str_sub(vt_blok, start = 2))) %>% 
  #   # select(-tot_time, -op_huidige_pl, -keuze, -vt_blok) %>% 
  #   select(-tot_time, -keuze, -vt_blok) %>% 
  #   arrange(playlist, vt_blok_letter, vt_blok_nr)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken waar ook echt wat in staat
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # pl_nieuw.1 <- pl_nieuw %>% filter(playlist %in% pl_werken$playlist) %>% 
  #   select(playlist_id, playlist, programma, start, anchor)
  # 
  # if (nrow(pl_nieuw.1) == 0) {
  #   break
  # }
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Meld de nieuwe spullenboel als "afgeleverd".
  # Dat lijkt te vroeg, maar is gedaan om te voorkomen dat een playlist meer dan eens wordt opgepakt
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # df_afgeleverd_op <- gd_nip_nxt_pl %>%  
  #   filter(str_detect(playlist_id, "NN")) %>% 
  #   select(playlist_id, gereed, afgeleverd_op)
  # 
  # df_afgeleverd_op$afgeleverd_op <- na_if(df_afgeleverd_op$afgeleverd_op, "NULL")
  # 
  # df_afgeleverd_op.1 <- df_afgeleverd_op %>%
  #   mutate(afgeleverd_op_upd = if_else(
  #     gereed == T &
  #       is.na(afgeleverd_op),
  #     now(tzone = "Europe/Amsterdam") + hours(1),
  #     as_datetime(unlist(afgeleverd_op), origin = "1970-01-01")
  #   )) %>% select(afgeleverd_op_upd)
  # 
  # range_write(ss = config$url_nipper_next,
  #             data = df_afgeleverd_op.1,
  #             sheet = "playlists",
  #             range = "S3",
  #             col_names = F,
  #             reformat = F)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken als alle blokken uniek genummerd zijn en er geen ontbreekt
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # dubbele_blokken <- pl_werken %>% 
  #   group_by(playlist, vt_blok_letter, vt_blok_nr) %>% 
  #   summarise(n_dubbel = n()) %>% 
  #   filter(n_dubbel > 1) %>% select(-n_dubbel)
  # 
  # if(nrow(dubbele_blokken) > 0) {
  #   err_blokken <- unite(data = dubbele_blokken, col = regel, sep = " ")
  #   flog.info("Sommige blokken zijn dubbel benoemd: %s\ngeen playlists etc. gemaakt.", 
  #             err_blokken, name = "nsbe_log")
  #   break
  # }
  # 
  # ontbrekende_blokken <-
  #   pl_werken %>% select(playlist, vt_blok_letter, vt_blok_nr) %>%
  #   group_by(playlist, vt_blok_letter) %>%
  #   summarise(
  #     grp_count = n(),
  #     grp_min = min(vt_blok_nr),
  #     grp_max = max(vt_blok_nr)
  #   ) %>%
  #   filter(grp_max != grp_count | grp_min != 1)
  # 
  # if(nrow(ontbrekende_blokken) > 0) {
  #   err_blokken <- unite(data = ontbrekende_blokken, col = regel, sep = " ")
  #   flog.info("Sommige blokken ontbreken: %s\ngeen playlists etc. gemaakt.", 
  #             err_blokken, name = "nsbe_log")
  #   break
  # }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Bepaal playlist lengtes ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  playlists.6 <- playlists.5 %>% 
    group_by(pl_id, block_order, block_id) %>% 
    mutate(blokduur_sec = sum(length)) %>% ungroup()
    # group_by(pl_name) %>% 
    # summarise(blokken = n(),
    #           muzieklengte = sum(blokduur_sec)) %>% 
    # # 'speling': 00:50 tune+uitzending_aan/af
    # #            00:30 minimum aanvulling, 
    # #            05:00 maximum aanvulling, 
    # #            00:40 presentatie per blok af+aan
    # mutate(speling_min = 50 +  30 + 40 * blokken, 
    #        speling_max = 50 + 300 + 40 * blokken,
    #        slotlengte = 60 * as.integer(str_sub(playlist, start = 15, end = 17)),
    #        muziek_min = slotlengte - speling_max,
    #        muziek_max = slotlengte - speling_min,
    #        vulling = case_when(muzieklengte > muziek_max ~ paste0("rood (+", muzieklengte - muziek_max, "s)"),
    #                            muzieklengte > muziek_min ~ "groen",
    #                            TRUE                      ~ paste0("geel (-", muziek_min - muzieklengte, "s)")
    #        )
    # )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de tracks op
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # df_albums_and_tracks_file <- "C:/Users/gergiev/cz_rds_store/df_albums_and_tracks_all.RDS"
  # df_albums_and_tracks_all <- read_rds(df_albums_and_tracks_file)
  
  # df_albums_and_tracks.1 <- gd_albums_and_tracks(pl_nieuw) 
  
  # df_albums_and_tracks.2 <- df_albums_and_tracks.1 %>% 
  # df_albums_and_tracks.2 <- pl_werken %>% 
  #   select(playlist, opnameNr) %>% inner_join(df_albums_and_tracks_all, 
  #                                             by = c("opnameNr" = "album_key")) %>% 
  #   select(playlist, muw_album_id, muw_track)
  
  # nipper_tracks <- readRDS("resources/nipper_tracks.rds") %>% distinct()

  # koppel muw-audio ----
  n_cols <- str_count(playlists.6$recording_no, ",") %>% max() + 1
  # muw_wavs.1 <-
  playlists.7 <- separate(data = playlists.6,
                          col = recording_no,
                          into = paste0("muw", 1:n_cols),
                          sep = ", ",
                          fill = "right") %>% 
    pivot_longer(cols = starts_with("muw"), 
                 names_to = "muw_grp", 
                 values_to = "muw_id", 
                 values_drop_na = T) %>% 
    mutate(uzm_locatie = paste0("//Volumes/Data/Nipper/muziekweb_audio/", muw_id, ".wav"))

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # RL-programs samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  programma_sleutels <- read_sheet(ss = config$url_nipper_next, sheet = "programma_sleutels")
  
  audio_locaties <- read_sheet(ss = config$url_nipper_next, sheet = "audio_locaties")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Stop RL-scheduler op de mac en wacht 5 seconden - stoppen duurt soms even
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  flog.info("RL-scheduler stoppen", name = "nsbe_log")
  switch <- read_lines(file = switch_home)
  switch <- "stop RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  
  Sys.sleep(time = 5)
  flog.info("RL-scheduler is gestopt", name = "nsbe_log")
  
  for (cur_pl in playlists.4) {
    duration_rlprg <- 3600L * as.numeric(str_sub(cur_pl, 15, 17)) / 60L
    
    cur_duur <- playlists.4 %>% filter(pl_name == cur_pl) %>% 
      mutate(cur_duur_parm = paste0("Duration:", duration_rlprg)) %>% 
      select(cur_duur_parm) %>% 
      unite(col = regel, sep = "\t")
    
    rlprg_file <- bind_rows(cur_duur) # , cur_tune)
    
    cur_pl_nieuw <- playlists.7 %>% filter(pl_name == cur_pl)
    
    blokken <- cur_pl_nieuw %>% distinct(block_order) %>% 
      mutate(bid = paste0("BLK_", LETTERS[block_order])) %>% 
      select(vt_blok_letter = bid)
    slot_letter <- paste0("BLK_", LETTERS[1 + nrow(blokken)])
    slot <- slot_letter %>% as_tibble %>% setNames("vt_blok_letter")
    blokken %<>% bind_rows(slot) 
    
    playlist_id <- playlists.4 %>% filter(pl_name == cur_pl) %>% 
      mutate(playlist_id = paste0("NS", post_id)) %>% select(playlist_id) 
    
    vt_blok_pad <- audio_locaties %>% filter(sleutel == "vt_blok", functie == "pres_blok") %>% 
      mutate(locatie = paste0(home_vt_audio_mac, locatie)) %>% 
      select(locatie) 

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
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # scheduler-script samenstellen
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    build_rl_script(cur_pl)
    
    flog.info("RL-schedulerscript toegevoegd voor %s", cur_pl, name = "nsbe_log")
    
    #+ RL-scripts to play recycled OE's ----
    # For this, 'build_rl_script' needs 2 arguments: a playlist A, having the
    # required play date; and a playlist B, having the OE-episode to be
    # recycled. Playlist A will be artificial, as there is no GD-selection for
    # it.
    
    for (seg_oe_a in 1:1) {
      # !TEST! # cur_pl <- "20200106_ma07-180_ochtendeditie"
      
      if (!str_detect(string = cur_pl, pattern = "_ochtendeditie")) {
        break
      }
      
      #+ . set dummy pl_name ----
      # required play date is cur_pl_date + 7 days.
      cur_pl_date <- playlist2postdate(cur_pl)
      stamped_format <- stamp("20191229_zo", orders = "%Y%0m%d_%a")
      dummy_pl <- paste0(stamped_format(cur_pl_date + days(7L)),
                         "07-180_ochtendeditie")
      
      #+ . set OE-episode pl_name ----
      # details are on GD: kringloopherhalingen ochtendeditie
      oe_offset <-
        case_when(
          cur_pl_date >= ymd_hms("2020-06-22 07:00:00", tz = "Europe/Amsterdam") ~ 175L,
          str_detect(string = cur_pl, pattern = "_(ma|di|wo|do)\\d") ~ 175L,
          TRUE                                                       ~ 182L
        )
      
      oe_pl <-
        paste0(stamped_format(cur_pl_date + days(7L) - days(oe_offset)),
               "07-180_ochtendeditie")
      
      oe_pl_set <- c(dummy_pl, oe_pl)
      build_rl_script(oe_pl_set)
      flog.info("RL-schedulerscript toegevoegd voor OE-herh: %s", dummy_pl, name = "nsbe_log")
    }
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Herstart RL-scheduler op de mac, om de nieuwe scripts te uploaden
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  switch <- read_lines(file = switch_home)
  switch <- "start RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  flog.info("RL-scheduler draait weer", name = "nsbe_log")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Draaiboeken en gids samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  flog.info("Draaiboeken maken...", name = "nsbe_log")
  source("src/compile_hostscript_docx.R", encoding = "UTF-8")  
  flog.info("Gids bijwerken...", name = "nsbe_log")
  source("src/update_gids.R", encoding = "UTF-8")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Muziekweb-audio verplaatsen en uitpakken
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # flog.info("Muziekweb-audio verplaatsen en uitpakken...", name = "nsbe_log")
  # muw_zips <- dir_ls(path = "C:/Users/gergiev/Downloads/", 
  #                    recurse = F, 
  #                    regexp = "Bestelling.+\\.zip$") %>% as_tibble() %>% rename(zip_name = value)
  # 
  # if (nrow(muw_zips) == 0) {
  #   flog.info("Geen nieuwe Muziekweb-zips aangetroffen.", name = "nsbe_log")
  # } else {
  #   for (a_zip in muw_zips$zip_name) {
  #     ### TEST ###
  #     # a_zip = "C:/Users/gergiev/Downloads/Bestelling#1562-D2CA3BF6.zip"
  #     ### TEST ###
  #     flog.info("Verplaats nieuwe Muziekweb-zip naar UZM: %s",
  #               a_zip,
  #               name = "nsbe_log")
  #     file_move(a_zip, "u:/Nipper/muziekweb_audio/")
  #     # uzm_zip <-
  #     #   str_replace(a_zip, pattern = "C:/Users/gergiev/Downloads/", "U:/Nipper/muziekweb_audio/")
  #     # uzm_zip_done <- paste0(uzm_zip, ".done")
  #     # unzip(uzm_zip, exdir = "U:/Nipper/muziekweb_audio")
  #     # file_move(uzm_zip, uzm_zip_done)
  #   }
  # }
  
  flog.info("= = = = = NipperNext stop = = = = =", name = "nsbe_log")
}

