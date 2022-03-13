# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-playlists, -schedules, draaiboeken en gidsvermeldingen..
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
suppressWarnings(suppressPackageStartupMessages(library(knitr)))
suppressWarnings(suppressPackageStartupMessages(library(rmarkdown)))
suppressWarnings(suppressPackageStartupMessages(library(RCurl)))
suppressWarnings(suppressPackageStartupMessages(library(readr)))
suppressWarnings(suppressPackageStartupMessages(library(futile.logger)))
suppressWarnings(suppressPackageStartupMessages(library(DBI)))
suppressWarnings(suppressPackageStartupMessages(library(officer)))
suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(keyring)))
suppressWarnings(suppressPackageStartupMessages(library(googlesheets4)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))
suppressWarnings(suppressPackageStartupMessages(library(magrittr)))
suppressWarnings(suppressPackageStartupMessages(library(hms)))
suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(zip)))
suppressWarnings(suppressPackageStartupMessages(library(stringr)))
# suppressWarnings(suppressPackageStartupMessages(library(RMySQL)))


config <- read_yaml("config_nip_nxt.yaml")


filter <- dplyr::filter # voorkom verwarring met stats::filter

home_prop <- function(prop) {
  prop_name <- paste0(prop, ".", host)
  prop <- config[[prop_name]] %>% 
    curlEscape %>% 
    str_replace_all(pattern = "\\%2F", replacement = "/")
}

fa <- flog.appender(appender.file("c:/Users/gergiev/Logs/nipper.log"), name = "nipperlog")
flog.info("= = = = = NipperNext start = = = = =", name = "nipperlog")

source(config$toolbox, encoding = "UTF-8")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
host <- config$host
home_vt_audio_mac <- home_prop("home_vt_audio_mac")
home_vt_audio_win  <- home_prop("home_vt_audio_win") %>% 
  str_replace_all(pattern = "%20", replacement = " ")
home_radiologik <- home_prop("home_radiologik")
home_fonotheek <- home_prop("home_fonotheek")
switch_home <- paste0(home_prop("home_schedulerswitch"), "/nipper_msg.txt")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Nipper Next spreadsheet op GD openen, na aanmelden bij Google
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
gs4_auth(email = "cz.teamservice@gmail.com")

gd_nip_nxt_pl <- read_sheet(ss = config$url_nipper_next, sheet = "playlists")
gd_nip_nxt_sel_raw <- read_sheet(ss = config$url_nipper_next, sheet = "nipper-select")
gd_nip_nxt_sel <- gd_nip_nxt_sel_raw %>% 
  filter(!is.na(lengte)) %>% 
  mutate(lengte_sec = 3600 * hour(lengte) + 60 * minute(lengte) + second(lengte),
         lengte_hms = as_hms(lengte)) %>% 
  select(-lengte)
gd_nip_nxt_muw <- read_sheet(ss = config$url_nipper_next, sheet = "muziekweb")

for (seg1 in 1:1) { # zorgt voor een script-segment dat met "break" verlaten kan worden

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Kijk in werkblad "playlists" welke nieuwe playlists er moeten komen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_nieuw <- gd_nip_nxt_pl %>% filter(gereed == T & afgeleverd_op == "NULL")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de werken op 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_werken <- gd_nip_nxt_sel %>% 
    filter(playlist %in% pl_nieuw$playlist) %>% 
    filter(keuze == T) %>% 
    # splits de voice-tracking blokken in letter en volgnummer, om bij sorteren te verhinderen 
    # dat blok A10 meteen na blok A1 komt
    mutate(vt_blok_letter = str_sub(vt_blok, start = 1, end = 1), 
           vt_blok_nr = as.integer(str_sub(vt_blok, start = 2))) %>% 
    # select(-tot_time, -op_huidige_pl, -keuze, -vt_blok) %>% 
    select(-tot_time, -keuze, -vt_blok) %>% 
    arrange(playlist, vt_blok_letter, vt_blok_nr)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken waar ook echt wat in staat
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_nieuw.1 <- pl_nieuw %>% filter(playlist %in% pl_werken$playlist) %>% 
    select(playlist_id, playlist, programma, start, anchor)
  
  if(nrow(pl_nieuw.1) == 0){
    flog.info("Er zijn geen nieuwe playlists", name = "nipperlog")
    break
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Alleen playlists maken als alle blokken uniek genummerd zijn en er geen ontbreekt
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  dubbele_blokken <- pl_werken %>% 
    group_by(playlist, vt_blok_letter, vt_blok_nr) %>% 
    summarise(n_dubbel = n()) %>% 
    filter(n_dubbel > 1) %>% select(-n_dubbel)
  
  if(nrow(dubbele_blokken) > 0) {
    err_blokken <- unite(data = dubbele_blokken, col = regel, sep = " ")
    flog.info("Sommige blokken zijn dubbel benoemd: %s\ngeen playlists etc. gemaakt.", 
              err_blokken, name = "nipperlog")
    break
  }
  
  ontbrekende_blokken <-
    pl_werken %>% select(playlist, vt_blok_letter, vt_blok_nr) %>%
    group_by(playlist, vt_blok_letter) %>%
    summarise(
      grp_count = n(),
      grp_min = min(vt_blok_nr),
      grp_max = max(vt_blok_nr)
    ) %>%
    filter(grp_max != grp_count | grp_min != 1)
  
  if(nrow(ontbrekende_blokken) > 0) {
    err_blokken <- unite(data = ontbrekende_blokken, col = regel, sep = " ")
    flog.info("Sommige blokken ontbreken: %s\ngeen playlists etc. gemaakt.", 
              err_blokken, name = "nipperlog")
    break
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Bepaal de playlist lengtes
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_duur <- pl_werken %>% 
    group_by(playlist, vt_blok_letter) %>% 
    summarise(blokduur_sec = sum(lengte_sec)) %>% 
    group_by(playlist) %>% 
    summarise(blokken = n(),
              muzieklengte = sum(blokduur_sec)) %>% 
    # 'speling': 00:50 tune+uitzending_aan/af
    #            00:30 minimum aanvulling, 
    #            05:00 maximum aanvulling, 
    #            00:40 presentatie per blok af+aan
    mutate(speling_min = 50 +  30 + 40 * blokken, 
           speling_max = 50 + 300 + 40 * blokken,
           slotlengte = 60 * as.integer(str_sub(playlist, start = 15, end = 17)),
           muziek_min = slotlengte - speling_max,
           muziek_max = slotlengte - speling_min,
           vulling = case_when(muzieklengte > muziek_max ~ paste0("rood (+", muzieklengte - muziek_max, "s)"),
                               muzieklengte > muziek_min ~ "groen",
                               TRUE                      ~ paste0("geel (-", muziek_min - muzieklengte, "s)")
           )
    )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de tracks op
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  df_albums_and_tracks_file <- "C:/Users/gergiev/cz_rds_store/df_albums_and_tracks_all.RDS"
  df_albums_and_tracks_all <- read_rds(df_albums_and_tracks_file)
  
  # df_albums_and_tracks.1 <- gd_albums_and_tracks(pl_nieuw) 
  
  # df_albums_and_tracks.2 <- df_albums_and_tracks.1 %>% 
  # df_albums_and_tracks.2 <- pl_werken %>% 
  #   select(playlist, opnameNr) %>% inner_join(df_albums_and_tracks_all, 
  #                                             by = c("opnameNr" = "album_key")) %>% 
  #   select(playlist, muw_album_id, muw_track)
  
  # nipper_tracks <- readRDS("resources/nipper_tracks.rds") %>% distinct()
  
  pl_tracks <- pl_werken %>% 
    select(playlist, vt_blok_letter, vt_blok_nr, opnameNr) %>% 
    left_join(df_albums_and_tracks_all, by =  c("opnameNr" = "album_key")) %>% 
    mutate(uzm_locatie = paste0("//Volumes/Avonden/Nipper/muziekweb_audio/",
                                muw_album_id,
                                "-",
                                str_pad(muw_track, width = 4, side = "left", pad = "0"),
                                ".wav"))

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # RL-programs samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  programma_sleutels <- read_sheet(ss = config$url_nipper_next, sheet = "programma_sleutels")
  
  audio_locaties <- read_sheet(ss = config$url_nipper_next, sheet = "audio_locaties")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Stop RL-scheduler op de mac en wacht 5 seconden - stoppen duurt soms even
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  flog.info("RL-scheduler stoppen", name = "nipperlog")
  switch <- read_lines(file = switch_home)
  switch <- "stop RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  
  Sys.sleep(time = 5)
  flog.info("RL-scheduler is gestopt", name = "nipperlog")
  
  source("src/compile_schedulerscript.R", encoding = "UTF-8") # bevat alleen functies
  source("src/shared_functions.R", encoding = "UTF-8") # bevat alleen functies
  
  for (cur_pl in pl_nieuw$playlist) {
    ### TEST
    # cur_pl <- "20220328_ma07.180_ochtendeditie"
    ### TEST
    duration_rlprg <- 3600L * as.numeric(str_sub(cur_pl, 15, 17)) / 60L
    
    cur_duur <- pl_duur %>% filter(playlist == cur_pl) %>% 
      mutate(cur_duur_parm = paste0("Duration:", duration_rlprg)) %>% 
      select(cur_duur_parm) %>% 
      unite(col = regel, sep = "\t")
    
    cur_pl_nieuw <- pl_nieuw %>% filter(playlist == cur_pl)

    rlprg_file <- bind_rows(cur_duur) # , cur_tune)
    
    blokken <- pl_tracks %>% filter(playlist == cur_pl) %>% distinct(vt_blok_letter) 
    slot_letter <- LETTERS[1 + nrow(blokken)]
    slot <- slot_letter %>% as_tibble %>% setNames("vt_blok_letter")
    blokken %<>% bind_rows(slot) 
    
    playlist_id <- pl_nieuw %>% filter(playlist == cur_pl) %>% select(playlist_id) %>% slice(1)
    
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
    home_radiologik_playlists <- paste0(home_prop("home_radiologik"), "Programs/")
    rlprg_file_name <- paste0(home_radiologik_playlists, cur_pl, ".rlprg")
    write.table(x = rlprg_file, file = rlprg_file_name, row.names = FALSE, col.names = FALSE, 
                sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
    
    flog.info("RL-program toegevoegd: %s", rlprg_file_name, name = "nipperlog")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # scheduler-script samenstellen
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    build_rl_script(cur_pl)
    
    flog.info("RL-schedulerscript toegevoegd voor %s", cur_pl, name = "nipperlog")
    
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
      flog.info("RL-schedulerscript toegevoegd voor OE-herh: %s", dummy_pl, name = "nipperlog")
    }
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Herstart RL-scheduler op de mac, zodat de nieuwe scripts ingelezen worden
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  switch <- read_lines(file = switch_home)
  switch <- "start RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  flog.info("RL-scheduler draait weer", name = "nipperlog")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Draaiboeken en gids samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  flog.info("Draaiboeken maken...", name = "nipperlog")
  source("src/compile_hostscript_docx.R", encoding = "UTF-8")  
  flog.info("Gids bijwerken...", name = "nipperlog")
  source("src/update_gids.R", encoding = "UTF-8")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Muziekweb-audio verplaatsen en uitpakken
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  flog.info("Muziekweb-audio verplaatsen en uitpakken...", name = "nipperlog")
  muw_zips <- dir_ls(path = "C:/Users/gergiev/Downloads/", 
                     recurse = F, 
                     regexp = "^Bestelling#.+\\.zip$") %>% as_tibble() %>% rename(zip_name = value)
  
  if (nrow(muw_zips) == 0) {
    flog.info("Geen nieuwe Muziekweb-zips aangetroffen.", name = "nipperlog")
  } else {
    for (a_zip in muw_zips$zip_name) {
      ### TEST ###
      # a_zip = "C:/Users/gergiev/Downloads/Bestelling#1562-D2CA3BF6.zip"
      ### TEST ###
      flog.info("Verplaats nieuwe Muziekweb-zip naar UZM en pak uit: %s",
                a_zip,
                name = "nipperlog")
      file_move(a_zip, "u:/Nipper/muziekweb_audio/")
      uzm_zip <-
        str_replace(a_zip, pattern = "C:/Users/gergiev/Downloads/", "U:/Nipper/muziekweb_audio/")
      uzm_zip_done <- paste0(uzm_zip, ".done")
      unzip(uzm_zip, exdir = "U:/Nipper/muziekweb_audio")
      file_move(uzm_zip, uzm_zip_done)
    }
  }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Meld de nieuwe spullenboel als "afgeleverd".
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### TEST
  # gd_nip_nxt_pl_tst <- read_sheet(ss = "1opszI9cZi-vLnNp-0vcv2mfzX7Pv9q80nfZYcaVtq2U", sheet = "playlists")
  ### TEST
  df_afgeleverd_op <- gd_nip_nxt_pl %>%  
    filter(str_detect(playlist_id, "NN")) %>% 
    select(playlist_id, gereed, afgeleverd_op)
  
  df_afgeleverd_op$afgeleverd_op <- na_if(df_afgeleverd_op$afgeleverd_op, "NULL")

  df_afgeleverd_op.1 <- df_afgeleverd_op %>%
    mutate(afgeleverd_op_upd = if_else(
      gereed == T &
        is.na(afgeleverd_op),
      now(tzone = "Europe/Amsterdam") + hours(1),
      as_datetime(unlist(afgeleverd_op), origin = "1970-01-01")
    )) %>% select(afgeleverd_op_upd)

  range_write(ss = config$url_nipper_next,
              data = df_afgeleverd_op.1,
              sheet = "playlists",
              range = "S3",
              col_names = F,
              reformat = F)
  
}

flog.info("= = = = = NipperNext stop = = = = =", name = "nipperlog")
