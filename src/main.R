# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-playlists, -schedules, draaiboeken en gidsvermeldingen..
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(tidyr)
library(knitr)
library(rmarkdown)
library(googlesheets)
library(RCurl)
library(yaml)
library(magrittr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(fs)
library(readr)
library(futile.logger)
library(keyring)
library(RMySQL)
library(officer)

filter <- dplyr::filter # voorkom verwarring met stats::filter

home_prop <- function(prop) {
  prop_name <- paste0(prop, ".", host)
  prop <- config[[prop_name]] %>% 
    curlEscape %>% 
    str_replace_all(pattern = "\\%2F", replacement = "/")
}

flog.appender(appender.file("/Users/nipper/Logs/nipper.log"), name = "nipperlog")
flog.info("= = = = = NIPPER start = = = = =", name = "nipperlog")

config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")

host <- config$host
home_vt_audio_mac <- home_prop("home_vt_audio_mac")
home_vt_audio_win  <- home_prop("home_vt_audio_win") %>% 
  str_replace_all(pattern = "%20", replacement = " ")
home_radiologik <- home_prop("home_radiologik")
home_fonotheek <- home_prop("home_fonotheek")
switch_home <- paste0(home_prop("home_schedulerswitch"), "/nipper_msg.txt")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Stop RL-scheduler op de mac en wacht 5 seconden - stoppen duurt soms even
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
flog.info("RL-scheduler stoppen", name = "nipperlog")
switch <- read_lines(file = switch_home)
switch <- "stop RL-scheduler"
write_lines(switch, path = switch_home, append = FALSE)

Sys.sleep(time = 5)
flog.info("RL-scheduler is gestopt", name = "nipperlog")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Nipper Express spreadsheet op GD openen
# NB!! zonodig: change to new user; er opent een browser dialogue
#               gs_auth(new_user = TRUE)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
gd_nip_xpr <- gs_title(config$nip_xpr_gd_reg)

for (seg1 in 1:1) { # zorgt voor een script-segment dat met "break" verlaten kan worden

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Kijk in werkblad "playlists" welke nieuwe playlists er moeten komen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_nieuw <- gd_nip_xpr %>% 
    gs_read(ws = "playlists") %>% 
    filter(is.na(samengesteld_op), !is.na(playlist))
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Haal de werken op 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_werken <- gd_nip_xpr %>% 
    gs_read(ws = "nipper-select") %>% 
    filter(playlist %in% pl_nieuw$playlist) %>% 
    filter(!is.na(keuze)) %>% 
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
  pl_nieuw %<>% filter(playlist %in% pl_werken$playlist) %>% 
    select(playlist_id, playlist, programma, start, anchor)
  
  if(nrow(pl_nieuw) == 0){
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
    summarise(blokduur = sum(lengte)) %>% 
    # blokduur omzetten in seconden: seconds(hms = 00:05:00) = 300S
    #                                as.integer(300S) = 300
    mutate(blokduur_sec = as.integer(seconds(blokduur))) %>% 
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
  nipper_tracks <- readRDS("resources/nipper_tracks.rds") %>% distinct
  
  pl_tracks <- pl_werken %>% 
    select(playlist, vt_blok_letter, vt_blok_nr, opnameNr) %>% 
    left_join(., nipper_tracks, by = "opnameNr") %>% 
    mutate(uzm_locatie = curlEscape(uzm_locatie),
           uzm_locatie = paste0(home_fonotheek, uzm_locatie),
           uzm_locatie = str_replace_all(uzm_locatie, pattern = "\\%2F", replacement = "/"))
           
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # RL-playlist samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  programma_sleutels <- gd_nip_xpr %>% gs_read(ws = "programma_sleutels")
  
  audio_locaties <- gd_nip_xpr %>% gs_read(ws = "audio_locaties")

  source("src/compile_schedulerscript.R", encoding = "UTF-8")
  
  for (cur_pl in pl_nieuw$playlist) {
    # cur_pl <- "20190215_vr07.180_ochtendeditie"
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
          audiofile = paste0("file://", uzm_locatie),
          const_false = "FALSE",
          start_sec_sinds_middernacht = -1, # "direct erna afspelen"
          fwdtab1 = "",
          fwdtab2 = "",
          fwdtab3 = "",
          speler_regel01 = componist_lbl,
          opname_hfd_sub = paste0(opnameNr.x, "-", opnameVlgNr),
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
    
    flog.info("RL-playlist toegevoegd: %s", rlprg_file_name, name = "nipperlog")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # scheduler-script samenstellen
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    build_rl_script(cur_pl)
    
    flog.info("RL-schedulerscript toegevoegd voor %s", cur_pl, name = "nipperlog")
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Draaiboeken en gids samenstellen
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  source("src/compile_hostscript_docx.R", encoding = "UTF-8")  
  source("src/update_gids.R", encoding = "UTF-8")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Nieuwe playlists afstempelen met aanmaakdatum. gs_edit_cells zou dat in 1 keer kunnen doen, mits de
  # rijen 1 aaneengesloten blok vormen - wat niet per se zo is bij nieuwe playlists in het GD-tabblad
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  pl_anchor <- pl_nieuw %>% select(anchor)
  samengesteld_op <- today(tzone = "Europe/Amsterdam")
  
  for (a1 in 1:nrow(pl_nieuw)) {
    cur_anchor <- pl_anchor[a1, ] %>% as.character
    gs_edit_cells(gd_nip_xpr, ws = "playlists", anchor = cur_anchor, input = samengesteld_op)
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Start RL-scheduler op de mac, zodat de nieuwe scripts ingelezen worden
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
switch <- read_lines(file = switch_home)
switch <- "start RL-scheduler"
write_lines(switch, path = switch_home, append = FALSE)
flog.info("RL-scheduler draait weer", name = "nipperlog")

flog.info("= = = = = NIPPER stop = = = = =", name = "nipperlog")
