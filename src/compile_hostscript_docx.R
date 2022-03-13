library(officer)
library(magrittr)

np_sec2hms <- function(duur_sec) {
  result <- paste0("00:00:", duur_sec) %>% 
    hms(roll = TRUE) 
  result <- sprintf("%02d:%02d:%02d", result@hour, result@minute, result@.Data)
}

draaiboek <- pl_werken %>%
  arrange(playlist, vt_blok_letter, vt_blok_nr)

drbk <- pl_werken %>%
  group_by(playlist, vt_blok_letter) %>% 
  summarise(bloklengte = sum(lengte_sec)) %>% 
  mutate(bloklengte = bloklengte + 40, # per blok 40 seconden presentatie
         cum_lengte = cumsum(as.integer(bloklengte)))

drbk$cum_lengte <- np_sec2hms(drbk$cum_lengte)
drbk$bloklengte_hms <- np_sec2hms(drbk$bloklengte)

distinct_playlists <- unique(pl_werken$playlist)
drb_template <- "C:/cz_salsa/np_template1.docx"

for (d_pls in distinct_playlists) {
  
  ### TEST
  # d_pls <- "20220216_wo08.060_een_vroege_wandeling"
  ### TEST
  
  drb_naam <- paste0(d_pls, ".docx")

  drb <- draaiboek %>% dplyr::filter(playlist == d_pls) %>% 
    left_join(drbk, by = c("playlist", "vt_blok_letter")) %>% 
    left_join(pl_nieuw, by = c("playlist"))
  
  drb_doc <- read_docx(drb_template) %>% body_add_par(d_pls, style = "drb_titel")
  
  drb_blokken <- drb %>% 
    select(vt_blok_letter, bloklengte_hms, cum_lengte, playlist_id) %>% distinct
  
  for (p1 in 1:nrow(drb_blokken)) {
    # Blok A <lengte> <lengte_cum>
    alinea <- sprintf("Blok %s: %s (eindigt %s)", 
                      drb_blokken$vt_blok_letter[p1], 
                      drb_blokken$bloklengte_hms[p1],
                      drb_blokken$cum_lengte[p1])
    drb_doc %>% body_add_par(alinea, style = "drb_blok") 
    
    # voicetrack audiofile
    alinea <- sprintf("VoiceTrack: ../Nipper/studiomontage/vt_clips/%s_%s.aif", 
                      drb_blokken$playlist_id[p1],
                      match(drb_blokken$vt_blok_letter[p1], LETTERS))
    drb_doc %>% body_add_par(alinea, style = "drb_vt_value")
    
    # tracks dit blok
    # drb_tracks <- drb %>% filter(vt_blok_letter == "B")
    drb_tracks <- drb %>% dplyr::filter(vt_blok_letter == drb_blokken$vt_blok_letter[p1]) 
    n_tracks <- nrow(drb_tracks)
    
    for (p2 in 1:nrow(drb_tracks)) {
      alinea <- sprintf("track %s (%s)", 
                        drb_tracks$vt_blok_nr[p2],
                        drb_tracks$lengte_hms[p2])
      drb_doc %>% body_add_par(alinea, style = "drb_track_hdr")
      
      drb_doc %>% body_add_par(drb_tracks$titel[p2], style = "drb_alinea")
      
      alinea <- sprintf("%s",
                        drb_tracks$componist[p2])
      drb_doc %>% body_add_par(alinea, style = "drb_alinea")
      
      drb_doc %>% body_add_par(drb_tracks$uitvoerenden[p2], style = "drb_alinea")
    }
  }
  
  # voicetrack audiofile slot
  alinea <- sprintf("VoiceTrack: ../Nipper/studiomontage/vt_clips/%s_%s.aif", 
                    drb_blokken$playlist_id[p1],
                    nrow(drb_blokken) + 1)
  
  suppressMessages(drb_doc %>% body_add_par("Blok - slot", style = "drb_blok") %>% 
    body_add_par(alinea, style = "drb_vt_value"))
  
  # draaiboek exporteren
  drb_doc %>% cursor_begin() %>% body_remove() %>%
    print(target = paste0(config$home_draaiboeken, drb_naam))
    # print(target = paste0("resources/draaiboeken/", drb_naam))
  
  flog.info("Draaiboek toegevoegd: %s", drb_naam, name = "nipperlog")
}
