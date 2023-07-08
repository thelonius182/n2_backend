pacman::p_load(officer, magrittr)

build_host_script <- function(arg_pl_name) {
  
  draaiboek <- playlists.6 %>% filter(pl_name == arg_pl_name) %>% arrange(block_order, track_order)
  
  blokken <- draaiboek %>% select(post_id, block_order) %>% distinct() %>% 
    mutate(vt_blok = paste0("NS", post_id, LETTERS[block_order]))
  
  draaiboek.1 <- draaiboek %>% inner_join(blokken)
  
  dbk <- draaiboek.1 %>% distinct(vt_blok, .keep_all = T) %>% 
    group_by(vt_blok) %>% 
    summarise(bloklengte = sum(blokduur_sec)) %>% 
    mutate(bloklengte = bloklengte + 40, # per blok 40 seconden presentatie
           cum_lengte = cumsum(as.integer(bloklengte)))
  
  dbk$cum_lengte <- np_sec2hms(dbk$cum_lengte)
  dbk$bloklengte_hms <- np_sec2hms(dbk$bloklengte)
  
  dbk_template <- "C:/cz_salsa/np_template1.docx"
  
  dbk_naam <- paste0(draaiboek.1$pl_name[1], ".docx")
  
  draaiboek.3 <- draaiboek.1 %>% left_join(dbk) 
  
  dbk_doc <- read_docx(dbk_template) %>% 
    body_add_par(draaiboek.1$pl_name[1], style = "drb_titel")
  
  dbk_blokken <- dbk %>% 
    select(vt_blok, bloklengte_hms, cum_lengte) %>% distinct()
  
  for (p1 in 1:nrow(dbk_blokken)) {
    # Blok A <lengte> <lengte_cum>
    alinea <- sprintf("Blok %s: %s (eindigt %s)", 
                      dbk_blokken$vt_blok[p1], 
                      dbk_blokken$bloklengte_hms[p1],
                      dbk_blokken$cum_lengte[p1])
    
    dbk_doc %>% body_add_par(alinea, style = "drb_blok") 
    
    # voicetrack audiofile
    alinea <- sprintf("VoiceTrack: <vt_clips_folder>/%s.aif", dbk_blokken$vt_blok[p1])
    dbk_doc %>% body_add_par(alinea, style = "drb_vt_value")
    
    # tracks dit blok
    # drb_tracks <- drb %>% filter(vt_blok_letter == "B")
    dbk_tracks <- draaiboek.3 %>% filter(vt_blok == dbk_blokken$vt_blok[p1]) 
    n_tracks <- nrow(dbk_tracks)
    
    for (p2 in 1:nrow(dbk_tracks)) {
      alinea <- sprintf("track %s (%s)", 
                        dbk_tracks$recording_no[p2],
                        np_sec2hms(dbk_tracks$length[p2]))
      dbk_doc %>% body_add_par(alinea, style = "drb_track_hdr")
      
      dbk_doc %>% body_add_par(dbk_tracks$titel[p2], style = "drb_alinea")
      
      alinea <- sprintf("%s", dbk_tracks$componist[p2])
      dbk_doc %>% body_add_par(alinea, style = "drb_alinea")
      
      dbk_doc %>% body_add_par(dbk_tracks$uitvoerenden[p2], style = "drb_alinea")
    }
  }
  
  # voicetrack audiofile slot
  slot_blok <-
    str_replace(dbk_blokken$vt_blok[p1], 
                "(.*)(\\w)", 
                paste0("\\1", 
                       LETTERS[match(str_sub(dbk_blokken$vt_blok[p1], -1), LETTERS) + 1])
    )
  
  alinea <- sprintf("VoiceTrack: <vt_clips_folder>/%s.aif", slot_blok)
  
  suppressMessages(dbk_doc %>% 
                     body_add_par("Blok - slot", style = "drb_blok") %>% 
                     body_add_par(alinea, style = "drb_vt_value"))
  
  # draaiboek exporteren
  dbk_doc %>% cursor_begin() %>% body_remove() %>%
    print(target = paste0(config$home_draaiboeken, dbk_naam))
  
  flog.info("Draaiboek toegevoegd: %s", dbk_naam, name = "nipperlog")
  
}