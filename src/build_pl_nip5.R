
pl_nip5 <- opnameNrs_per_playlist %>% filter(playlist_id == "NIP0005") %>% select(opnameNr)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees tracks van geselecteerde werken ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
pl_nip5_tracks <- left_join(pl_nip5, nipper_track, by = "opnameNr")

pl_nip5_werken <- left_join(pl_nip5, nipper_werk, by = "opnameNr")

library(RCurl)
pl_nip5_tracks %<>% 
  mutate(uzm_locatie_enc = curlEscape(uzm_locatie)) %>% 
  mutate(uzm_locatie_enc = str_replace_all(uzm_locatie_enc, pattern = "\\%2F", replacement = "/"))
