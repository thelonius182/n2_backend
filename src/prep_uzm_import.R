# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Geïmporteerde track-info van de audiofiles op de uitzendmac schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees track-info Uitzendmac ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo <- read_delim("resources/archief01_tracks.txt",
                           "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           col_names = c("track", "dir", "lengte_uzm")
                           ) %>% 
  arrange(dir, track)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Skip audiofiles in dir Wereldmuziek ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  filter(!str_detect(dir, pattern = "/.*WERELDMUZIEKARCHIEF/"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Splits dir: catalogusNr (+ disk_volgNr) + ¶ + album ----
# Skip track-info's waarbij dat niet lukt.
# 
# Bv. "/Volumes/cz archief 01/C01143-2 L_Oeuvre Integral Pour Piano (Setrak) 2" 
#     dir_splits1 = "1143-2¶L_Oeuvre Integral Pour Piano (Setrak) 2"
#     dir_splits2 = "1143-2", album = "L_Oeuvre Integral Pour Piano (Setrak) 2"
#     vervolgens: catNr = "1143", diskNr_dir = "2" 
#     NB - er zitten ook diskNr's in album en track, vandaar hier de sfx "_dir".
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(dir_splits1 = sub("^.*/C0*(\\d+( ?(-|CD) ?\\d+)?)( |_ )(.*)$", "\\1¶\\5", dir, perl=TRUE)) %>% 
  filter(dir_splits1 != dir) %>% 
  separate(dir_splits1, c("dir_splits2", "album"), sep = "¶")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Harmoniseer catalogusnummer en schijfnummer in de naam vd uitzendmac-directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(catDskNr_dir = harmoniseer_catDskNr_dir(dir_splits2)) %>% 
  separate(catDskNr_dir, c("catNr", "diskNr_dir"), sep = "¶") %>% 
  select(-dir_splits2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Knip diskNrs uit albumnaam ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(diskNr_album = getDiskNr_in_albumNaam(album))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Knip disk/trackNr's uit de tracknaam ----
# Behoud alleen die track-info's waarbij dat lukt.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(track_splits1 = sub("^(\\d{1,3}(-\\d{1,3})?).*$", "\\1", track, perl=TRUE)) %>% 
  filter(track_splits1 != track) %>% 
  mutate(dskTrkNr = harmoniseer_dskTrkNr(track_splits1)) %>% 
  separate(dskTrkNr, c("diskNr_track", "trackNr"), sep = "-") %>% 
  select(-track_splits1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Schijfnummer kiezen uit de 3 alternatieven, locatie vd track samenstellen, albumnaam opschonen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
uzmTrackInfo %<>% 
  mutate(catNr = as.integer(catNr), trackNr = as.integer(trackNr),
         diskNr = case_when(diskNr_dir   != "0" ~ as.integer(diskNr_dir),
                            diskNr_album != "0" ~ as.integer(diskNr_album),
                            diskNr_track != "0" ~ as.integer(diskNr_track),
                            TRUE ~  as.integer("1")
         ),
         uzm_locatie = paste0(dir, "/", track),
         album = str_replace_all(album, pattern = "_", replacement = " ")
  ) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Alleen de relevante variabelen overhouden + sorteren + ontdubbelen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
uzmTrackInfo %<>%
  select(catNr, diskNr, trackNr, album, lengte_uzm, uzm_locatie) %>%
  # parent folder vd audio verwijderen zodat audio ook op andere servers kan staan
  arrange(catNr, diskNr, trackNr) %>%
  distinct(catNr, diskNr, trackNr, .keep_all = TRUE)

uzmTrackInfo$uzm_locatie = str_replace(uzmTrackInfo$uzm_locatie, 
                                       pattern = "/Volumes/cz archief 01/", 
                                       replacement = "")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Bewaar in rds-indeling
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
saveRDS(object = uzmTrackInfo, file = "resources/uzmTrackInfo.rds")
