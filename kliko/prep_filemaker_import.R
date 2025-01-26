# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Geïmporteerde track-info uit Filemaker schoonmaken
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Lees composities, geïmporteerd uit DB-klassiek ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo <-
  read_delim("resources/export_filemaker_20180523.tab",
             "\t",
             escape_double = FALSE,
             trim_ws = TRUE
  ) %>% 
  filter(!is.na(`cz-catalogusnummer`) 
         & !is.na(tracknummers) 
         & !is.na(componist)
         & !str_detect(componist, "-|--|\\(")
         & str_detect(`cz-catalogusnummer`, "^C\\d+")
  ) %>% 
  mutate(hh = if_else(is.na(h), 0, as.numeric(h) * 3600L),
         mm = if_else(is.na(m), 0, as.numeric(m) * 60L),
         ss = if_else(is.na(s), 0, as.numeric(s)),
         lengte_fm = hh + mm + ss
  ) %>%
  select(
    componist,
    titel,
    czid = `cz-catalogusnummer`,
    album = `cd-of-lp-naam`,
    tracks = tracknummers,
    lengte_fm,
    bezetting,
    uitvoerenden = `uitvoerenden-1`,
    label,
    labelcode
  ) %>% 
  arrange(
      componist,
      titel,
      czid,
      tracks
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# czID's schoonmaken ----
#   schoonmaken, dan classificeren om de probleemgevallen te kunnen opsporen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(czid  = str_replace_all(czid, pattern = "\\.$",  replacement = ""), # EINDIGT op punt! 
         czid  = str_replace_all(czid, pattern = "[ .,/:;]", replacement = "-"),
         czid2 = str_replace_all(czid, pattern = "\\d", replacement = "0")
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Schone compositielijst maken ----
# Verzamel de unieke czid-types + aantal. Behoud alleen composities met een geldig type (in czID[1:3]) 
# of een trackreeks-met-schijfnummer waarmee een ongeldig type te repareren valt.
#
#   czID[1:3]  type       aantal
#'  ---------  ---------  ------
#   1          C00000     124160
#   2          C00000-0   703
#   3          C00000-00  123
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
count_czid_type <- count(filemakerTrackInfo, czid2, sort = TRUE) # dwz sort by "n"!

filemakerTrackInfo %<>% 
  filter(czid2 %in% count_czid_type$czid2[1:3] | str_detect(tracks, pattern = ":"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Catalogus/Disk/Track uitknippen ----
# czID splitsen in catNr en diskNr
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(czid = harmoniseer_catDskNr_in_FM_czID(czid)) %>% 
  separate(czid, c("catNr", "diskNr_czid"), sep = " ")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# tracks splitsen in diskNr, trackBeg, trackEnd
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(tracks_splits1 = harmoniseer_catTrcks_in_FM_tracks(tracks)) %>% 
  separate(tracks_splits1, c("diskNr_tracks", "trackBeg", "trackEnd"), sep = "¶")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Uiteindelijke Cat/Dsk/Trk-nr's bepalen + ontdubbelen; skip alles met onconverteerbaar tracknr, zoals
# "4 CD's", "1:01-39;2:01-25;3:01-26", etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(catNr = as.integer(catNr),
         diskNr_czid = as.integer(diskNr_czid),
         diskNr_tracks = as.integer(diskNr_tracks),
         diskNr = case_when(diskNr_tracks > 0 ~ diskNr_tracks,
                            diskNr_czid > 0 ~ diskNr_czid,
                            TRUE ~ as.integer("1")
         ),
         trackBeg = as.integer(trackBeg),
         trackEnd = as.integer(trackEnd)
  ) %>% 
  filter(!is.na(trackBeg) & catNr > 0) %>% 
  select(catNr, diskNr, trackBeg, trackEnd, componist, titel, 
         bezetting, uitvoerenden, lengte_fm, label, labelcode) %>% 
  arrange(catNr, diskNr, trackBeg) %>% 
  distinct(catNr, diskNr, trackBeg, .keep_all = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# OpnameNr uitdelen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
filemakerTrackInfo %<>% 
  mutate(opnameNr = 1:nrow(filemakerTrackInfo))
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Bewaar in rds-indeling
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
saveRDS(object = filemakerTrackInfo, file = "resources/filemakerTrackInfo.rds")
