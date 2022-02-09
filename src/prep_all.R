# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Gegevens van uitzendmac en filemaker klaarzetten ----
# Resultaat: 2 tibbles - uzm_fm_schoon en opnamelengte
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# source("src/prep_uzm_import.R", encoding = "UTF-8")
# source("src/prep_filemaker_import.R", encoding = "UTF-8")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Filemaker en audiofiles koppelen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
uzmTrackInfo <- readRDS("resources/uzmTrackInfo.rds")
filemakerTrackInfo <- readRDS("resources/filemakerTrackInfo.rds")

uzm_fm_vuil <-
  left_join(uzmTrackInfo,
            filemakerTrackInfo,
            by = c("catNr",
                   "diskNr",
                   "trackNr" = "trackBeg"))

uzm_fm_bruikbaar <- uzm_fm_vuil %>%
  filter(trackNr == 1 & !is.na(trackEnd)) %>%
  distinct(catNr, diskNr) %>%
  mutate(heeftMetadata = TRUE)

uzm_fm_schoon <-
  left_join(uzm_fm_vuil, uzm_fm_bruikbaar, by = c("catNr", "diskNr")) %>%
  filter(heeftMetadata) %>%
  select(
    opnameNr,
    catNr,
    diskNr,
    trackBeg = trackNr,
    trackEnd,
    componist,
    titel,
    lengte_uzm,
    lengte_fm,
    album,
    bezetting,
    uitvoerenden,
    uzm_locatie,
    label,
    labelcode
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# OpnameNr's completeren & kolommen vervangen/hernoemen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
opnNr_sav <- NULL
i1 <- 0
tmp_opnameNr <- NULL # "tmp_", want er zit al een opnameNr in uzm_fm_schoon

for (opnNr in uzm_fm_schoon$opnameNr) {
  i1 <- i1 + 1
  
  if (!is.na(opnNr)) {
    opnNr_sav <- opnNr
  }
  
  tmp_opnameNr[i1] <- opnNr_sav
}

uzm_fm_schoon %<>% select(-opnameNr, -trackEnd)

uzm_fm_schoon <- bind_cols(as.data.frame(tmp_opnameNr), uzm_fm_schoon)

uzm_fm_schoon %<>% rename(opnameNr = tmp_opnameNr, trackNr = trackBeg)

rm(uzm_fm_bruikbaar, uzm_fm_vuil, tmp_opnameNr, i1, opnNr, opnNr_sav)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Opnamelengtes berekenen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
opnamelengte <- uzm_fm_schoon %>% 
  group_by(opnameNr) %>% 
  summarise(tot_lengte_in_seconden = sum(lengte_uzm))
