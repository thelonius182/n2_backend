# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# =                                                M A I N
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

config <- read_yaml("config.yaml")

source(config$toolbox, encoding = "UTF-8")

filter <-
  dplyr::filter # don't use stats::filter unless so qualified

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Er valt alleen iets te preppen als er een nieuwe import van uitzendmac of filemaker is
# <TODO>analyseer de file-properties om dit vast te stellen</TODO>
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (config$uitzendmac_gewijzigd | config$filemaker_gewijzigd) {
  source("src/prep_uzm_import.R", encoding = "UTF-8")
  source("src/prep_filemaker_import.R", encoding = "UTF-8")
}

source("src/prep_all.R", encoding = "UTF-8")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees gematchte Componisten op GD ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
nip_componisten_GD_reg <- gs_title("Nipper Componisten") 

gd_componisten <- nip_componisten_GD_reg %>% 
  gs_read(ws = "componisten")

gd_componisten_db <- nip_componisten_GD_reg %>% 
  gs_read(ws = "db_componisten") %>% 
  select(-starts_with("X"), -starts_with("peil"), -starts_with("land"), -delta) %>% 
  filter(!is.na(id))

ref_componisten <- left_join(gd_componisten, gd_componisten_db, by = "componist_key")
rm(gd_componisten, gd_componisten_db, nip_componisten_GD_reg)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Voeg componist-data toe aan track-info ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
nipper <- left_join(uzm_fm_schoon, ref_componisten, by = c("componist" = "componist_FM")) %>% 
  select(
    opnameNr,
    catNr,
    diskNr,
    trackNr,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    lengte_uzm,
    lengte_fm,
    album,
    label,
    labelcode,
    van,
    tot,
    uzm_locatie,
    -id,
    - componist
  ) %>% 
  arrange(
    opnameNr,
    catNr,
    diskNr,
    trackNr
  ) %>% 
  mutate(trackNr = factor(trackNr, levels = unique(trackNr))) %>% 
  group_by(opnameNr) %>% 
  mutate(opnameVlgNr = dense_rank(trackNr)) 

rm(ref_componisten)

saveRDS(object = nipper, file = "resources/nipper.rds")

nipper_werk_links <- nipper %>% 
  select(
    opnameNr,
    opnameVlgNr,
    catNr, 
    diskNr,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    lengte_fm,
    album
  ) %>% 
  filter(opnameVlgNr == 1) %>% 
  select(-opnameVlgNr)

nipper_werk <- left_join(nipper_werk_links, opnamelengte, by = "opnameNr") %>% 
  mutate(fm_uzm_delta = round(abs(tot_lengte_in_seconden - lengte_fm), digits = 0),
         lengte_uzm = as.character(hms::as.hms(round(tot_lengte_in_seconden, digits = 0))),
         lengte_fm = as.character(hms::as.hms(round(lengte_fm, digits = 0)))) %>% 
  select(
    opnameNr,
    catNr,
    diskNr,
    lengte_fm,
    lengte_uzm,
    fm_uzm_delta,
    tijdvak,
    nationaliteit,
    componist_key,
    componist_lbl,
    titel,
    bezetting,
    uitvoerenden,
    album
  ) 

nipper_werk_delta <- nipper_werk %>% 
  select(catNr, diskNr, opnameNr, fm_uzm_delta) %>% 
  group_by(catNr, diskNr) %>% 
  summarise(median_delta = median(fm_uzm_delta)) %>% 
  filter(median_delta > 15.0)
  
rm(nipper_werk_links)

nipper_tracks <- nipper %>% 
  select(
    opnameNr,
    opnameVlgNr,
    uzm_locatie
  )

saveRDS(object = nipper_tracks, file = "resources/nipper_tracks.rds")

nipper_todo <- nipper_werk %>% filter(tijdvak == "?") %>% 
  group_by(componist_key) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

nipper_werk %<>% 
  filter(tijdvak != "?") %>% 
  select(tijdvak,
         componist_lbl,
         nationaliteit,
         titel,
         lengte = lengte_uzm,
         bezetting,
         uitvoerenden,
         album,
         catNr, 
         diskNr,
         opnameNr
  ) %>% 
  mutate(tijdvak = factor(tijdvak, levels = c("Middeleeuwen",
                                              "Renaissance",
                                              "Barok",
                                              "Klassiek",
                                              "Romantiek",
                                              "Modern")
                          )
  ) %>% 
  arrange(tijdvak,
          componist_lbl,
          titel,
          lengte
  ) 

deletables <- nipper_werk_delta %>% left_join(nipper_werk) %>% select(opnameNr)

nipper_werk %<>% 
  filter(!opnameNr %in% deletables$opnameNr) %>% 
  select(-catNr, -diskNr)

write_delim(nipper_werk, config$nipper_werk, delim = "\U0009")

rm(filemakerTrackInfo, uzmTrackInfo, uzm_fm_schoon, deletables, nipper_werk_delta)
