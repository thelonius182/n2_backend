# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Verplaats de muziekweb-audio naar UZM
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

suppressWarnings(suppressPackageStartupMessages(library(futile.logger)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))
suppressWarnings(suppressPackageStartupMessages(library(zip)))

config <- read_yaml("config_nip_nxt.yaml")

fa <- flog.appender(appender.file("c:/Users/gergiev/Logs/nipper.log"), name = "nipperlog")

muw_zips <- dir_ls(path = "C:/Users/gergiev/Downloads/",
                   recurse = F,
                   regexp = "Bestelling.+\\.zip$") %>% as_tibble() %>% rename(zip_name = value)
muw_err <- F
muw_err_msg <- NULL

if (nrow(muw_zips) > 0) {
  
  flog.info("
Muziekweb-audio verplaatsen en uitpakken...", name = "nipperlog")
  
  for (a_zip in muw_zips$zip_name) {
    ### TEST ###
    # a_zip = "C:/Users/gergiev/Downloads/Bestelling#1562-D2CA3BF6.zip"
    ### TEST ###
    flog.info("Verplaats nieuwe Muziekweb-zip naar UZM: %s",
              a_zip,
              name = "nipperlog")
    
    tryCatch(expr = file_move(a_zip, "//UITZENDMAC-CZ/Avonden/Nipper/muziekweb_audio/"),
             error = function(e1) {
               nuw_err <<- T
               nuw_err_msg <<- e1
             })
    
    if (muw_err) {
      flog.info("muw-move-fout: %s", muw_err_msg, name = "nipperlog")
    }
    
    uzm_zip <-
      str_replace(a_zip, pattern = "C:/Users/gergiev/Downloads/", "//UITZENDMAC-CZ/Avonden/Nipper/muziekweb_audio/")
    uzm_zip_done <- paste0(uzm_zip, ".done")
    
    zip::unzip(uzm_zip, junkpaths = T, exdir = "//UITZENDMAC-CZ/Avonden/Nipper/muziekweb_audio")
    file_move(uzm_zip, uzm_zip_done)
  }
  
  flog.info("Muziekweb-audio verplaatsen en uitpakken... DONE", name = "nipperlog")
}
