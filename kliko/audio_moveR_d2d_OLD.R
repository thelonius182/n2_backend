# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 1. Oude muziekweb-audio verwijderen uit sync-folder met U-2, Downloads/sync_uzm_two
# 2. Nieuwe muziekweb-audio verplaatsen van Downloads naar Downloads/sync_uzm_two
#    FolderSync op U-2 haalt ze daaruit op. Een bashscript op U-2 pakt ze daar uit en verwijdert ze
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

suppressWarnings(suppressPackageStartupMessages(library(futile.logger)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))
suppressWarnings(suppressPackageStartupMessages(library(zip)))

config <- read_yaml("config_nip_nxt.yaml")

fa <- flog.appender(appender.file("c:/Users/gergiev/Logs/nipper_uzm_two.log"), name = "d2d_log")

# Oude inhoud sync-met-U-2-folder verwijderen
muw_sync_zips <- dir_ls(path = "C:/Users/gergiev/Downloads/sync_uzm_two/",
                   recurse = F,
                   regexp = "Bestelling.+\\.zip$") %>% as_tibble() %>% rename(zip_name = value)

muw_sync_zips$zip_name %>% file_delete()

# Nieuwe inhoud in sync-met-U-2-folder plaatsen
muw_zips <- dir_ls(path = "C:/Users/gergiev/Downloads/",
                   recurse = F,
                   regexp = "Bestelling.+\\.zip$") %>% as_tibble() %>% rename(zip_name = value)
muw_err <- F
muw_err_msg <- NULL

if (nrow(muw_zips) > 0) {
  
  flog.info("
Muziekweb-audio verplaatsen naar lokale sync-folder", name = "d2d_log")
  
  for (a_zip in muw_zips$zip_name) {
    ### TEST ###
    # a_zip = "C:/Users/gergiev/Downloads/Bestelling#2179-D6C88977.zip"
    ### TEST ###
    move_zip_to <- str_replace(a_zip, pattern = "Downloads", replacement = "Downloads/sync_uzm_two")
    flog.info("... target: %s", move_zip_to, name = "d2d_log")
    
    tryCatch(expr = file_move(a_zip, move_zip_to),
             error = function(e1) {
               nuw_err <<- T
               nuw_err_msg <<- e1
             })
    
    if (muw_err) {
      flog.info("muw-move-fout: %s", muw_err_msg, name = "d2d_log")
    }
  }
  
  flog.info("Muziekweb-audio is verplaatst", name = "d2d_log")
}
