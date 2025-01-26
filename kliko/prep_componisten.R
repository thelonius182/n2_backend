# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Scraper output Musicalics.com inlezen
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

musicalics <- read_delim("resources/comp_r.csv",
                         "\t",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         col_names = TRUE
) %>% 
  select(-starts_with("X"), -id)

seed_mus <- bind_cols(seed_list, musicalics)

seed_mus %<>% 
  mutate(seed = if_else(is.na(geboren_dtm), NA_character_, seed),
         componist = if_else(is.na(geboren_dtm), NA_character_, componist)
  ) %>% 
  select(componist_key = componist_std, componist_lbl = componist, 
         leefde_van = geboren_dtm, geboren_te, leefde_tot = overleden_op, overleden_te)
