# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Componisten importeren uit GD en schoonmaken tbv Crawler
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Lees Componisten op GD ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(googlesheets)
nip_componisten_reg <- gs_title("Nipper Componisten")

nip_componisten <- nip_componisten_reg %>% 
  gs_read(ws = "componisten")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Maak seedlist: de query-strings voor Musicalics.com ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
seed_list <- nip_componisten %>% 
  distinct(componist_key) %>% 
  arrange(componist_key) %>% 
  mutate(seed_prep = str_replace_all(componist_key, pattern = ",", replacement = ""),
         seed = str_replace_all(seed_prep, pattern = " ", replacement = " AND ")
        ) %>% 
  select(-seed_prep)
