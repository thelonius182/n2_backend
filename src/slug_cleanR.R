library(stringi)

nipperstudio_redacteuren_raw <- read_delim("C:/cz_salsa/nipperstudio_redacteuren.txt", col_names = FALSE, delim = "¶", lazy = F)

red.1 <- nipperstudio_redacteuren_raw %>% 
  mutate(slug_red = X1 %>% 
           stri_trans_general("latin-ascii") %>% 
           str_replace_all("[ ]", "_") %>% 
           str_replace_all("[-:()!&'\"]", "") %>% 
           str_to_lower()) %>% 
  mutate(slug_red = str_replace_all(slug_red, "_van_|_de_|_den_|_der_|_le_", "_")) %>% 
  mutate(slug_red = str_replace_all(slug_red, "_van_|_de_|_den_|_der_", "_")) %>% 
  mutate(slug_red = str_replace_all(slug_red, "_en_.*", "")) %>% 
  mutate(slug_red = sub("(.*)_(\\w).*", "\\1_\\2", slug_red, perl=TRUE)) %>% 
  mutate(slug_red = str_replace_all(slug_red, "_", " ")) %>% 
  mutate(slug_red = stri_trans_totitle(slug_red)) %>% 
  mutate(slug_red = str_replace_all(slug_red, " ", "_")) %>% 
  rename(redacteur = X1, redacteur_slug = slug_red)

nipperstudio_titels_raw <- read_delim("C:/cz_salsa/nipperstudio_pgm_titels.txt", col_names = FALSE, delim = "¶", lazy = F)

tit.1 <- nipperstudio_titels_raw %>% 
  mutate(slug_tit = X1 %>% 
           stri_trans_general("latin-ascii") %>% 
           str_replace_all("[ ]", "_") %>% 
           str_replace_all("[-:()!&'\"]", "") %>% 
           str_to_lower()) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, "_van_|_de_|_den_|_der_|_le_|_het_", "_")) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, "_van_|_de_|_den_|_der_|_le_|_het_", "_")) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, "_van_|_de_|_den_|_der_", "_")) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, "_en_.*", "")) %>% 
  mutate(slug_tit = sub("(.*)_(\\w).*", "\\1_\\2", slug_tit, perl=TRUE)) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, "_", " ")) %>% 
  mutate(slug_tit = stri_trans_totitle(slug_tit)) %>% 
  mutate(slug_tit = str_replace_all(slug_tit, " ", "_")) %>% 
  rename(titel = X1, titel_slug = slug_tit)

write_delim(red.1, "C:/cz_salsa/red_slugs.tsv", delim = "\t")
write_delim(tit.1, "C:/cz_salsa/tit_slugs.tsv", delim = "\t")
