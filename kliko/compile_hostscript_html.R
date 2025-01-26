library(knitr)
library(rmarkdown)

np_sec2hms <- function(duur_sec) {
  result <- paste0("00:00:", duur_sec) %>% 
    hms(roll = TRUE) 
  result <- sprintf("%02d:%02d:%02d", result@hour, result@minute, result@.Data)
}

draaiboek <- pl_werken %>%
  arrange(playlist, vt_blok_letter, vt_blok_nr)

drbk <- pl_werken %>%
  group_by(playlist, vt_blok_letter) %>% 
  summarise(bloklengte = sum(lengte)) %>% 
  mutate(bloklengte = bloklengte + 40, # per blok 40 seconden presentatie
         cum_lengte = cumsum(as.integer(bloklengte)))

drbk$cum_lengte <- np_sec2hms(drbk$cum_lengte)
drbk$bloklengte_hms <- np_sec2hms(drbk$bloklengte)

distinct_playlists <- unique(pl_werken$playlist)

for (d_pls in distinct_playlists) {
  drb <- draaiboek %>% dplyr::filter(playlist == d_pls) %>% 
    left_join(drbk, by = c("playlist", "vt_blok_letter")) %>% 
    left_join(pl_nieuw, by = c("playlist"))
  
  rmarkdown::render(
    input = "src/draaiboek_sjabloon_html_0.1.Rmd",
    output_format = "html_document",
    output_file = paste(d_pls, ".html", sep = ""),
    output_dir = config$home_draaiboeken
  )
  
}
