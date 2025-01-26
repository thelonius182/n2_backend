pacman::p_load(readr, dplyr, stringr, rio, tidyr)

om1 <- import("g:/salsa/om_muw_req.txt")

order_lines <- om1 |> mutate(s1 = str_split(recording_no, pattern = "-", simplify = T),
                       album_id = s1[,1], track_id = as.character(parse_number(s1[,2]))) |> 
  select(album_id, track_id) |> 
  pivot_longer(names_to = "df_name", values_to = "order_line", cols = c("album_id", "track_id")) |> 
  select(order_line)

write_lines(x = order_lines$order_line, file = "g:/salsa/om_muw_req_ord.txt", append = F)
