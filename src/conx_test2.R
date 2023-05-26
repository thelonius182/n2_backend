tracks <- function(query) {
  conx_tst_tracks <- dbGetQuery(ns_con, query)
}
