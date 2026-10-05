# Completion notifications for controlled source writers. Definitions only.
# Call notify_wp_salsa_post() after the existing writes have completed.
# Internal queue primitive, also used by disposable queue checks.
# Source writers use notify_wp_salsa_post(), which enforces publication scope.
wp_salsa_mark_post_batch <- function(con, post_ids) {
  ids <- unique(as.character(post_ids))
  if (!length(ids)) stop("Supply post IDs.")
  ids <- vapply(ids, salsa_dirty_id, character(1), USE.NAMES = FALSE)
  for (id in ids) mark_wp_salsa_dirty(con, "wp_post", id,
    c("episode", "broadcast", "audio"))
  invisible(NULL)
}

# Public adapter: call after all post writes. Only currently published posts notify.
notify_wp_salsa_post <- function(con, post_ids) {
  ids <- unique(as.character(post_ids))
  if (!length(ids)) stop("Supply post IDs.")
  ids <- vapply(ids, salsa_dirty_id, character(1), USE.NAMES = FALSE)
  for (id in ids) {
    post <- dbGetQuery(con, "SELECT post_type, post_status FROM wp_posts WHERE ID = ?",
      params = unname(list(id)))
    published <- nrow(post) == 1L && post$post_status == "publish" &&
      post$post_type %in% c("programma", "programma_woj")
    if (published) wp_salsa_mark_post_batch(con, id)
  }
  invisible(NULL)
}
