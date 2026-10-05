# Queue primitives only. Sourcing does not connect, create a table, or write.
# Bits are persistence encoding of the existing resource owner names, not UUID types.
salsa_dirty_owner_bits <- c(
  episode = 1L,
  broadcast = 2L,
  program = 4L,
  genre = 8L,
  subgenre = 16L,
  image = 32L,
  audio = 64L
)

salsa_dirty_owner_mask <- function(owners) {
  
  owners <- unique(as.character(owners))
  
  if (!length(owners) || anyNA(owners) || !all(owners %in% names(salsa_dirty_owner_bits))) 
    stop("Supply explicit resource owners: ", paste(names(salsa_dirty_owner_bits), collapse = ", "))

  as.integer(sum(salsa_dirty_owner_bits[owners]))
}

salsa_dirty_owners <- function(mask) {
  
  if (length(mask) != 1L || is.na(mask) || mask != floor(mask) || mask < 1L || mask > 127L) 
    stop("Invalid resource owner mask.")

  names(salsa_dirty_owner_bits)[bitwAnd(as.integer(mask), salsa_dirty_owner_bits) != 0L]
}

salsa_dirty_id <- function(id) {
  
  if (length(id) != 1L || is.na(id)) stop("Supply one source resource ID.")
  
  id <- sub("^0+", "", as.character(id))
  
  if (!grepl("^[1-9][0-9]*$", id) || nchar(id) > 20L ||
      (nchar(id) == 20L && id > "18446744073709551615")) stop("Invalid unsigned source ID.")
  
  id
}

mark_wp_salsa_dirty <- function(con,
                                resource_type,
                                resource_id,
                                owners,
                                action = "rebuild") {
  
  if (length(resource_type) != 1L || is.na(resource_type) ||
      !resource_type %in% c("wp_post", "wp_term")) stop("Unknown source resource type.")
  
  if (length(action) != 1L ||
      is.na(action) ||
      !action %in% c("rebuild", "delete")) stop("Unknown source action.")
  
  id <- salsa_dirty_id(resource_id)
  mask <- salsa_dirty_owner_mask(owners)
  
  dbExecute(
    con,
    "INSERT INTO wp_salsa_dirty
    (resource_type, resource_id, action, owner_mask, generation, delete_seen, first_changed_at, changed_at)
    VALUES (?, ?, ?, ?, 1, ?, UTC_TIMESTAMP(6), UTC_TIMESTAMP(6))
    ON DUPLICATE KEY UPDATE
      action = VALUES(action),
      owner_mask = owner_mask | VALUES(owner_mask),
      generation = generation + 1,
      delete_seen = GREATEST(delete_seen, VALUES(delete_seen)),
      last_attempted_at = NULL,
      changed_at = UTC_TIMESTAMP(6)",
    params = unname(list(
      resource_type,
      id,
      action,
      mask,
      as.integer(action == "delete")
    ))
  )
  
  invisible(NULL)
}

read_wp_salsa_dirty <- function(con, limit = 100L) {
  
  if (length(limit) != 1L || is.na(limit) || limit < 1L || limit > 500L || limit != floor(limit)) 
    stop("limit must be an integer from 1 to 500.")
  
  # Numeric interpolation is restricted to the validated integer limit.
  result <- dbGetQuery(
    con,
    paste0(
      "SELECT resource_type,
    CAST(resource_id AS CHAR) AS resource_id, action, owner_mask,
    CAST(generation AS CHAR) AS generation, delete_seen,
    CAST(first_changed_at AS CHAR) AS first_changed_at,
    CAST(changed_at AS CHAR) AS changed_at
    FROM wp_salsa_dirty
    ORDER BY COALESCE(last_attempted_at, first_changed_at), resource_type, resource_id LIMIT ",
      as.integer(limit)
    )
  )
  
  result$owners <- lapply(result$owner_mask, salsa_dirty_owners)
  result
}

acknowledge_wp_salsa_dirty <- function(con, entry, receipts) {
  
  if (!is.data.frame(entry) || nrow(entry) != 1L ||!all( c("resource_type",
                                                           "resource_id",
                                                           "generation",
                                                           "owner_mask",
                                                           "delete_seen") %in% names(entry))) 
    stop("Supply one previously read queue entry.")
  
  if (entry$resource_type != "wp_post" && entry$resource_type != "wp_term") stop("Unknown source type.")
  
  id <- salsa_dirty_id(entry$resource_id)
  generation <- salsa_dirty_id(entry$generation)
  owners <- salsa_dirty_owners(entry$owner_mask)
  
  if (is.na(entry$delete_seen) || entry$delete_seen != 0L) 
    stop("Source deletion was observed; retain this entry until deletion policy is implemented.")
  
  if (!is.list(receipts) || !length(receipts) || 
      !all(vapply(receipts, function(x) {
        is.list(x) && isTRUE(x$committed) && isTRUE(x$verified) && length(x$resource) == 1L && !is.na(x$resource) &&
          x$resource %in% owners && length(x$ids) > 0L
      }, logical(1)))) stop("Acknowledgement requires committed, verified resource receipts.")
  
  processed <- vapply(receipts, function(x)
    x$resource, character(1))
  
  if (!setequal(processed, owners)) stop("Every pending owner must be committed before acknowledgement.")
  
  affected <- dbExecute(
    con,
    "DELETE FROM wp_salsa_dirty
    WHERE resource_type = ? AND resource_id = ? AND generation = ?
      AND owner_mask = ? AND delete_seen = 0",
    params = unname(list(entry$resource_type, id, generation, as.integer(entry$owner_mask)))
  )
  
  identical(as.integer(affected), 1L)
}
