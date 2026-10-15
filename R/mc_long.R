#' @title Convert Wide Method Comparison Data to Long Format
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Converts data with one column per measurement method into the long format
#' (one row per measurement) used by `method_vc()` and [sim_methods()].
#'
#' @param data A data frame in wide format, with one column per method.
#' @param methods Character vector naming the method columns (at least two).
#'   The order sets the order of the `method` factor levels; the first is the
#'   reference method.
#' @param id Name of the column identifying the item (subject). If `NULL`
#'   (default), each row is treated as a separate item and an `id` column is
#'   created from the row number.
#' @param replicate Name of the column identifying the replicate (measurement
#'   occasion). If `NULL` and some items have more than one row, a `replicate`
#'   column is created from the row order within each item (with a message):
#'   methods recorded on the same row are then treated as measured on the same
#'   occasion.
#' @param names_to Name of the new column holding the method names. Default
#'   is "method".
#' @param values_to Name of the new column holding the measurements. Default
#'   is "y".
#'
#' @return A tibble with columns for the item identifier (named as in `id`, or
#'   `id` if created), the replicate (if supplied or created), the method (a
#'   factor with levels in the order of `methods`), and the measurement. Rows
#'   with a missing measurement are kept.
#'
#' @examples
#' data("reps")
#' # reps has replicate pairs of x and y measured on the same occasion
#' reps_long = mc_long(reps, methods = c("x", "y"), id = "id")
#' head(reps_long)
#'
#' @seealso `method_vc()`, [sim_methods()]
#' @importFrom tidyr pivot_longer
#' @export

mc_long = function(data,
                   methods,
                   id = NULL,
                   replicate = NULL,
                   names_to = "method",
                   values_to = "y") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (!is.character(methods) || length(methods) < 2) {
    stop("`methods` must be a character vector naming at least two method ",
         "columns.", call. = FALSE)
  }
  if (anyDuplicated(methods)) {
    stop("`methods` must not contain duplicates.", call. = FALSE)
  }
  miss = setdiff(c(methods, id, replicate), names(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in data: ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }
  for (arg in c("id", "replicate")) {
    val = get(arg)
    if (!is.null(val) && (!is.character(val) || length(val) != 1)) {
      stop("`", arg, "` must be a single column name.", call. = FALSE)
    }
  }
  if (any(c(id, replicate) %in% methods)) {
    stop("`id` and `replicate` must not be method columns.", call. = FALSE)
  }

  df = dplyr::as_tibble(data)

  ## item identifier ----
  if (is.null(id)) {
    if ("id" %in% names(df) && !("id" %in% methods)) {
      stop("data already has a column named 'id'; supply it as `id` or ",
           "rename it.", call. = FALSE)
    }
    df$id = seq_len(nrow(df))
    id = "id"
  }

  ## replicate ----
  if (is.null(replicate) && anyDuplicated(df[[id]])) {
    if ("replicate" %in% names(df)) {
      stop("Some items have several rows; supply the replicate column as ",
           "`replicate`.", call. = FALSE)
    }
    message("Some items have several rows; a 'replicate' column was created ",
            "from the row order within each item.")
    df = df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(id))) %>%
      dplyr::mutate(replicate = dplyr::row_number()) %>%
      dplyr::ungroup()
    replicate = "replicate"
  }

  out = df %>%
    dplyr::select(dplyr::all_of(c(id, replicate, methods))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(methods),
                        names_to = names_to,
                        values_to = values_to)
  out[[names_to]] = factor(out[[names_to]], levels = methods)
  out
}
