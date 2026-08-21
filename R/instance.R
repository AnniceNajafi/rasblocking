#' Default competition settings
#'
#' Parameter defaults from the RAS 2026 release. Any of these may be
#' overridden by a `setting.csv` shipped with an instance.
#' @return A named list of settings.
#' @export
ras_settings_default <- function() {
  list(
    min_vol_short = 350,      #OD distance <  100 mi
    min_vol_med   = 700,      #100 <= OD distance <= 500 mi
    min_vol_long  = 1050,     #OD distance >  500 mi
    max_circuitous_ratio = 1.3,
    block_fixed_cost = 2500,
    transport_cost_coefficient = 1,
    interchange_cost = 100,
    stress_penalty_M = 5,
    demand_multiplier = 1
  )
}

#' Read a GMNS-format blocking instance
#'
#' Reads `node.csv`, `link.csv`, `demand.csv` and (optionally) `setting.csv`
#' from a directory. The node and link files follow the General Modeling
#' Network Specification, so any GMNS network with the required yard attributes
#' can be read.
#'
#' @param dir Directory containing the instance CSV files.
#' @param settings Optional settings list; defaults to [ras_settings_default()].
#' @return A list with `nodes`, `links`, `demands` (data.tables) and `settings`.
#' @export
ras_read_instance <- function(dir, settings = ras_settings_default()) {
  need <- c("node.csv", "link.csv", "demand.csv")
  miss <- need[!file.exists(file.path(dir, need))]
  if (length(miss)) {
    stop("missing instance files in ", dir, ": ", paste(miss, collapse = ", "))
  }
  nodes <- data.table::fread(file.path(dir, "node.csv"))
  links <- data.table::fread(file.path(dir, "link.csv"))
  dem   <- data.table::fread(file.path(dir, "demand.csv"))

  sfile <- file.path(dir, "setting.csv")
  if (file.exists(sfile)) {
    s <- data.table::fread(sfile)
    if (all(c("parameter", "value") %in% tolower(names(s)))) {
      data.table::setnames(s, tolower(names(s)))
      kv <- stats::setNames(as.list(s$value), s$parameter)
      settings <- utils::modifyList(settings, lapply(kv, function(z) {
        n <- suppressWarnings(as.numeric(z)); if (is.na(n)) z else n
      }))
    }
  }
  structure(list(nodes = nodes, links = links, demands = dem,
                 settings = settings), class = "ras_instance")
}

#' @export
print.ras_instance <- function(x, ...) {
  cat("<ras_instance>\n")
  cat(sprintf("  nodes   %8d  (yards %d)\n", nrow(x$nodes),
              sum(x$nodes$node_type == "yard", na.rm = TRUE)))
  cat(sprintf("  links   %8d\n", nrow(x$links)))
  cat(sprintf("  demands %8d  (volume %s cars)\n", nrow(x$demands),
              format(sum(x$demands$volume), big.mark = ",")))
  invisible(x)
}

#' Read an origin-destination minimum-distance matrix
#'
#' @param path Path to `od_distance_matrix.csv` or
#'   `yard_to_yard_min_distance.csv`.
#' @return A data.table keyed on `from_yard_id`, `to_yard_id`.
#' @export
ras_read_od <- function(path) {
  od <- data.table::fread(path)
  req <- c("from_yard_id", "to_yard_id", "min_distance_mile")
  if (!all(req %in% names(od))) {
    stop("OD matrix must contain: ", paste(req, collapse = ", "))
  }
  od <- unique(od, by = c("from_yard_id", "to_yard_id"))
  data.table::setkeyv(od, c("from_yard_id", "to_yard_id"))
  od[, req, with = FALSE]
}

#' Read a solution payload
#'
#' @param path Path to a `solution_result*.json` file.
#' @return A list with `design`, `sequence` and `route` data.tables.
#' @export
ras_read_solution <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  o <- if (!is.null(j$outputs)) j$outputs else j
  get <- function(nm) {
    v <- o[[nm]]
    if (is.null(v)) stop("solution is missing section: ", nm)
    data.table::as.data.table(v)
  }
  structure(list(design   = get("1 Block Design"),
                 sequence = get("2 Blocking Sequence"),
                 route    = get("3 Block Route")),
            class = "ras_solution")
}

#' @export
print.ras_solution <- function(x, ...) {
  cat("<ras_solution>\n")
  cat(sprintf("  blocks    %6d\n", nrow(x$design)))
  cat(sprintf("  sequences %6d\n", nrow(x$sequence)))
  invisible(x)
}
