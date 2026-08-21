DIRECT_ONLY <- c("Intermodal", "Automobile")

#' Structural diagnosis of a blocking instance
#'
#' Reports the two structural facts that govern how hard an instance is,
#' independently of any solver:
#'
#' * the **sub-tier share** -- the fraction of demands whose scaled volume falls
#'   below the minimum block volume set by their own origin-destination
#'   distance. A sub-tier demand cannot open a block alone, so every block it
#'   rides must be a coalition. This share rises sharply as the demand
#'   multiplier falls, because the tiers are absolute while demand scales.
#' * the **direct-only floor** -- demands of a commodity type confined to a
#'   single direct block whose entire available pool is below tier. No feasible
#'   design serves these at all, so the penalty they carry is irreducible.
#'
#' The floor is proved over the direct-only commodity types only. Demand of
#' other types in the same penalty term is *not* covered by it and may well be
#' addressable; see the `addressable` element of the result.
#'
#' @param instance A `ras_instance`.
#' @param od OD distance matrix from [ras_read_od()].
#' @param multiplier Demand multiplier.
#' @param direct_only Commodity types restricted to a single direct block.
#' @return An object of class `ras_diagnosis`.
#' @export
ras_diagnose <- function(instance, od, multiplier = 1,
                         direct_only = DIRECT_ONLY) {
  s <- instance$settings
  d <- data.table::copy(instance$demands)
  d[, "vol" := as.numeric(get("volume")) * multiplier]

  odk <- stats::setNames(od$min_distance_mile,
                         paste(od$from_yard_id, od$to_yard_id))
  d[, "od" := as.numeric(odk[paste(get("origin_yard_id"), get("dest_yard_id"))])]
  n_missing <- sum(is.na(d$od))
  d <- d[!is.na(d$od)]
  d[, "tier" := ras_tier(get("od"), s)]
  d[, "sub_tier" := get("vol") < get("tier")]
  d[, "direct" := get("block_type") %in% direct_only]

  #A direct-only demand may only pool with demand sharing its exact
  #(origin, destination, commodity type) triple.
  pool <- d[, list(pool = sum(get("vol"))),
            by = c("origin_yard_id", "dest_yard_id", "block_type")]
  d <- merge(d, pool, by = c("origin_yard_id", "dest_yard_id", "block_type"),
             all.x = TRUE)
  d[, "locked" := get("direct") & get("pool") < get("tier")]

  structure(list(
    multiplier   = multiplier,
    n_demands    = nrow(d),
    n_missing_od = n_missing,
    sub_tier_n   = sum(d$sub_tier),
    sub_tier_pct = 100 * mean(d$sub_tier),
    locked_n     = sum(d$locked),
    locked_cars  = sum(d$vol[d$locked]),
    addressable  = d[d$sub_tier & !d$locked,
                     list(n = .N, cars = sum(get("vol"))), by = "block_type"],
    by_type      = d[d$sub_tier, list(n = .N), by = c("block_type")],
    demands      = d
  ), class = "ras_diagnosis")
}

#' @export
print.ras_diagnosis <- function(x, ...) {
  cat("<ras_diagnosis>  demand multiplier", x$multiplier, "\n")
  cat(sprintf("  demands              %6d\n", x$n_demands))
  if (x$n_missing_od) {
    cat(sprintf("  dropped (no OD dist) %6d\n", x$n_missing_od))
  }
  cat(sprintf("  sub-tier             %6d  (%.1f%%)  cannot open a block alone\n",
              x$sub_tier_n, x$sub_tier_pct))
  cat(sprintf("  direct-only locked   %6d  (%s cars)  unservable by ANY design\n",
              x$locked_n, format(round(x$locked_cars), big.mark = ",")))
  if (nrow(x$addressable)) {
    cat("  addressable sub-tier demand by type:\n")
    for (i in seq_len(nrow(x$addressable))) {
      cat(sprintf("    %-14s %6d demands  %10s cars\n",
                  x$addressable$block_type[i], x$addressable$n[i],
                  format(round(x$addressable$cars[i]), big.mark = ",")))
    }
    cat("  NOTE: the locked count is a floor over direct-only types only.\n")
    cat("        The rows above are NOT covered by it.\n")
  }
  invisible(x)
}
