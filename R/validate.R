#' Check a blocking plan against the official feasibility rules
#'
#' Implements the competition's feasibility checks. Each element of the result
#' names the constraint it enforces and reports the number of violations.
#'
#' @param solution A `ras_solution`.
#' @param instance A `ras_instance`.
#' @param od OD distance matrix from [ras_read_od()].
#' @param multiplier Demand multiplier.
#' @param direct_only Commodity types confined to a single direct block.
#' @return An object of class `ras_validation`.
#' @export
ras_validate <- function(solution, instance, od, multiplier = 1,
                         direct_only = DIRECT_ONLY) {
  s <- instance$settings
  des <- solution$design
  seqs <- solution$sequence
  chk <- list()
  bad <- function(name, n, note = NULL) {
    chk[[name]] <<- list(pass = n == 0, n_violations = n, note = note)
  }

  bidx <- stats::setNames(seq_len(nrow(des)), des$block_id)
  bfrom <- stats::setNames(des$from_yard_id, des$block_id)
  bto   <- stats::setNames(des$to_yard_id,   des$block_id)
  seq_ids <- lapply(seqs$blocking_sequence, ras_parse_arrow)

  #--- 1 flow conservation: chain must run origin -> destination -----------
  v1 <- 0L; v1b <- 0L
  for (i in seq_along(seq_ids)) {
    ids <- seq_ids[[i]]
    if (!length(ids)) { v1 <- v1 + 1L; next }
    fr <- bfrom[as.character(ids)]; to <- bto[as.character(ids)]
    if (any(is.na(fr))) { v1 <- v1 + 1L; next }
    if (fr[1] != seqs$origin_yard_id[i] ||
        to[length(to)] != seqs$dest_yard_id[i]) v1 <- v1 + 1L
    else if (length(ids) > 1 && any(to[-length(to)] != fr[-1])) v1 <- v1 + 1L
    yards <- c(fr, to[length(to)])
    if (anyDuplicated(yards)) v1b <- v1b + 1L
  }
  bad("1_flow_conservation", v1)
  bad("1b_no_subtour", v1b)

  #--- 2 yard track limits (classification block types only) --------------
  CLASSIF <- c("manifest", "coal", "grain")
  cb <- des[tolower(as.character(des$block_type)) %in% CLASSIF, ]
  per <- if (nrow(cb)) table(cb$from_yard_id) else integer(0)
  ntr <- stats::setNames(as.numeric(instance$nodes$num_tracks),
                         instance$nodes$node_id)
  v2 <- if (length(per)) {
    sum(as.numeric(per) > ifelse(is.na(ntr[names(per)]), Inf, ntr[names(per)]))
  } else 0
  bad("2_yard_track_limits", v2)

  #--- 3 yard handling capacity -------------------------------------------
  hv <- ras_handling_volume(solution, instance)
  hcap <- stats::setNames(as.numeric(instance$nodes$handling_capacity),
                          instance$nodes$node_id)
  v3 <- if (nrow(hv)) {
    sum(hv$volume > ifelse(is.na(hcap[as.character(hv$yard_id)]), Inf,
                           hcap[as.character(hv$yard_id)]))
  } else 0
  bad("3_yard_handling_capacity", v3)

  #--- 4 minimum block volume ---------------------------------------------
  fl <- ras_block_flows(solution, instance)
  odk <- stats::setNames(od$min_distance_mile,
                         paste(od$from_yard_id, od$to_yard_id))
  bod <- as.numeric(odk[paste(des$from_yard_id, des$to_yard_id)])
  fvol <- stats::setNames(fl$volume, fl$block_id)[as.character(des$block_id)]
  used <- !is.na(fvol) & fvol > 0
  v4 <- sum(used & fvol < ras_tier(bod, s) - 1e-6, na.rm = TRUE)
  bad("4_min_block_volume", v4)

  #--- 5 link capacity -----------------------------------------------------
  lcap <- stats::setNames(as.numeric(instance$links$capacity),
                          instance$links$link_id)
  lf <- data.table::rbindlist(Map(function(p, b) {
    ids <- ras_parse_arrow(p)
    if (!length(ids)) return(NULL)
    data.table::data.table(link_id = ids,
                           v = as.numeric(fvol[as.character(b)]))
  }, solution$route$physical_path_links, solution$route$block_id))
  v5 <- 0
  if (length(lf) && nrow(lf)) {
    agg <- lf[, list(v = sum(get("v"), na.rm = TRUE)), by = "link_id"]
    cap <- lcap[as.character(agg$link_id)]
    v5 <- sum(!is.na(cap) & agg$v > cap)
  }
  bad("5_link_capacity", v5)

  #--- 6 maximum circuitous ratio -----------------------------------------
  dist <- stats::setNames(fl$distance, fl$block_id)[as.character(des$block_id)]
  v6 <- sum(!is.na(bod) & bod > 0 & !is.na(dist) &
              dist > s$max_circuitous_ratio * bod + 1e-4)
  bad("6_max_circuitous_ratio", v6)

  #--- 7 one blocking sequence per demand ---------------------------------
  v7 <- sum(seqs[, list(n = length(unique(get("blocking_sequence")))),
                 by = "commodity_id"]$n > 1)
  bad("7_single_path_uniqueness", v7)

  #--- 8 one commodity type per block -------------------------------------
  bt <- data.table::rbindlist(Map(function(ids, ty) {
    if (!length(ids)) return(NULL)
    data.table::data.table(block_id = ids, type = ty)
  }, seq_ids, as.character(seqs$commodity_type)))
  v8 <- if (length(bt) && nrow(bt)) {
    sum(bt[, list(n = length(unique(get("type")))), by = "block_id"]$n > 1)
  } else 0
  bad("8_blocking_rule_single_commodity_type", v8)

  #--- 9 direct-only commodities ------------------------------------------
  v9 <- 0L
  for (i in seq_along(seq_ids)) {
    if (!(as.character(seqs$commodity_type[i]) %in% direct_only)) next
    ids <- seq_ids[[i]]
    if (length(ids) != 1) { v9 <- v9 + 1L; next }
    if (bfrom[as.character(ids)] != seqs$origin_yard_id[i] ||
        bto[as.character(ids)] != seqs$dest_yard_id[i]) v9 <- v9 + 1L
  }
  bad("9_direct_block_rule", v9)

  #--- 9b demand volume consistency ---------------------------------------
  dem <- data.table::copy(instance$demands)
  dem[, "vol" := as.numeric(get("volume")) * multiplier]
  srv <- seqs[, list(served = sum(as.numeric(get("volume")))),
              by = c("commodity_id")]
  data.table::setnames(srv, "commodity_id", "demand_id")
  m <- merge(dem, srv, by = "demand_id", all.y = TRUE)
  v9b <- sum(is.na(m$vol)) + sum(!is.na(m$vol) & m$served > m$vol + 1e-6) +
    sum(as.numeric(seqs$volume) <= 0)
  bad("9b_demand_volume_consistency", v9b)

  structure(list(checks = chk,
                 pass = all(vapply(chk, function(z) z$pass, logical(1)))),
            class = "ras_validation")
}

#' @export
print.ras_validation <- function(x, ...) {
  cat(sprintf("<ras_validation>  %s\n",
              if (x$pass) "ALL CHECKS PASSED" else "INFEASIBLE"))
  for (n in names(x$checks)) {
    z <- x$checks[[n]]
    cat(sprintf("  %-40s %s%s\n", n, if (z$pass) "pass" else "FAIL",
                if (z$pass) "" else sprintf("  (%d violations)", z$n_violations)))
  }
  invisible(x)
}
