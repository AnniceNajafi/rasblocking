#' Enumerate donor-funded destination-pooling candidates
#'
#' The minimum block volume is evaluated on **aggregate** block volume, not per
#' demand. A demand stranded below tier can therefore reach an otherwise
#' unreachable destination by riding a block that some other demand's volume
#' already justifies.
#'
#' The construction: take an existing block \eqn{X \to Y} carrying volume
#' \eqn{f}, and a set of sub-tier demands that share a destination \eqn{D}.
#' Build \eqn{X \to D} and \eqn{D \to Y} and move every rider of \eqn{X \to Y}
#' onto the new pair. The old block empties and closes, refunding its fixed cost
#' and freeing a classification track at \eqn{X}; the two new blocks clear their
#' tiers on the donor's volume; and the stranded demands are served at the cost
#' of the detour.
#'
#' Two properties make this pay where single-demand moves do not. Pooling by
#' shared **destination** rather than origin halves the donor requirement while
#' serving the whole group from one funded block. And each accepted move creates
#' blocks that become staging yards for the next, so candidates must be
#' re-enumerated after every application rather than filtered once.
#'
#' This function enumerates and prices candidates; it does not apply them. Any
#' candidate must still pass [ras_verify()] and the full feasibility check
#' before promotion.
#'
#' @param instance A `ras_instance`.
#' @param solution A `ras_solution` giving the incumbent design.
#' @param od OD distance matrix from [ras_read_od()].
#' @param multiplier Demand multiplier.
#' @param max_candidates Cap on the number of candidates returned.
#' @return A data.table of candidates ordered by estimated net gain, with
#'   columns `donor_block`, `X`, `Y`, `D`, `donor_volume`, `pooled_cars`,
#'   `detour_miles`, `penalty_released`, `added_transport` and `net_gain`.
#' @export
ras_pool_candidates <- function(instance, solution, od, multiplier = 1,
                                max_candidates = 200L) {
  s <- instance$settings
  dg <- ras_diagnose(instance, od, multiplier)
  d <- dg$demands

  #stranded = sub-tier, not locked by the direct-only rule, and unserved
  served <- solution$sequence[, list(served = sum(as.numeric(get("volume")))),
                              by = c("commodity_id")]
  data.table::setnames(served, "commodity_id", "demand_id")
  d <- merge(d, served, by = "demand_id", all.x = TRUE)
  d[is.na(d$served), "served"] <- 0
  stranded <- d[d$sub_tier & !d$locked & d$served < d$vol]
  if (!nrow(stranded)) {
    return(data.table::data.table())
  }

  odk <- stats::setNames(od$min_distance_mile,
                         paste(od$from_yard_id, od$to_yard_id))
  odist <- function(a, b) as.numeric(odk[paste(a, b)])

  fl <- ras_block_flows(solution, instance)
  vol <- stats::setNames(fl$volume, fl$block_id)
  des <- solution$design
  des$volume <- as.numeric(vol[as.character(des$block_id)])

  #group stranded demand by destination
  grp <- stranded[, list(pooled = sum(get("vol") - get("served")),
                         n = .N,
                         type = get("block_type")[1]),
                  by = c("dest_yard_id")]
  grp <- grp[order(-grp$pooled)]

  out <- vector("list", 0L)
  for (i in seq_len(nrow(grp))) {
    D <- grp$dest_yard_id[i]
    pooled <- grp$pooled[i]
    #donors must carry enough volume to clear both new tiers on their own
    cand <- des[des$volume > 0 & des$from_yard_id != D & des$to_yard_id != D, ]
    if (!nrow(cand)) next
    dXD <- odist(cand$from_yard_id, D)
    dDY <- odist(D, cand$to_yard_id)
    dXY <- odist(cand$from_yard_id, cand$to_yard_id)
    ok <- !is.na(dXD) & !is.na(dDY) & !is.na(dXY) &
      cand$volume + pooled >= ras_tier(dXD, s) &
      cand$volume >= ras_tier(dDY, s) &
      (dXD + dDY) <= s$max_circuitous_ratio * dXY

    if (!any(ok)) next
    k <- which(ok)
    detour <- (dXD[k] + dDY[k]) - dXY[k]
    #penalty released: stranded cars now served, priced on their own OD
    rel <- pooled * mean(odist(stranded$origin_yard_id[
      stranded$dest_yard_id == D], D), na.rm = TRUE) * s$stress_penalty_M
    add <- cand$volume[k] * detour * s$transport_cost_coefficient +
      s$block_fixed_cost   #two new blocks, one closed
    gain <- rel - add
    out[[length(out) + 1L]] <- data.table::data.table(
      donor_block = cand$block_id[k], X = cand$from_yard_id[k],
      Y = cand$to_yard_id[k], D = D,
      donor_volume = cand$volume[k], pooled_cars = pooled,
      detour_miles = detour, penalty_released = rel,
      added_transport = add, net_gain = gain)
  }
  if (!length(out)) return(data.table::data.table())
  res <- data.table::rbindlist(out)
  res <- res[order(-res$net_gain)]
  utils::head(res, max_candidates)
}
