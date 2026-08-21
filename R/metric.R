#' Parse an arrow-separated integer list
#'
#' The competition encodes blocking sequences and physical paths as
#' `"12 -> 45 -> 9"`. Splitting is on the literal `" -> "`, matching the
#' official validator.
#' @param x A character scalar.
#' @return An integer vector; empty for `NA` or empty input.
#' @export
ras_parse_arrow <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  if (is.na(x) || !nzchar(trimws(as.character(x)))) return(integer(0))
  p <- strsplit(as.character(x), " -> ", fixed = TRUE)[[1]]
  p <- trimws(p)
  suppressWarnings(as.integer(p[nzchar(p)]))
}

#' Minimum block volume tier
#'
#' The tier is a step function of the origin-destination distance of the
#' block's endpoints. It is absolute: it does not scale with the demand
#' multiplier, which is the source of the coalition regime documented in
#' [ras_diagnose()].
#'
#' @param dist Numeric vector of OD distances in miles.
#' @param settings Settings list; see [ras_settings_default()].
#' @return Numeric vector of minimum volumes.
#' @export
ras_tier <- function(dist, settings = ras_settings_default()) {
  ifelse(dist < 100, settings$min_vol_short,
         ifelse(dist <= 500, settings$min_vol_med, settings$min_vol_long))
}

#' Realised volume and route distance per block
#'
#' @param solution A `ras_solution`.
#' @param instance A `ras_instance`.
#' @return A data.table with `block_id`, `volume` (realised from the blocking
#'   sequences) and `distance` (sum of physical link lengths).
#' @export
ras_block_flows <- function(solution, instance) {
  seqs <- solution$sequence
  bids <- lapply(seqs$blocking_sequence, ras_parse_arrow)
  n <- vapply(bids, length, integer(1))
  flow <- data.table::data.table(
    block_id = unlist(bids),
    volume   = rep(as.numeric(seqs$volume), n)
  )
  vol <- flow[, list(volume = sum(volume)), by = "block_id"]

  llen <- stats::setNames(as.numeric(instance$links$length), instance$links$link_id)
  dist <- vapply(solution$route$physical_path_links, function(p) {
    ids <- ras_parse_arrow(p)
    if (!length(ids)) return(0)
    sum(llen[as.character(ids)], na.rm = TRUE)
  }, numeric(1))
  rd <- data.table::data.table(block_id = solution$route$block_id, distance = dist)

  out <- merge(data.table::data.table(block_id = solution$design$block_id),
               vol, by = "block_id", all.x = TRUE)
  out <- merge(out, rd, by = "block_id", all.x = TRUE)
  out[is.na(out$volume), "volume"] <- 0
  out[is.na(out$distance), "distance"] <- 0
  out[]
}

#' Volume reclassified at each yard
#'
#' Handling is charged at the origin yard of every block in a demand's
#' sequence *after the first*: the first block is built at the demand's own
#' origin and is not an intermediate reclassification.
#'
#' @inheritParams ras_block_flows
#' @return A data.table with `yard_id` and `volume`.
#' @export
ras_handling_volume <- function(solution, instance) {
  bf <- stats::setNames(solution$design$from_yard_id, solution$design$block_id)
  recs <- data.table::rbindlist(Map(function(bs, v) {
    ids <- ras_parse_arrow(bs)
    if (length(ids) < 2) return(NULL)
    data.table::data.table(yard_id = bf[as.character(ids[-1])], volume = v)
  }, solution$sequence$blocking_sequence, as.numeric(solution$sequence$volume)))
  if (!length(recs) || nrow(recs) == 0) {
    return(data.table::data.table(yard_id = integer(0), volume = numeric(0)))
  }
  recs <- recs[!is.na(recs$yard_id)]
  recs[, list(volume = sum(volume)), by = "yard_id"]
}

CLASS_I <- c("BNSF", "CN", "CSX", "UP", "NS", "CPKC")

#' Normalise a railroad label
#' @param rr Character vector of railroad identifiers.
#' @return Upper-case labels with `CSXT` mapped to `CSX`.
#' @export
ras_norm_railroad <- function(rr) {
  rr <- toupper(trimws(as.character(rr)))
  rr[rr == "CSXT"] <- "CSX"
  rr
}

#' Operating cost of a blocking plan
#'
#' Computes the four components of the competition objective: block fixed
#' cost, transport, handling and interchange.
#'
#' @inheritParams ras_block_flows
#' @return A named list of the four components and their `total`.
#' @export
ras_cost <- function(solution, instance) {
  s <- instance$settings
  fl <- ras_block_flows(solution, instance)

  fixed <- nrow(solution$design) * s$block_fixed_cost
  transport <- sum(fl$volume * fl$distance) * s$transport_cost_coefficient

  hv <- ras_handling_volume(solution, instance)
  hc <- stats::setNames(as.numeric(instance$nodes$handling_cost), instance$nodes$node_id)
  handling <- if (nrow(hv)) {
    sum(hv$volume * ifelse(is.na(hc[as.character(hv$yard_id)]), 0,
                           hc[as.character(hv$yard_id)]))
  } else 0

  rr <- stats::setNames(ras_norm_railroad(instance$nodes$railroad_id),
                        instance$nodes$node_id)
  d <- solution$design
  ro <- rr[as.character(d$from_yard_id)]
  rd <- rr[as.character(d$to_yard_id)]
  chargeable <- !is.na(ro) & !is.na(rd) & ro %in% CLASS_I & rd %in% CLASS_I & ro != rd
  volmap <- stats::setNames(fl$volume, fl$block_id)
  interchange <- sum(volmap[as.character(d$block_id)][chargeable], na.rm = TRUE) *
    s$interchange_cost

  list(fixed = fixed, transport = transport, handling = handling,
       interchange = interchange,
       total = fixed + transport + handling + interchange)
}

#' Stress score of a blocking plan
#'
#' The competition objective: operating cost plus `M` times unserved
#' car-miles, where unserved car-miles are priced on the origin-destination
#' minimum distance of the demand.
#'
#' @inheritParams ras_block_flows
#' @param od OD distance matrix from [ras_read_od()].
#' @param multiplier Demand multiplier (0.5, 1 or 2).
#' @return A list with `cost`, `unserved_cars`, `unserved_carmiles`,
#'   `served_ratio` and `stress`.
#' @export
ras_stress <- function(solution, instance, od, multiplier = 1) {
  s <- instance$settings
  dem <- data.table::copy(instance$demands)
  dem[, "volume" := as.numeric(get("volume")) * multiplier]

  served <- solution$sequence[, list(served = sum(as.numeric(get("volume")))),
                              by = c("commodity_id")]
  data.table::setnames(served, "commodity_id", "demand_id")
  d <- merge(dem, served, by = "demand_id", all.x = TRUE)
  d[is.na(d$served), "served"] <- 0
  d[, "short" := pmax(get("volume") - get("served"), 0)]

  key <- paste(d$origin_yard_id, d$dest_yard_id)
  odk <- stats::setNames(od$min_distance_mile,
                         paste(od$from_yard_id, od$to_yard_id))
  d[, "od" := as.numeric(odk[key])]
  if (any(is.na(d$od) & d$short > 0)) {
    stop("cannot price unserved demand: OD distance missing for ",
         sum(is.na(d$od) & d$short > 0), " rows")
  }
  ucm <- sum(d$short * ifelse(is.na(d$od), 0, d$od))

  cost <- ras_cost(solution, instance)
  list(cost = cost,
       unserved_cars = sum(d$short),
       unserved_carmiles = ucm,
       served_ratio = sum(d$served) / sum(d$volume),
       stress = cost$total + s$stress_penalty_M * ucm)
}

GROUP_SCALES <- c(15e9, 18e9, 20e9)

#' Ladder interval for a solved-scenario count
#' @param solved_cases Number of solved network layers (0-3).
#' @param solved_scenarios Number of solved scenarios (0-9).
#' @return Numeric length-2 vector, the interval `[lower, upper)`.
#' @export
ras_ladder <- function(solved_cases, solved_scenarios) {
  tab <- list(
    "3-9" = c(0, 6),   "3-8" = c(6, 12),  "3-7" = c(12, 18), "3-6" = c(18, 24),
    "3-5" = c(24, 30), "3-4" = c(30, 36), "3-3" = c(36, 42), "2-6" = c(42, 48),
    "2-5" = c(48, 54), "2-4" = c(54, 60), "2-3" = c(60, 66), "2-2" = c(66, 72),
    "1-3" = c(72, 81), "1-2" = c(81, 90), "1-1" = c(90, 99), "0-0" = c(100, 100))
  k <- paste(solved_cases, solved_scenarios, sep = "-")
  if (is.null(tab[[k]])) stop("no ladder interval for ", k)
  tab[[k]]
}

#' Final competition score from nine scenario stresses
#'
#' Scenarios are grouped by network layer in threes and each group is scaled by
#' its own denominator; the mean of the three scaled group means is added to a
#' ladder lower bound determined by how many scenarios validated.
#'
#' @param stress Numeric vector of nine stress scores, ordered
#'   L1(0.5,1,2), L2(0.5,1,2), L3(0.5,1,2). `NA` marks an invalid scenario.
#' @return A list with `components`, `quality`, `ladder_lower` and `score`.
#' @export
ras_score <- function(stress) {
  if (length(stress) != 9) stop("expected 9 scenario stresses")
  g <- rep(1:3, each = 3)
  comp <- vapply(1:3, function(i) {
    v <- stress[g == i]; v <- v[!is.na(v)]
    if (!length(v)) NA_real_ else mean(v) / GROUP_SCALES[i]
  }, numeric(1))
  solved_scen <- sum(!is.na(stress))
  solved_cases <- sum(vapply(1:3, function(i) any(!is.na(stress[g == i])), logical(1)))
  lower <- ras_ladder(solved_cases, solved_scen)[1]
  quality <- mean(comp[!is.na(comp)])
  list(components = comp, quality = quality, ladder_lower = lower,
       score = lower + quality)
}

#' Marginal value of one dollar of cost removed from a scenario
#'
#' Because layer 2 is layer 1 relabelled, the two carry identical stress and a
#' dollar saved on a layer-1 scenario is credited in two groups.
#'
#' @param layer One of `"L1"`, `"L2"`, `"L3"`.
#' @return Score improvement per dollar removed.
#' @export
ras_exchange_rate <- function(layer = c("L1", "L2", "L3")) {
  layer <- match.arg(layer)
  switch(layer,
         L1 = 1 / (9 * GROUP_SCALES[1]) + 1 / (9 * GROUP_SCALES[2]),
         L2 = 1 / (9 * GROUP_SCALES[1]) + 1 / (9 * GROUP_SCALES[2]),
         L3 = 1 / (9 * GROUP_SCALES[3]))
}
