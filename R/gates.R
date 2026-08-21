#' Structural gate: reject malformed or impossible block routes
#'
#' Deliberately knows nothing about the cost model. It checks that every
#' declared physical route is at least as long as the shortest path between the
#' block's endpoints, that block identifiers are integers in both the design and
#' the route table, and that the two tables reference the same set of blocks.
#'
#' This gate is one half of a pair. On its own it accepts a design that is
#' structurally perfect but mispriced; see [ras_gate_arithmetic()]. Run this
#' one first -- it is cheap, model-free, and catches the failure whose
#' downstream arithmetic would otherwise look self-consistent.
#'
#' @param solution A `ras_solution`.
#' @param instance A `ras_instance`.
#' @param tol Relative tolerance on the shortest-path floor.
#' @return An object of class `ras_gate` with `pass` and `findings`.
#' @export
ras_gate_structure <- function(solution, instance, tol = 1e-6) {
  f <- list()

  is_int_like <- function(v) {
    if (is.numeric(v)) return(all(is.finite(v) & v == floor(v)))
    !any(is.na(suppressWarnings(as.integer(as.character(v)))))
  }
  if (!is_int_like(solution$design$block_id)) {
    f$design_block_id <- "non-integer block_id in '1 Block Design'"
  }
  if (!is_int_like(solution$route$block_id)) {
    f$route_block_id <- "non-integer block_id in '3 Block Route'"
  }

  db <- unique(as.integer(solution$design$block_id))
  rb <- unique(as.integer(solution$route$block_id))
  if (length(setdiff(rb, db))) {
    f$orphan_routes <- sprintf("%d routed blocks are not designed",
                               length(setdiff(rb, db)))
  }
  if (length(setdiff(db, rb))) {
    f$unrouted <- sprintf("%d designed blocks have no route",
                          length(setdiff(db, rb)))
  }

  used <- unique(unlist(lapply(solution$sequence$blocking_sequence,
                               ras_parse_arrow)))
  if (length(setdiff(db, used))) {
    f$unused <- sprintf("%d designed blocks carry no demand",
                        length(setdiff(db, used)))
  }

  #in-metric shortest-path floor, orientation-tolerant
  g <- igraph::graph_from_data_frame(
    data.frame(from = as.character(instance$links$from_node_id),
               to   = as.character(instance$links$to_node_id),
               weight = as.numeric(instance$links$length)),
    directed = FALSE)

  fl <- ras_block_flows(solution, instance)
  dmap <- stats::setNames(fl$distance, fl$block_id)
  d <- solution$design
  ends <- unique(data.frame(o = as.character(d$from_yard_id),
                            e = as.character(d$to_yard_id)))
  have <- igraph::V(g)$name
  ends <- ends[ends$o %in% have & ends$e %in% have, , drop = FALSE]

  short <- rep(NA_real_, nrow(d))
  if (nrow(ends)) {
    for (o in unique(ends$o)) {
      tgt <- ends$e[ends$o == o]
      dd <- igraph::distances(g, v = o, to = tgt, weights = igraph::E(g)$weight)
      key <- paste(o, tgt)
      short[match(key, paste(d$from_yard_id, d$to_yard_id))] <- as.numeric(dd)
    }
  }
  declared <- dmap[as.character(d$block_id)]
  bad <- which(!is.na(short) & !is.na(declared) &
                 declared < short * (1 - tol) - 1e-6)
  if (length(bad)) {
    f$below_floor <- sprintf(
      "%d blocks declare a route shorter than the shortest path (worst %.3f mi)",
      length(bad), max(short[bad] - declared[bad]))
  }
  zero <- which(!is.na(declared) & declared == 0 & !is.na(short) & short > 0)
  if (length(zero)) {
    f$zero_distance <- sprintf(
      "%d blocks declare zero route distance on a non-trivial OD pair",
      length(zero))
  }

  structure(list(gate = "structure", pass = length(f) == 0, findings = f),
            class = "ras_gate")
}

#' Arithmetic gate: recompute every cost component from primary data
#'
#' Recomputes the four cost components from the instance files and compares
#' them against a declared cost. Catches the family of failures in which a
#' defaulted lookup silently converts a missing key into a free operation.
#'
#' It does **not** catch a cost that is missing from both the payload and the
#' recomputation -- for example a blank physical path, which both sides read as
#' absent and therefore agree on. Agreement between two implementations is
#' evidence of consistency, not of correctness, when both read the same source.
#' That case is caught by [ras_gate_structure()].
#'
#' @inheritParams ras_gate_structure
#' @param declared Optional named list of declared components to compare
#'   against, e.g. the output of a solver's own accounting.
#' @param tol Absolute tolerance in dollars.
#' @return An object of class `ras_gate`.
#' @export
ras_gate_arithmetic <- function(solution, instance, declared = NULL, tol = 0.01) {
  got <- ras_cost(solution, instance)
  f <- list()

  fl <- ras_block_flows(solution, instance)
  nz <- sum(fl$distance == 0 & fl$volume > 0)
  if (nz > 0) {
    f$zero_priced <- sprintf("%d blocks carry volume but price zero transport", nz)
  }
  hv <- ras_handling_volume(solution, instance)
  known <- instance$nodes$node_id
  unknown <- sum(!(hv$yard_id %in% known))
  if (unknown > 0) {
    f$unknown_yard <- sprintf("%d reclassification yards are absent from node.csv (handled free)",
                              unknown)
  }
  if (!is.null(declared)) {
    for (k in intersect(names(declared), names(got))) {
      if (abs(as.numeric(declared[[k]]) - got[[k]]) > tol) {
        f[[paste0("mismatch_", k)]] <- sprintf(
          "%s: declared %.2f, recomputed %.2f (delta %.2f)",
          k, declared[[k]], got[[k]], declared[[k]] - got[[k]])
      }
    }
  }
  structure(list(gate = "arithmetic", pass = length(f) == 0,
                 findings = f, cost = got), class = "ras_gate")
}

#' @export
print.ras_gate <- function(x, ...) {
  cat(sprintf("<ras_gate: %s>  %s\n", x$gate,
              if (x$pass) "PASS" else "FAIL"))
  for (n in names(x$findings)) cat("  -", x$findings[[n]], "\n")
  invisible(x)
}

#' Run both gates
#'
#' Neither gate suffices alone. A payload is promotable only if both pass.
#' @inheritParams ras_gate_arithmetic
#' @return A list of two `ras_gate` objects with an overall `pass`.
#' @export
ras_verify <- function(solution, instance, declared = NULL) {
  a <- ras_gate_structure(solution, instance)
  b <- ras_gate_arithmetic(solution, instance, declared)
  structure(list(structure = a, arithmetic = b,
                 pass = a$pass && b$pass), class = "ras_verification")
}

#' @export
print.ras_verification <- function(x, ...) {
  print(x$structure); print(x$arithmetic)
  cat(sprintf("OVERALL: %s\n", if (x$pass) "PROMOTABLE" else "REJECTED"))
  invisible(x)
}
