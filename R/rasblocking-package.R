#' rasblocking: Scoring, Verification and Diagnostics for Railroad Blocking Plans
#'
#' Tools for the railroad blocking problem as posed by the INFORMS RAS 2026
#' Problem Solving Competition. The package reimplements the competition
#' objective and its feasibility checks exactly, provides two independent
#' verification gates that reject payloads which pass every feasibility check
#' while misreporting their own cost, computes structural diagnostics that
#' certify which demand is unservable by any feasible design, and enumerates the
#' destination-pooling construction used to serve sub-tier demand.
#'
#' Instances are read in the General Modeling Network Specification (GMNS)
#' node/link format, so the diagnostics apply to networks beyond the competition
#' instances. The package depends on no mathematical programming solver and no
#' Python.
#'
#' @section Main entry points:
#' \describe{
#'   \item{[ras_read_instance()]}{read a GMNS instance}
#'   \item{[ras_diagnose()]}{structural diagnosis, no solver required}
#'   \item{[ras_validate()]}{the official feasibility checks}
#'   \item{[ras_stress()], [ras_score()]}{objective and competition score}
#'   \item{[ras_verify()]}{both verification gates}
#'   \item{[ras_pool_candidates()]}{donor-funded pooling candidates}
#' }
#'
#' @keywords internal
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom igraph graph_from_data_frame distances V E
#' @importFrom stats setNames
#' @importFrom utils head modifyList
"_PACKAGE"

#data.table uses non-standard evaluation; declare the column names that appear
#bare inside j/by expressions so R CMD check does not read them as globals.
utils::globalVariables(c("volume"))
