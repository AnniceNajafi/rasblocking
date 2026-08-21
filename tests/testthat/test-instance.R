skip_if_no_data <- function() {
  d <- Sys.getenv("RAS_DATA_ROOT", unset = "")
  if (!nzchar(d) || !dir.exists(file.path(d, "datasets", "l1"))) {
    skip("set RAS_DATA_ROOT to the release directory to run instance tests")
  }
  d
}

test_that("a GMNS instance reads with the expected shape", {
  d <- skip_if_no_data()
  inst <- ras_read_instance(file.path(d, "datasets", "l1"))
  expect_s3_class(inst, "ras_instance")
  expect_true(all(c("node_id", "node_type") %in% names(inst$nodes)))
  expect_true(all(c("link_id", "from_node_id", "to_node_id", "length")
                  %in% names(inst$links)))
  expect_gt(nrow(inst$demands), 0)
})

test_that("layer 2 is layer 1 relabelled, not a larger instance", {
  d <- skip_if_no_data()
  a <- ras_read_instance(file.path(d, "datasets", "l1"))$demands
  b <- ras_read_instance(file.path(d, "datasets", "l2"))$demands
  k <- c("origin_yard_id", "dest_yard_id", "volume", "block_type")
  A <- a[order(a$origin_yard_id, a$dest_yard_id, a$volume, a$block_type), ..k]
  B <- b[order(b$origin_yard_id, b$dest_yard_id, b$volume, b$block_type), ..k]
  expect_equal(nrow(A), nrow(B))
  expect_true(isTRUE(all.equal(as.data.frame(A), as.data.frame(B))))
})

test_that("cost and stress reproduce the competition backend", {
  d <- skip_if_no_data()
  sol_path <- Sys.getenv("RAS_SOLUTION_L1_05", unset = "")
  if (!nzchar(sol_path) || !file.exists(sol_path)) {
    skip("set RAS_SOLUTION_L1_05 to a solution_result JSON to run this test")
  }
  inst <- ras_read_instance(file.path(d, "datasets", "l1"))
  od   <- ras_read_od(file.path(d, "scoring", "od_distance_matrix.csv"))
  sol  <- ras_read_solution(sol_path)

  cst <- ras_cost(sol, inst)
  expect_equal(cst$fixed,       2562500,       tolerance = 1e-6)
  expect_equal(cst$transport,   1278207183.83, tolerance = 1e-2)
  expect_equal(cst$handling,    38229660,      tolerance = 1e-2)
  expect_equal(cst$interchange, 38430700,      tolerance = 1e-2)

  st <- ras_stress(sol, inst, od, multiplier = 0.5)
  expect_equal(st$stress, 1956721815.48, tolerance = 1e-2)
  expect_equal(st$unserved_cars, 92610, tolerance = 1)
})

test_that("all eleven official checks pass on a valid payload", {
  d <- skip_if_no_data()
  sol_path <- Sys.getenv("RAS_SOLUTION_L1_05", unset = "")
  if (!nzchar(sol_path) || !file.exists(sol_path)) skip("no solution supplied")
  inst <- ras_read_instance(file.path(d, "datasets", "l1"))
  od   <- ras_read_od(file.path(d, "scoring", "od_distance_matrix.csv"))
  sol  <- ras_read_solution(sol_path)
  v <- ras_validate(sol, inst, od, 0.5)
  expect_true(v$pass)
  expect_length(v$checks, 11)
})

test_that("diagnosis reports the coalition regime and the direct-only floor", {
  d <- skip_if_no_data()
  inst <- ras_read_instance(file.path(d, "datasets", "l1"))
  od   <- ras_read_od(file.path(d, "scoring", "od_distance_matrix.csv"))

  half <- ras_diagnose(inst, od, 0.5)
  expect_equal(half$sub_tier_pct, 84.5, tolerance = 0.1)
  expect_equal(half$locked_n, 213)

  #the tiers are absolute, so scaling demand up relieves the regime
  full <- ras_diagnose(inst, od, 2.0)
  expect_lt(full$sub_tier_pct, half$sub_tier_pct)

  #the floor covers direct-only types ONLY; other sub-tier demand remains
  expect_gt(sum(half$addressable$n), 0)
  expect_false(any(half$addressable$block_type %in%
                     c("Intermodal", "Automobile")))
})
