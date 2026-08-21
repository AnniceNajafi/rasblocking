test_that("arrow parsing matches the official encoding", {
  expect_equal(ras_parse_arrow("12 -> 45 -> 9"), c(12L, 45L, 9L))
  expect_equal(ras_parse_arrow("83"), 83L)
  expect_equal(ras_parse_arrow(NA), integer(0))
  expect_equal(ras_parse_arrow(""), integer(0))
})

test_that("minimum volume tiers follow the distance bands", {
  expect_equal(ras_tier(99.9), 350)
  expect_equal(ras_tier(100), 700)
  expect_equal(ras_tier(500), 700)
  expect_equal(ras_tier(500.1), 1050)
})

test_that("exchange rates follow from the group scales", {
  #layer 2 is layer 1 relabelled, so an L1 dollar is credited in two groups
  expect_equal(ras_exchange_rate("L1"), 1 / (9 * 15e9) + 1 / (9 * 18e9))
  expect_equal(ras_exchange_rate("L3"), 1 / (9 * 20e9))
  expect_equal(ras_exchange_rate("L1"), ras_exchange_rate("L2"))
  #an L1 dollar is worth about 2.44 L3 dollars
  expect_equal(ras_exchange_rate("L1") / ras_exchange_rate("L3"), 2.444,
               tolerance = 1e-3)
})

test_that("ladder intervals match the published table", {
  expect_equal(ras_ladder(3, 9), c(0, 6))
  expect_equal(ras_ladder(3, 4), c(30, 36))
  expect_equal(ras_ladder(3, 3), c(36, 42))
  expect_error(ras_ladder(3, 99))
})

test_that("score reproduces a known submission to ten decimals", {
  st <- c(1956721815.48, 3282092985.62, 6063953324.43,
          1956721815.48, 3282092985.62, 6063953324.43,
          1981835306.49, 3289398367.48, 6072907674.84)
  s <- ras_score(st)
  expect_equal(s$ladder_lower, 0)
  expect_equal(s$score, 0.2165173894, tolerance = 1e-9)
  expect_equal(unname(s$components[1]), 0.2511726250, tolerance = 1e-9)
  expect_equal(unname(s$components[3]), 0.1890690225, tolerance = 1e-9)
})

test_that("an invalid scenario drops the submission onto a worse ladder rung", {
  st <- c(1956721815.48, 3282092985.62, 6063953324.43,
          1956721815.48, 3282092985.62, NA,
          1981835306.49, 3289398367.48, 6072907674.84)
  s <- ras_score(st)
  #eight solved scenarios across three layers -> interval [6, 12)
  expect_equal(s$ladder_lower, 6)
  expect_gt(s$score, 6)
})
