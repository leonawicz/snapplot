context("leaflet")

test_that("leafloc returns as expected", {
  x1 <- leafloc("Fairbanks")
  x2 <- leafloc(as.numeric(snaplocs::get_coords("Fairbanks")))
  x3 <- leafloc("Anchorage")
  expect_is(x1, "leaflet")
  expect_equal(x1, x2)
  expect_true(x1$x$setView[[1]][1] != x3$x$setView[[1]][1])
})
