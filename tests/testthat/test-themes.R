context("themes")

library(ggplot2)
x <- c(rnorm(100), rnorm(100, 2))
d <- data.frame(x = x, grp = rep(LETTERS[1:2], each = 100))
g <- ggplot(d, aes(x, colour = grp, fill = grp)) + geom_density(size = 1, alpha = 0.5)

test_that("themes return as expected", {
  expect_is(theme_snap(), c("theme", "gg"))
  expect_is(theme_snapdark(), c("theme", "gg"))
  expect_is(g + theme_snap(), "ggplot")
  expect_is(g + theme_snapdark(), "ggplot")
})

test_that("palettes return as expected", {
  expect_is(snapalettes(), "character")
  expect_equal(length(snapalettes()), 9)
  expect_equal(length(snapalettes("q1")), 9)
  expect_equal(length(snapalettes(n = 5)), 5)
  expect_error(snapalettes(n = 10), "The selected color palette contains fewer than 10 colors.")
})
