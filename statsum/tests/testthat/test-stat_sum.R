library(gapminder)

test_that("stat_sum works", {
  expect_error(stat_sum(gapminder$country))
  expect_error(stat_sum("A", "B", "C"))
  expect_error(stat_sum(""))
  expect_error(stat_sum(" "))
  expect_error(stat_sum(c("0", "1")))
  expect_error(stat_sum(c(TRUE, FALSE)))
  expect_error(stat_sum(c("5")))
  expect_equal(stat_sum(gapminder$pop), c(min(gapminder$pop, na.rm=T), max(gapminder$pop, na.rm=T),
               quantile(gapminder$pop, 0.25, na.rm=T), median(gapminder$pop, na.rm=T),
               quantile(gapminder$pop, 0.75, na.rm=T), range(gapminder$pop, na.rm=T),
               IQR(gapminder$pop, na.rm=T), sd(gapminder$pop, na.rm=T)))
})
