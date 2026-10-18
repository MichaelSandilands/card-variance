test_that("series keep their colour while they stay selected", {
  first <- assign_colours(c("a", "b", "c"))
  expect_equal(unname(first), 1:3)
  later <- assign_colours(c("c", "d"), first)
  expect_equal(later[["c"]], 3L)           # survivor keeps its colour
  expect_equal(later[["d"]], 1L)           # newcomer takes the first free slot
})

test_that("filters default to their total-like value", {
  codes <- tibble::tibble(dim = "RMT_INTTN", code = c("_T", "NR", "R"),
                          label = c("Total", "Non-remote", "Remote"))
  expect_equal(default_choice(c("Non-remote", "Remote", "Total"), codes, "RMT_INTTN"), "Total")
  expect_equal(default_choice(c("Non-remote", "Remote"), codes, "RMT_INTTN"), "Non-remote")
})

test_that("series are named by the filter values that vary", {
  all <- tibble::tibble(series_key = c("k1", "k2"), unit = "Euro",
                        AREA = c("Domestic", "World"), CH = c("Total", "Total"))
  expect_equal(series_full_names(all[1, ], all, c("AREA", "CH")), "Domestic")
  expect_equal(series_names(all, c("AREA", "CH")), c("Domestic", "World"))
})

test_that("the R2 board refuses to start without credentials", {
  withr::local_envvar(R2_ACCOUNT_ID = "", R2_ACCESS_KEY_ID = "", R2_SECRET_ACCESS_KEY = "", R2_BUCKET = "")
  expect_error(card_board("r2"), "R2_ACCOUNT_ID")
})

test_that("percent formatting shows a sign and a dash for missing", {
  expect_equal(fmt_pct(c(0.0321, -0.5, NA)), c("+3.2%", "-50.0%", "\u2013"))
})
