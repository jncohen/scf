# tests/testthat/test-fixes_list.R

load_mock <- function() {
  td <- tempfile()
  dir.create(td)
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  s <- scf_load(2022, data_directory = td)
  unlink(td, recursive = TRUE, force = TRUE)
  s
}

test_that("scf_freq keeps the same columns with and without by", {
  s <- load_mock()
  a <- scf_freq(s, ~own)$results
  b <- scf_freq(s, ~own, by = ~edcl)$results
  expect_equal(names(a), names(b))
  expect_equal(names(a), c("group", "category", "proportion", "se_proportion", "n"))
  expect_true(all(a$group == "All"))
  expect_equal(sum(a$proportion), 100)
})

test_that("descriptive results report the number of households", {
  s <- load_mock()
  n_all <- nrow(s$mi_design[[1]]$variables)
  ed <- table(s$mi_design[[1]]$variables$edcl)

  expect_equal(scf_mean(s, ~networth)$results$n, n_all)
  expect_equal(scf_median(s, ~networth)$results$n, n_all)
  expect_equal(scf_median(s, ~networth, method = "stack")$results$n, n_all)

  m <- scf_mean(s, ~networth, by = ~edcl)$results
  expect_equal(m$n, as.numeric(ed[m$group]))
  p <- scf_percentile(s, ~networth, q = 0.9, by = ~edcl)$results
  expect_equal(p$n, as.numeric(ed[p$group]))
  f <- scf_freq(s, ~own, by = ~edcl)$results
  expect_equal(f$n, as.numeric(ed[f$group]))
})

test_that("tiny groups give one warning that names them", {
  s <- load_mock()
  s <- scf_update(s, tiny = ifelse(seq_along(age) <= 4, "a", "b"))
  collect <- function(expr) {
    w <- character(0)
    val <- withCallingHandlers(expr, warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    list(value = val, warnings = w)
  }

  m <- collect(scf_mean(s, ~networth, by = ~tiny))
  expect_equal(sum(grepl("Group `a` has 4 households", m$warnings)), 1)
  expect_equal(m$value$results$n, c(4, 196))

  expect_warning(scf_median(s, ~networth, by = ~tiny, method = "stack"),
                 "Group `a` has 4 households")
  f <- collect(scf_freq(s, ~own, by = ~tiny))
  expect_equal(sum(grepl("Group `a` has 4 households", f$warnings)), 1)
})

test_that("survey warnings are reported once with their implicates", {
  s <- load_mock()
  s <- scf_update(s, one = ifelse(seq_along(age) == 1, 1, 0))
  w <- character(0)
  withCallingHandlers(
    scf_mean(s, ~networth, by = ~one),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  survey_w <- w[!grepl("households", w)]
  expect_true(length(survey_w) >= 1)
  expect_equal(length(survey_w), length(unique(survey_w)))
  expect_true(all(grepl("\\(implicates? [0-9, ]+\\)$", survey_w)))
})

test_that("scf_update ignores a trailing comma", {
  s <- load_mock()
  out <- scf_update(s, a = age + 1, )
  expect_equal(out$mi_design[[1]]$variables$a, s$mi_design[[1]]$variables$age + 1)
  expect_error(scf_update(s, age + 1), "must be named")
})

test_that("stack percentiles by group use the implicate method's groups", {
  s <- load_mock()
  st <- scf_percentile(s, ~networth, q = 0.5, by = ~edcl, method = "stack")$results
  im <- scf_percentile(s, ~networth, q = 0.5, by = ~edcl)$results
  expect_equal(st$group, im$group)

  v <- do.call(rbind, lapply(s$mi_design, function(d) d$variables))
  keep <- v$edcl == 1
  expect_equal(st$estimate[st$group == "1"],
               scf:::.scf_wtd_quantile(v$networth[keep], v$wgt[keep] / 5, 0.5))
})

test_that("scf_plot_cbar passes the median method on", {
  s <- load_mock()
  p <- scf_plot_cbar(s, ~networth, ~edcl, stat = "median", method = "stack")
  ref <- scf_median(s, ~networth, by = ~edcl, method = "stack")$results
  expect_equal(p$data$estimate, ref$estimate)
})

test_that("scf_glm needs a family", {
  s <- load_mock()
  expect_error(scf_glm(s, own ~ age), "`family` is required")
})

test_that("scf_ratio has a group column and SE() names it", {
  s <- load_mock()
  r <- scf_ratio(s, ~income, ~networth)
  expect_equal(r$results$group, "All")
  expect_named(SE(r), "income / networth")
  rb <- scf_ratio(s, ~income, ~networth, by = ~edcl)
  expect_equal(names(SE(rb)), as.character(rb$results$group))
})

test_that("stack results print without empty columns", {
  s <- load_mock()
  out <- capture.output(print(scf_median(s, ~networth, method = "stack")))
  expect_false(any(grepl("\bse\b", out[grepl("estimate", out)])))
  expect_true(any(grepl("Stack method", out)))
  expect_true(any(grepl("Stack method", capture.output(summary(scf_median(s, ~networth, method = "stack"))))))
})

test_that("scf_plot_smooth shares are out of all households", {
  s <- load_mock()
  all <- scf_plot_smooth(s, ~age, method = "lm", binwidth = 5)$data
  part <- scf_plot_smooth(s, ~age, method = "lm", binwidth = 5, xlim = c(0, 50))$data
  expect_lt(sum(part$percent), 100)
  expect_equal(sum(all$percent), 100)
})

test_that("scf_real converts the mock data's raw wage variable", {
  s <- load_mock()
  r <- scf_real(s, ~x5702, income = ~x5702)$mi_design[[1]]$variables
  expect_equal(r$x5702_real, r$x5702 * 4315 / 3992)
})
