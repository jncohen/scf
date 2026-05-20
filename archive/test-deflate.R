# test_scf_deflate_nominal_replication.R

library(scf)

years <- seq(1989, 2022, by = 3)

# Official nominal-dollar figures, in thousands of dollars.
official_nominal <- c(
  `1989` = 1269.3,
  `1992` = 1252.7,
  `1995` = 1447.4,
  `1998` = 1944.5,
  `2001` = 2765.5,
  `2004` = 3118.7,
  `2007` = 3974.9,
  `2010` = 3681.9,
  `2013` = 3962.4,
  `2016` = 5310.9,
  `2019` = 5710.3,
  `2022` = 7771.3
)

results <- data.frame(
  year = years,
  official_nominal = as.numeric(official_nominal[as.character(years)]),
  scf_nominal = NA_real_,
  difference = NA_real_,
  pct_difference = NA_real_
)

for (i in seq_along(years)) {
  yr <- years[i]
  
  scf_download(yr)
  
  scf_year <- scf_load(yr)
  
  real_result <- scf_mean(
    scf_year,
    ~networth,
    by = ~nwcat
  )
  
  nominal_result <- scf_deflate(real_result, from_year = 2022)
  
  top10 <- nominal_result$results[
    as.character(nominal_result$results$group) == "5",
  ]
  
  results$scf_nominal[i] <- top10$estimate / 1000
  results$difference[i] <- results$scf_nominal[i] - results$official_nominal[i]
  results$pct_difference[i] <- 100 * results$difference[i] / results$official_nominal[i]
}

print(results)

tolerance <- 0.2

test_rows <- !is.na(results$official_nominal)

stopifnot(
  all(abs(results$difference[test_rows]) <= tolerance)
)

message("scf_deflate() nominal-dollar replication test passed.")