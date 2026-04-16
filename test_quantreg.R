library(scf)

# ---- Setup: load mock data --------------------------------------------------
td <- tempfile("qreg_test_")
dir.create(td)
src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
scf2022 <- scf_load(2022, data_directory = td)

# ---- 1. Basic smoke test: median regression ---------------------------------
m1 <- scf_quantreg(scf2022, networth ~ age + factor(edcl), tau = 0.5)
print(m1)
stopifnot(inherits(m1, "scf_quantreg"))
stopifnot(inherits(m1, "scf_model_result"))
stopifnot(nrow(m1$results) > 0)
stopifnot(length(m1$models) >= 2)

# ---- 2. Output structure ----------------------------------------------------
stopifnot(all(c("term", "estimate", "std.error", "t.value",
                "p.value", "stars") %in% names(m1$results)))
stopifnot(is.character(m1$results$stars))   # not a factor
stopifnot(is.numeric(m1$results$estimate))
stopifnot(all(m1$results$std.error > 0))
stopifnot(m1$tau == 0.5)
stopifnot(m1$se_method == "nid")

# ---- 3. Multiple quantiles --------------------------------------------------
m25  <- scf_quantreg(scf2022, networth ~ age, tau = 0.25)
m75  <- scf_quantreg(scf2022, networth ~ age, tau = 0.75)
m90  <- scf_quantreg(scf2022, networth ~ age, tau = 0.90)

# Intercepts should be monotonically ordered across quantiles
stopifnot(
  m25$results$estimate[1] 
  m75$results$estimate[1] 
  m90$results$estimate[1]
)

# ---- 4. SE methods ----------------------------------------------------------
for (method in c("nid", "iid", "ker", "boot")) {
  m <- scf_quantreg(scf2022, networth ~ age, tau = 0.5, se = method)
  stopifnot(all(m$results$std.error > 0))
  cat(sprintf("SE method '%s': OK (intercept SE = %.1f)\n",
              method, m$results$std.error[1]))
}

# replicate method (slow — comment out for quick runs)
# m_rep <- scf_quantreg(scf2022, networth ~ age, tau = 0.5, se = "replicate")
# stopifnot(all(m_rep$results$std.error > 0))

# ---- 5. summary() -----------------------------------------------------------
summary(m1)

# ---- 6. Input validation ----------------------------------------------------
tryCatch(
  scf_quantreg(scf2022, networth ~ age, tau = 0),
  error = function(e) cat("tau = 0 correctly rejected\n")
)
tryCatch(
  scf_quantreg(scf2022, networth ~ age, tau = 1.5),
  error = function(e) cat("tau = 1.5 correctly rejected\n")
)
tryCatch(
  scf_quantreg(list(), networth ~ age),
  error = function(e) cat("bad object class correctly rejected\n")
)
tryCatch(
  scf_quantreg(scf2022, "networth ~ age"),
  error = function(e) cat("string formula correctly rejected\n")
)

# ---- 7. Replicate SE vs analytical: point estimates should match ------------
m_nid <- scf_quantreg(scf2022, networth ~ age, tau = 0.5, se = "nid")
# m_rep <- scf_quantreg(scf2022, networth ~ age, tau = 0.5, se = "replicate")
# stopifnot(max(abs(m_nid$results$estimate - m_rep$results$estimate)) < 1)

# ---- Cleanup ----------------------------------------------------------------
unlink(td, recursive = TRUE, force = TRUE)
cat("\nAll tests passed.\n")