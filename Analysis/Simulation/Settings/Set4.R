set4 <- function(n = NA,
                 Xc_a = NA, Xc_b = NA,
                 ec_a = NA, ec_b = NA,
                 Xr_a = NA, Xr_b = NA,
                 er_a = NA, er_b = NA,
                 cdf_a = NA, cdf_b = NA) {
  # center
  Xc <- rnorm(n, Xc_a, Xc_b) # predictor of center
  for (i in seq_along(Xc)) {
    while (Xc[i] < 0) {
      Xc[i] <- rnorm(1, Xc_a, Xc_b)
    }
  }
  epsc <- rnorm(n, ec_a, ec_b) # error of center
  # range
  Xr <- runif(n, Xr_a, Xr_b) # predictor of range
  epsr <- rnorm(n, er_a, er_b) # error of range
  # other parameters pt 2
  cdf <- pnorm(Xr, cdf_a, cdf_b)
  # equations
  Yc <- 10 * log(Xc) + 10 + epsc # response of center
  Yr <- 5 * sqrt(Xr) + epsr # response of range
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
}
