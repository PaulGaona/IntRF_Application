set2 <- function(n = NA,
                 Xc_a = NA, Xc_b = NA,
                 ec_a = NA, ec_b = NA,
                 Xr_a = NA, Xr_b = NA,
                 er_a = NA, er_b = NA) {
  # center
  Xc <- rnorm(n, Xc_a, Xc_b) # predictor of center
  epsc <- rnorm(n, ec_a, ec_b) # error of center
  # range
  Xr <- runif(n, Xr_a, Xr_b) # predictor of range
  epsr <- rnorm(n, er_a, er_b) # error of range
  # equations
  Yc <- -Xc + 50 + epsc # response of center
  Yr <- -2*Xr + 25 + epsr # response of range
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
}
