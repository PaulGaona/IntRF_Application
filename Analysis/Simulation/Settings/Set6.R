set6 <- function(n = NA,
                 Xc_a = NA, Xc_b = NA,
                 ec_a = NA, ec_b = NA,
                 Xr_a = NA, Xr_b = NA,
                 er_a = NA, er_b = NA) {
  # center
  Xc <- rnorm(n, Xc_a, Xc_b) # predictor of center
  errc <- rnorm(n, ec_a, ec_b) # error of center
  # range
  Xr <- runif(n, Xr_a, Xr_b) # predictor of range
  errr <- rnorm(n, er_a, er_b) # error of range
  # equations
  Yc <- -(Xc - 8)^2 + 32 + errc # response of center
  for (i in seq_along(Yc)) {
    while (Yc[i] < 0) {
      Yc[i] <- -(rnorm(length(Yc[i]), Xc_a, Xc_b) - 8)^2 + 32 + rnorm(length(Yc[i]), ec_a, ec_b)
    }
  }
  Yr <- (.5*exp(Xr))*(sqrt(Xc)/8) + errr # response of range
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
}
