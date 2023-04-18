set5 <- function(n = NA,
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
  Yc <- 10 * sin(.15 * pi * Xc) + errc # response of center
  Yr <- Xr + .5 + errr # response of range
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0 || is.na(Yr[i])) {
      Yr[i] <- (runif(length(Yr[i]), Xr_a, Xr_b)) + .5 + rnorm(length(Yr[i]), er_a, er_b)
    }
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
}
