set3 <- function(n = NA,
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
  Yc <- .5 * Xc^2 + 20 + epsc # response of center
  for (i in seq_along(Yc)) {
    while (Yc[i] < 0) {
      Yc[i] <- .5 * (rnorm(length(Yc[i]), Xc_a, Xc_b))^2 + 20 + rnorm(length(Yc[i]), ec_a, ec_b)
    }
  }
  Yr <- 1/(20*Xr^2)+ 1 + epsr
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0) {
      Yr[i] <- 1 / (20*(runif(length(Yr[i]), Xr_a, Xr_b))^2) + rnorm(length(Yr[i]), er_a, er_b)
    }
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
}
