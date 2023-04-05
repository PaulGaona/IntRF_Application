################################################################################
# Setting 1
################################################################################
set1 <- function(n = NA,
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
  Yc <- 2 * Xc + 15 + epsc # response of center
  for (i in seq_along(Yc)) {
    while (Yc[i] < 0) {
      Yc[i] <- 2 * rnorm(length(Yc[i]), Xc_a, Xc_b) + 15 + rnorm(length(Yc[i]), ec_a, ec_b)
    }
  }
  Yr <- .25 * Xr + epsr # response of range
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0) {
      Yr[i] <- .25 * runif(length(Yc[i]), Xr_a, Xr_b) + rnorm(length(Yc[i]), er_a, er_b)
    }
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 2
################################################################################
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
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 3
################################################################################
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
  Yc <- Xc^2 + 20 + epsc # response of center
  for (i in seq_along(Yc)) {
    while (Yc[i] < 0) {
      Yc[i] <- (.5 * rnorm(length(Yc[i]), Xc_a, Xc_b))^2 + 20 + rnorm(length(Yc[i]), ec_a, ec_b)
    }
  }
  Yr <- 1 / Xr^2 + epsr
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0) {
      Yr[i] <- 1 / ((runif(length(Yr[i]), Xr_a, Xr_b))^2) + rnorm(length(Yr[i]), er_a, er_b)
    }
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 4
################################################################################
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
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 5
################################################################################
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
  Yr <- 2 * Xr + .5 + errr # response of range
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0 || is.na(Yr[i])) {
      Yr[i] <- 2 * (runif(length(Yr[i]), Xr_a, Xr_b)) + .5 + rnorm(length(Yr[i]), er_a, er_b)
    }
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 6
################################################################################
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
  Yc <- 10 * sin(.5 * pi * Xc) + errc # response of center
  Yr <- 2 * abs(-.3 * Xr * Xc + .5) + errr # response of range
  for (i in seq_along(Yr)) {
    while (Yr[i] < 0 || is.na(Yr[i])) {
      Yr[i] <- 2 * abs(-.3 * runif(length(Yr[i]), Xr_a, Xr_b) * rnorm(length(Yr[i]), Xc_a, Xc_b) + .5)
    }
    +rnorm(length(Yr[i]), er_a, er_b)
  }
  # storing center and range values for predictor and response
  df <- data.frame(Xc, Xr, Yc, Yr)
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
################################################################################
# Setting 7
################################################################################
# Multivariate Setting
set7 <- function(n = NA,
                 X1c_a = NA, X1c_b = NA,
                 X1r_a = NA, X1r_b = NA,
                 X2c_a = NA, X2c_b = NA,
                 X2r_a = NA, X2r_b = NA,
                 X3c_a = NA, X3c_b = NA,
                 X3r_a = NA, X3r_b = NA,
                 X4c_a = NA, X4c_b = NA,
                 X4r_a = NA, X4r_b = NA,
                 X5c_a = NA, X5c_b = NA,
                 X5r_a = NA, X5r_b = NA,
                 ec_a = NA, ec_b = NA,
                 er_a = NA, er_b = NA,
                 tau1_a = NA, tau1_b = NA,
                 tau2_a = NA, tau2_b = NA,
                 un1_a = NA, un1_b = NA,
                 un2_a = NA, un2_b = NA,
                 v1_a = NA, v1_b = NA,
                 v2_a = NA, v2_b = NA) {
  # Given Variables
  tau1 <- rnorm(n, tau1_a, tau1_b)
  tau2 <- rnorm(n, tau2_a, tau2_b)
  un1 <- runif(n, un1_a, un1_b)
  un2 <- runif(n, un2_a, un2_b)
  V1 <- un1 + exp(-.5 * rgamma(n, v1_a, v1_b)) + tau1
  V2 <- un2 + exp(-.5 * rbeta(n, v2_a, v2_b)) + tau2
  # centers and ranges
  # Var1
  Xc1 <- rnorm(n, X1c_a, X1c_b)
  Xr1 <- (2 * V1) / (1 + V1)
  # Var2
  Xc2 <- rbeta(n, X2c_a, X2c_b)
  Xr2 <- (3 * V1) / (1 + V2)
  # Var3
  Xc3 <- rnorm(n, X3c_a, X3c_b)
  Xr3 <- rnorm(n, X3r_a, X3r_b)
  # Var4
  Xc4 <- runif(n, X4c_a, X4c_b)
  Xr4 <- runif(n, X4r_a, X4r_b)
  # Var5
  Xc5 <- rnorm(n, X5c_a, X5c_b)
  Xr5 <- rbeta(n, X5r_a, X5r_b)
  # error
  ec <- rnorm(n, ec_a, ec_b)
  er <- rnorm(n, er_a, ec_b)
  # Predictor Variables
  Yc <- ((Xc1 + (Xc1)^2) * (Xc2 + (Xc2)^2)) - ((Xc3 + (Xc3)^2) * (Xc4 + (Xc4)^2)) - Xc5 + ec
  Yr <- (.2 * Xr2)^2 + .1 * Xr3 - 5 * (Xr1 * Xr4 + Xr5) + 4 + er
  df <- data.frame(Xc1, Xr1, Xc2, Xr2, Xc3, Xr3, Xc4, Xr4, Xc5, Xr5, Yc, Yr)
  df_s <- as.data.frame(scale(df,
    center = F,
    scale = apply(df, 2, sd, na.rm = TRUE)
  ))
  df_s
}
