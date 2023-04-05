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
}
