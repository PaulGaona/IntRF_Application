# Load necessary functions and libraries
source("./Analysis/Simulation/Settings.R")
source("./Functions/Auto_Models.R")
source("./Functions/Dat_Split.R")
source("./Functions/Res_Avg_Set.R")
source("./Functions/CCRM_Pred.R")
library("tidyverse")
library("IntRF.thesis")

# Set up simulation parameters
n_vec <- c(100, 250, 500) # Sample sizes to simulate
mc_sim <- 100 # Number of Monte Carlo simulations to run

# Initialize empty list to store results
all_res <- vector("list", length(n_vec))
names(all_res) <- paste("n =", n_vec)

# Loop over all elements of the n_vec vector
for (i in seq(all_res)) {
  #
  set.seed(1)
  # Generate a list of simulated data sets for each of the 7 different settings
  list_sims <- list(
    Setting1 = replicate(n = mc_sim, expr = set1(
      n = n_vec[i],
      Xc_a = 12, Xc_b = 3,
      ec_a = 0, ec_b = 3,
      Xr_a = 1, Xr_b = 12,
      er_a = 0, er_b = .5
    )),
    Setting2 = replicate(n = mc_sim, expr = set2(
      n = n_vec[i],
      Xc_a = -5, Xc_b = 10,
      ec_a = 0, ec_b = 6,
      Xr_a = 1, Xr_b = 6,
      er_a = 0, er_b = 3
    )),
    Setting3 = replicate(n = mc_sim, expr = set3(
      n = n_vec[i],
      Xc_a = 5, Xc_b = 2,
      ec_a = 0, ec_b = 15,
      Xr_a = 0.2, Xr_b = 1,
      er_a = 0, er_b = 2.75
    )),
    Setting4 = replicate(n = mc_sim, expr = set4(
      n = n_vec[i],
      Xc_a = 10, Xc_b = 5,
      ec_a = 0, ec_b = 5,
      Xr_a = .25, Xr_b = 20,
      er_a = 0, er_b = 2
    )),
    Setting5 = replicate(n = mc_sim, expr = set5(
      n = n_vec[i],
      Xc_a = 5, Xc_b = 5,
      ec_a = 0, ec_b = 2,
      Xr_a = .25, Xr_b = 3,
      er_a = 0, er_b = 1.25
    )),
    Setting6 = replicate(n = mc_sim, expr = set6(
      n = n_vec[i],
      Xc_a = 3, Xc_b = 2,
      ec_a = 2, ec_b = 1.5,
      Xr_a = 0.5, Xr_b = 1.5,
      er_a = 0, er_b = 1.5
    )),
    Setting7 = replicate(n = mc_sim, expr = set7(
      n = n_vec[i],
      X1c_a = 5, X1c_b = 3,
      X1r_a = NA, X1r_b = NA,
      X2c_a = .5, X2c_b = .5,
      X2r_a = NA, X2r_b = NA,
      X3c_a = 10, X3c_b = 3.5,
      X3r_a = 10, X3r_b = 3,
      X4c_a = .5, X4c_b = 1.5,
      X4r_a = 2.5, X4r_b = 3.5,
      X5c_a = 8, X5c_b = 3.5,
      X5r_a = 2, X5r_b = 5,
      ec_a = 0, ec_b = 1,
      er_a = -3, er_b = 15,
      tau1_a = 0, tau1_b = .2,
      tau2_a = 0, tau2_b = .2,
      un1_a = 0, un1_b = .5,
      un2_a = 0, un2_b = .5,
      v1_a = 3, v1_b = 2,
      v2_a = 1, v2_b = 3
    ))
  )

  # Convert the list of simulations into a list of data frames
  list_sims_df <- lapply(list_sims, data.frame)

  # Convert each element of the list of data frames into a nested
  # list of data frames
  list_sims_df2 <- lapply(list_sims_df, function(x) {
    lapply(x, data.frame)
  })

  # Call the auto_models function on each nested list of data frames
  # and store the results in a list
  set.seed(1)
  list_models <- lapply(list_sims_df2, function(x) {
    lapply(x, auto_models)
  })

  # Store the results for the i-th simulation in the i-th element of
  # the all_res list
  all_res[[i]] <- list_res(list_models)
}
all_res
