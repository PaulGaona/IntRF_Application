source("./Analysis/Simulation/Settings.R")
source("./Functions/Res_Avg_Set.R")
set.seed(1)
# Data Generating
# scale all data by variances
# 500 obs
list_sims <- list(
  Setting1 = replicate(n = 100, expr = set1(n = 500,
                                            Xc_a = 10, Xc_b = 3,
                                            ec_a = 0, ec_b = 9,
                                            Xr_a = 1, Xr_b = 5,
                                            er_a = 0, er_b = .25)
  ),
  Setting2 = replicate(n = 100, expr = set2(n = 500,Xc_a = -5, Xc_b = 6,
                                            ec_a = 0, ec_b = 6,
                                            Xr_a = 1, Xr_b = 6,
                                            er_a = -5, er_b = 3)
  ),
  Setting3 = replicate(n = 100, expr = set3(n = 500,Xc_a = 5, Xc_b = 2,
                                            ec_a = 0, ec_b = 15,
                                            Xr_a = 0.2, Xr_b = 1,
                                            er_a = 0, er_b = 2.75)
  ),
  Setting4 = replicate(n = 100, expr = set4(n = 500,Xc_a = 10, Xc_b = 5,
                                            ec_a = 0, ec_b = 5,
                                            Xr_a = .25, Xr_b = 20,
                                            er_a = 0, er_b = 2)
  ),
  Setting5 = replicate(n = 100, expr = set5(n = 500,Xc_a = 5, Xc_b = 5,
                                            ec_a = 0, ec_b = 2,
                                            Xr_a = .25, Xr_b = 3,
                                            er_a = 0, er_b = 1.25)
  ),
  Setting6 = replicate(n = 100, expr = set6(n = 500,Xc_a = 3, Xc_b = 2,
                                            ec_a = 2, ec_b = 1.5,
                                            Xr_a = 0.5, Xr_b = 1.5,
                                            er_a = 0, er_b = 1.5)
  ),
  Setting7 = replicate(n = 100, expr = set7(n = 500,
                                            X1c_a = 5, X1c_b = 3,
                                            X1r_a = NA, X1r_b = NA,
                                            X2c_a = .5, X2c_b = .5,
                                            X2r_a = NA, X2r_b = NA,
                                            X3c_a = 10, X3c_b = 3.5,
                                            X3r_a = 10, X3r_b = 3,
                                            X4c_a = .5, X4c_b = 1.4,
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
                                            v2_a = 1, v2_b = 3)
  )
)
#
list_sims_df <- lapply(list_sims, data.frame)
list_sims_df2 <- lapply(list_sims_df, function(x){
  lapply(x, data.frame)
})
tic()
set.seed(1)
list_models <- lapply(list_sims_df2, function(x){
  lapply(x,auto_models)
})
comb_res_500d <- list_res(list_models)
toc()
comb_res_500
comb_res_500b
comb_res_500c
