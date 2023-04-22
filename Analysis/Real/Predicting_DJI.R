# Data from Stock_Code.R

# Load required packages and set seed value
library(IntRF.thesis)
library(randomForest)
library(iRegression)

# Perform intrf model for Int RF and obtain results
set.seed(1)

int_price_rf <- IntRF.thesis::intrf(
  int_resp = yprice_train,
  cent_pred = xcprice_train,
  ran_pred = xrprice_train,
  train = price_train_stand ,
  test = price_test_stand ,
  mtry_int = ncol(xcprice_train)
)

res <- int_price_rf$Results

# Obtain accuracy metrics for Int RF
met_irf_dji <- IntRF.thesis::acc_met(
  cent_pred = res$center_pred,
  cent_act = res$center_actual,
  ran_pred = res$range_pred,
  ran_act = res$range_actual,
  yprice_train
)

# Print output for Int RF accuracy metrics
met_irf_dji

# Perform mvpart model for Int Tree and obtain predictions
set.seed(1)

ydat <- price_train_stand[names(yprice_train)]
irt <- IntRF.thesis::mvpart(data.matrix(ydat) ~ .,
  data = price_train_stand ,
  plot.add = FALSE,
  xv = "none"
)
ctpred <- predict(irt, newdata = price_test_stand, type = "matrix")[, 1]
rtpred <- predict(irt, newdata = price_test_stand, type = "matrix")[, 2]

# Obtain accuracy metrics for Int Tree
met_tree_dji <- IntRF.thesis::acc_met(
  ctpred,
  t(yprice_test[1]),
  rtpred,
  t(yprice_test[2]),
  yprice_train
)

# Print output for Int Tree accuracy metrics
met_tree_dji

# Perform randomForest model for RF and obtain predictions
set.seed(1)
crf <- randomForest(c.DJI ~ ., data = dplyr::select(price_train_stand , -c(r.DJI)))
rrf <- randomForest(r.DJI ~ ., data = dplyr::select(price_train_stand , -c(c.DJI)))
pcrf <- predict(crf, price_test_stand)
prrf <- predict(rrf, price_test_stand)

# Obtain accuracy metrics for RF
met_rf_dji <- acc_met(
  pcrf,
  t(yprice_test[1]),
  prrf,
  t(yprice_test[2]),
  yprice_train
)

# Print output for RF accuracy metrics
met_rf_dji

# Perform ccrm model and obtain predictions
set.seed(1)

simccrm <- ccrm("c.DJI ~ c.GE+c.PG+c.JPM+c.BA+c.MSFT",
  "r.DJI ~ r.GE+r.PG+r.JPM+r.BA+r.MSFT",
  data = price_train_stand
)

pred_ccrm <- ccrm_pred(
  cent_coef = simccrm[[1]],
  cent_pred = as.matrix(price_test_stand[2:6]),
  ran_coef = simccrm[[5]],
  ran_pred = as.matrix(price_test_stand[8:12])
)

# Obtain accuracy metrics for ccrm
met_ccrm_dji <- acc_met(
  pred_ccrm$center_pred,
  t(yprice_test[1]),
  pred_ccrm$range_pred,
  t(yprice_test[2]),
  yprice_train
)

# Combine accuracy metrics from all models into a single dataframe
combined_res_dji <- data.frame(
  IRF = t(met_irf_dji),
  IRT = t(met_tree_dji),
  RF = t(met_rf_dji),
  CCRM = t(met_ccrm_dji)
)

# output results
round(combined_res_dji, 3)
