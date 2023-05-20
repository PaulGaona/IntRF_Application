################################################################################
# Sourcing functions required
source("./Functions/CCRM_Pred.R")
################################################################################
# stock locations
# DJI : 1,7
# JPM : 4, 10
# BA : 5, 11
# GE : 2, 8
################################################################################

# Define the stock locations for each stock in the dataset

################################################################################
# JPM ~ DJI
################################################################################
# Data from Stock_Code.R
# train data

# Load in the training and testing data for the selected stock
price_train_jpm <- price_train_stand[c(4, 10, 1, 7)]
price_test_jpm <- price_test_stand[c(4, 10, 1, 7)]

# Split the training data into the response variable (price) and
# predictor variables (center and range values)
yprice_train_jpm <- price_train_jpm[c(1, 2)]
xcprice_train_jpm <- price_train_jpm[3]
xrprice_train_jpm <- price_train_jpm[4]

# Split the testing data into the response variable (price) and predictor
# variables (center and range values)
yprice_test_jpm <- price_test_jpm[c(1, 2)]

################################################################################
# Int RF
# Package

# Use the IntRF package to train a random forest model for interval regression
set.seed(1)
int_price_rf_jpm <- IntRF::intrf(
  int_resp = yprice_train_jpm,
  cent_pred = xcprice_train_jpm,
  ran_pred = xrprice_train_jpm,
  train = price_train_jpm,
  test = price_test_jpm,
  mtry_int = ncol(xcprice_train_jpm)
)

# Extract the results from the model
res_jpm <- int_price_rf_jpm$Results

# Calculate accuracy metrics for the model
met_irf_jpm <- IntRF::acc_met(
  cent_pred = res_jpm$center_pred,
  cent_act = res_jpm$center_actual,
  ran_pred = res_jpm$range_pred,
  ran_act = res_jpm$range_actual,
  yprice_train_jpm
)

# Output the accuracy metrics
met_irf_jpm

################################################################################
# Int Tree
# IRT

# Use the mvpart function to train an interval regression tree model
set.seed(1)
ydat_jpm <- price_train_stand[names(yprice_train_jpm)]
irt_jpm <- IntRF::mvpart(data.matrix(ydat_jpm) ~ .,
                          data = price_train_jpm,
                          plot.add = FALSE,
                          xv = "none"
)

# Make predictions using the model on the testing data
ctpred_jpm <- predict(irt_jpm, newdata = price_test_jpm, type = "matrix")[, 1]
rtpred_jpm <- predict(irt_jpm, newdata = price_test_jpm, type = "matrix")[, 2]

# Calculate accuracy metrics for the model
met_tree_jpm <- IntRF::acc_met(
  ctpred_jpm,
  t(yprice_test_jpm[1]),
  rtpred_jpm,
  t(yprice_test_jpm[2]),
  yprice_train_jpm
)

# Output the accuracy metrics
met_tree_jpm

################################################################################
# RF model

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
crf_jpm <- randomForest::randomForest(c.JPM ~ .,
                                      data = dplyr::select(price_train_jpm, -c(r.JPM)))

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
rrf_jpm <- randomForest::randomForest(r.JPM ~ .,
                                      data = dplyr::select(price_train_jpm, -c(c.JPM)))

# Make predictions using the model on the testing data
pcrf_jpm <- predict(crf_jpm, price_test_jpm)
prrf_jpm <- predict(rrf_jpm, price_test_jpm)

# calculate accuracy metrics for the RF models
met_rf_jpm <- IntRF::acc_met(
  pcrf_jpm,
  t(yprice_test_jpm[1]),
  prrf_jpm,
  t(yprice_test_jpm[2]),
  yprice_train_jpm
)

# output accuracy metrics for RF models
met_rf_jpm

################################################################################
# ccrm model
set.seed(1)

# create CCRM model for the JPM stock price as a function of the DJI stock price
simccrm_jpm <- iRegression::ccrm("c.JPM ~ c.DJI",
                                 "r.JPM ~ r.DJI",
                                 data = price_train_jpm
)

# predict  JPM stock price using the CCRM model
pred_ccrm_jpm <- ccrm_pred(
  cent_coef = simccrm_jpm[[1]],
  cent_pred = as.matrix(price_test_jpm[3]),
  ran_coef = simccrm_jpm[[5]],
  ran_pred = as.matrix(price_test_jpm[4])
)

# calculate accuracy metrics for the CCRM model
met_ccrm_jpm <- IntRF::acc_met(
  t(pred_ccrm_jpm$center_pred),
  t(yprice_test_jpm[1]),
  t(pred_ccrm_jpm$range_pred),
  t(yprice_test_jpm[2]),
  yprice_train_jpm
)

# combine accuracy metrics from all models into a single data frame for comparison
combined_res_jpm <- data.frame(
  IRF = t(met_irf_jpm),
  IRT = t(met_tree_jpm),
  RF = t(met_rf_jpm),
  CCRM = t(met_ccrm_jpm)
)

combined_res_jpm
################################################################################

# Define the stock locations for each stock in the dataset

################################################################################
# BA ~ DJI
################################################################################
# Data from Stock_Code.R
# train data

# Load in the training and testing data for the selected stock
price_train_ba <- price_train_stand[c(5, 11, 1, 7)]
price_test_ba <- price_test_stand[c(5, 11, 1, 7)]

# Split the training data into the response variable (price) and
# predictor variables (center and range values)
yprice_train_ba <- price_train_ba[c(1, 2)]
xcprice_train_ba <- price_train_ba[3]
xrprice_train_ba <- price_train_ba[4]

# Split the testing data into the response variable (price) and predictor
# variables (center and range values)
yprice_test_ba <- price_test_ba[c(1, 2)]

################################################################################
# Int RF
# Package

# Use the IntRF package to train a random forest model for interval regression
set.seed(1)
int_price_rf_ba <- IntRF::intrf(
  int_resp = yprice_train_ba,
  cent_pred = xcprice_train_ba,
  ran_pred = xrprice_train_ba,
  train = price_train_ba,
  test = price_test_ba,
  mtry_int = ncol(xcprice_train_ba)
)

# Extract the results from the model
res_ba <- int_price_rf_ba$Results

# Calculate accuracy metrics for the model
met_irf_ba <- IntRF::acc_met(
  cent_pred = res_ba$center_pred,
  cent_act = res_ba$center_actual,
  ran_pred = res_ba$range_pred,
  ran_act = res_ba$range_actual,
  yprice_train_ba
)

# Output the accuracy metrics
met_irf_ba

################################################################################
# Int Tree
# IRT

# Use the mvpart function to train an interval regression tree model
set.seed(1)
ydat_ba <- price_train_ba[names(yprice_train_ba)]
irt_ba <- IntRF::mvpart(data.matrix(ydat_ba) ~ .,
                         data = price_train_ba,
                         plot.add = FALSE,
                         xv = "none"
)
# Make predictions using the model on the testing data
ctpred_ba <- predict(irt_ba, newdata = price_test_ba, type = "matrix")[, 1]
rtpred_ba <- predict(irt_ba, newdata = price_test_ba, type = "matrix")[, 2]

# Calculate accuracy metrics for the model
met_tree_ba <- IntRF::acc_met(
  ctpred_ba,
  t(yprice_test_ba[1]),
  rtpred_ba,
  t(yprice_test_ba[2]),
  yprice_train_ba
)

# Output the accuracy metrics
met_tree_ba

################################################################################
# RF model

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
crf_ba <- randomForest::randomForest(c.BA ~ .,
                                     data = dplyr::select(price_train_ba, -c(r.BA)))

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
rrf_ba <- randomForest::randomForest(r.BA ~ .,
                                     data = dplyr::select(price_train_ba, -c(c.BA)))

# Make predictions using the model on the testing data
pcrf_ba <- predict(crf_ba, price_test_ba)
prrf_ba <- predict(rrf_ba, price_test_ba)

# calculate accuracy metrics for the RF models
met_rf_ba <- IntRF::acc_met(
  pcrf_ba,
  t(yprice_test_ba[1]),
  prrf_ba,
  t(yprice_test_ba[2]),
  yprice_train_ba
)

# output accuracy metrics for RF models
met_rf_ba

################################################################################
# ccrm model

set.seed(1)
# create CCRM model for the JPM stock price as a function of the DJI stock price
simccrm_ba <- iRegression::ccrm("c.BA ~ c.DJI",
  "r.BA ~ r.DJI",
  data = price_train_ba
)

# predict  BA stock price using the CCRM model
pred_ccrm_ba <- ccrm_pred(
  cent_coef = simccrm_ba[[1]],
  cent_pred = as.matrix(price_test_ba[3]),
  ran_coef = simccrm_ba[[5]],
  ran_pred = as.matrix(price_test_ba[4])
)

# calculate accuracy metrics for the CCRM model
met_ccrm_ba <- IntRF::acc_met(
  t(pred_ccrm_ba$center_pred),
  t(yprice_test_ba[1]),
  t(pred_ccrm_ba$range_pred),
  t(yprice_test_ba[2]),
  yprice_train_ba
)

################################################################################

# combine accuracy metrics from all models into a single data frame for comparison
combined_res_ba <- data.frame(
  IRF = t(met_irf_ba),
  IRT = t(met_tree_ba),
  RF = t(met_rf_ba),
  CCRM = t(met_ccrm_ba)
)

combined_res_ba

################################################################################

# Define the stock locations for each stock in the dataset

################################################################################
# GE ~ DJI
################################################################################

# Define which stock we will be analyzing

################################################################################
# Data from Stock_Code.R
# train data

# Load in the training and testing data for the selected stock
price_train_ge <- price_train_stand[c(2, 8, 1, 7)]
price_test_ge <- price_test_stand[c(2, 8, 1, 7)]

# Split the training data into the response variable (price) and
# predictor variables (center and range values)
yprice_train_ge <- price_train_ge[c(1, 2)]
xcprice_train_ge <- price_train_ge[3]
xrprice_train_ge <- price_train_ge[4]

# Split the testing data into the response variable (price) and predictor
# variables (center and range values)
yprice_test_ge <- price_test_ge[c(1, 2)]

################################################################################
# Int RF
# Package

# Use the IntRF package to train a random forest model for interval regression
set.seed(1)
int_price_rf_ge <- IntRF::intrf(
  int_resp = yprice_train_ge,
  cent_pred = xcprice_train_ge,
  ran_pred = xrprice_train_ge,
  train = price_train_ge,
  test = price_test_ge,
  mtry_int = ncol(xcprice_train_ge)
)

# Extract the results from the model
res_ge <- int_price_rf_ge$Results

# Calculate accuracy metrics for the model
met_irf_ge <- IntRF::acc_met(
  cent_pred = res_ge$center_pred,
  cent_act = res_ge$center_actual,
  ran_pred = res_ge$range_pred,
  ran_act = res_ge$range_actual,
  yprice_train_ge
)

# Output the accuracy metrics
met_irf_ge

################################################################################
# Int Tree
# IRT

# Use the mvpart function to train an interval regression tree model
set.seed(1)
ydat_ge <- price_train_ge[names(yprice_train_ge)]
irt_ge <- IntRF::mvpart(data.matrix(ydat_ge) ~ .,
  data = price_train_ge,
  plot.add = FALSE,
  xv = "none"
)

# Make predictions using the model on the testing data
ctpred_ge <- predict(irt_ge, newdata = price_test_ge, type = "matrix")[, 1]
rtpred_ge <- predict(irt_ge, newdata = price_test_ge, type = "matrix")[, 2]

# Calculate accuracy metrics for the model
met_tree_ge <- IntRF::acc_met(
  ctpred_ge,
  t(yprice_test_ge[1]),
  rtpred_ge,
  t(yprice_test_ge[2]),
  yprice_train_ge
)

# Output the accuracy metrics
met_tree_ge

################################################################################
# RF model

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
crf_ge <- randomForest::randomForest(c.GE ~ .,
                                     data = dplyr::select(price_train_ge, -c(r.GE)))

set.seed(1)
# create random forest model for JPM stock price using all other variables
# except actual stock price
rrf_ge <- randomForest::randomForest(r.GE ~ .,
                                     data = dplyr::select(price_train_ge, -c(c.GE)))

# Make predictions using the model on the testing data
pcrf_ge <- predict(crf_ge, price_test_ge)
prrf_ge <- predict(rrf_ge, price_test_ge)

# calculate accuracy metrics for the RF models
met_rf_ge <- IntRF::acc_met(
  pcrf_ge,
  t(yprice_test_ge[1]),
  prrf_ge,
  t(yprice_test_ge[2]),
  yprice_train_ge
)

# output accuracy metrics for RF models
met_rf_ge

################################################################################
# ccrm model

set.seed(1)
# create CCRM model for the JPM stock price as a function of the DJI stock price
simccrm_ge <- iRegression::ccrm("c.GE ~ c.DJI",
  "r.GE ~ r.DJI",
  data = price_train_ge
)

# predict  JPM stock price using the CCRM model
pred_ccrm_ge <- ccrm_pred(
  cent_coef = simccrm_ge[[1]],
  cent_pred = as.matrix(price_test_ge[3]),
  ran_coef = simccrm_ge[[5]],
  ran_pred = as.matrix(price_test_ge[4])
)

# calculate accuracy metrics for the CCRM model
met_ccrm_ge <- IntRF::acc_met(
  t(pred_ccrm_ge$center_pred),
  t(yprice_test_ge[1]),
  t(pred_ccrm_ge$range_pred),
  t(yprice_test_ge[2]),
  yprice_train_ge
)

################################################################################

# combine accuracy metrics from all models into a single data frame for comparison
combined_res_ge <- data.frame(
  IRF = t(met_irf_ge),
  IRT = t(met_tree_ge),
  RF = t(met_rf_ge),
  CCRM = t(met_ccrm_ge)
)

combined_res_ge

################################################################################

################################################################################
round(combined_res_jpm, 2)
round(combined_res_ba, 2)
round(combined_res_ge, 2)
