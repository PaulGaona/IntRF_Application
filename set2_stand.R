#
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}
# data set
df_set2
# different forms of standardizing
# Our Method
thesis_stand <- as.data.frame(scale(df_set2,
                                    center = F,
                                    scale = apply(df_set2, 2, sd, na.rm = TRUE)
))
# root-mean-square standardization
rms_stand <- as.data.frame(scale(df_set2,
                                 center = F,
                                 scale = T))
# N(0,1) then min-max standadization
stand <- as.data.frame(scale(df_set2,
                             center = T,
                             scale = T))
minmax_stand <- norm_minmax(stand)
# split to training and testing
set.seed(1)
# create a sample index for training data
samp_ex <- sort(sample(nrow(df_set2), nrow(df_set2) * .8))
# Training data for each standardization
original_train <- df_set2[samp_ex,]
thesis_stand_train <- thesis_stand[samp_ex,]
rms_stand_train <- rms_stand[samp_ex,]
minmax_stand_train <- minmax_stand[samp_ex,]
# Test data for each standardization
original_test <- df_set2[-samp_ex,]
thesis_stand_test <- thesis_stand[-samp_ex,]
rms_stand_test <- rms_stand[-samp_ex,]
minmax_stand_test <- minmax_stand[-samp_ex,]
################################################################################
# Models
################################################################################
# ORIGINAL
################################################################################
# IRF
##################################
# Perform intrf model for Int RF and obtain results
set.seed(1)

irf_original <- IntRF.thesis::intrf(
  int_resp = original_train[c(3,4)],
  cent_pred = original_train[1],
  ran_pred = original_train[2],
  train = original_train,
  test = original_test,
  mtry_int = ncol(original_train[1])
)

irf_original_res <- irf_original$Results

# Obtain accuracy metrics for Int RF
met_irf_orig <- IntRF.thesis::acc_met(
  cent_pred = irf_original_res$center_pred,
  cent_act = irf_original_res$center_actual,
  ran_pred = irf_original_res$range_pred,
  ran_act = irf_original_res$range_actual,
  original_train[c(3,4)]
)
# Print output for Int RF accuracy metrics
met_irf_orig
##################################
#RF
##################################
# Perform randomForest model for RF and obtain predictions
set.seed(1)
crf_orig <- randomForest(Yc ~ Xc, data = original_train)
rrf_orig <- randomForest(Yr ~ Xr, data = original_train)
pcrf_orig <- predict(crf_orig, original_test)
prrf_orig <- predict(rrf_orig, original_test)

# Obtain accuracy metrics for RF
met_rf_orig <- acc_met(
  pcrf_orig,
  t(original_test[3]),
  prrf_orig,
  t(original_test[4]),
  original_train[c(3,4)]
)

# Print output for RF accuracy metrics
met_rf_orig
################################################################################
# THESIS_STAND
################################################################################
# IRF
##################################
# Perform intrf model for Int RF and obtain results
set.seed(1)

irf_thesis_stand <- IntRF.thesis::intrf(
  int_resp = thesis_stand_train[c(3,4)],
  cent_pred = thesis_stand_train[1],
  ran_pred = thesis_stand_train[2],
  train = thesis_stand_train,
  test = thesis_stand_test,
  mtry_int = ncol(thesis_stand_train[1])
)

irf_thesis_stand_res <- irf_thesis_stand$Results

# Obtain accuracy metrics for Int RF
met_irf_thesis <- IntRF.thesis::acc_met(
  cent_pred = irf_thesis_stand_res$center_pred,
  cent_act = irf_thesis_stand_res$center_actual,
  ran_pred = irf_thesis_stand_res$range_pred,
  ran_act = irf_thesis_stand_res$range_actual,
  thesis_stand_train[c(3,4)]
)
# Print output for Int RF accuracy metrics
met_irf_thesis
##################################
#RF
##################################
# Perform randomForest model for RF and obtain predictions
set.seed(1)
crf_thesis <- randomForest(Yc ~ Xc, data = dplyr::select(thesis_stand_train, -c(Xr,Yr)))
rrf_thesis <- randomForest(Yr ~ Xr, data = dplyr::select(thesis_stand_train, -c(Xc,Yc)))
pcrf_thesis <- predict(crf_thesis, thesis_stand_test)
prrf_thesis <- predict(rrf_thesis, thesis_stand_test)

# Obtain accuracy metrics for RF
met_rf_thesis <- acc_met(
  pcrf_thesis,
  t(thesis_stand_test[3]),
  prrf_thesis,
  t(thesis_stand_test[4]),
  thesis_stand_train[c(3,4)]
)

# Print output for RF accuracy metrics
met_rf_thesis
################################################################################
# RMS_STAND
################################################################################
# IRF
##################################
# Perform intrf model for Int RF and obtain results
set.seed(1)

irf_rms_stand <- IntRF.thesis::intrf(
  int_resp = rms_stand_train[c(3,4)],
  cent_pred = rms_stand_train[1],
  ran_pred = rms_stand_train[2],
  train = rms_stand_train,
  test = rms_stand_test,
  mtry_int = ncol(rms_stand_train[1])
)

irf_rms_stand_res <- irf_rms_stand$Results

# Obtain accuracy metrics for Int RF
met_irf_rms <- IntRF.thesis::acc_met(
  cent_pred = irf_rms_stand_res$center_pred,
  cent_act = irf_rms_stand_res$center_actual,
  ran_pred = irf_rms_stand_res$range_pred,
  ran_act = irf_rms_stand_res$range_actual,
  rms_stand_train[c(3,4)]
)
# Print output for Int RF accuracy metrics
met_irf_rms
##################################
#RF
##################################
# Perform randomForest model for RF and obtain predictions
set.seed(1)
crf_rms <- randomForest(Yc ~ Xc, data = rms_stand_train)
rrf_rms <- randomForest(Yr ~ Xr, data = rms_stand_train)
pcrf_rms <- predict(crf_rms, rms_stand_test)
prrf_rms <- predict(rrf_rms, rms_stand_test)

# Obtain accuracy metrics for RF
met_rf_rms <- acc_met(
  pcrf_rms,
  t(rms_stand_test[3]),
  prrf_rms,
  t(rms_stand_test[4]),
  rms_stand_train[c(3,4)]
)

# Print output for RF accuracy metrics
met_rf_rms
################################################################################
# MINMAX_STAND
################################################################################
# IRF
##################################
# Perform intrf model for Int RF and obtain results
set.seed(1)

irf_minmax_stand <- IntRF.thesis::intrf(
  int_resp = minmax_stand_train[c(3,4)],
  cent_pred = minmax_stand_train[1],
  ran_pred = minmax_stand_train[2],
  train = minmax_stand_train,
  test = minmax_stand_test,
  mtry_int = ncol(minmax_stand_train[1])
)

irf_minmax_stand_res <- irf_minmax_stand$Results

# Obtain accuracy metrics for Int RF
met_irf_minmax <- IntRF.thesis::acc_met(
  cent_pred = irf_minmax_stand_res$center_pred,
  cent_act = irf_minmax_stand_res$center_actual,
  ran_pred = irf_minmax_stand_res$range_pred,
  ran_act = irf_minmax_stand_res$range_actual,
  minmax_stand_train[c(3,4)]
)
# Print output for Int RF accuracy metrics
met_irf_minmax
##################################
#RF
##################################
# Perform randomForest model for RF and obtain predictions
set.seed(1)
crf_minmax <- randomForest(Yc ~ Xc, data = minmax_stand_train)
rrf_minmax <- randomForest(Yr ~ Xr, data = minmax_stand_train)
pcrf_minmax <- predict(crf_minmax, minmax_stand_test)
prrf_minmax <- predict(rrf_minmax, minmax_stand_test)

# Obtain accuracy metrics for RF
met_rf_minmax <- acc_met(
  pcrf_minmax,
  t(minmax_stand_test[3]),
  prrf_minmax,
  t(minmax_stand_test[4]),
  minmax_stand_train[c(3,4)]
)

# Print output for RF accuracy metrics
met_rf_minmax
#################################################################################
# RESULTS ALL TOGETHER
met_irf_orig
mean((irf_original_res$center_pred - irf_original_res$center_actual)^2)
mean((irf_original_res$range_pred - irf_original_res$range_actual)^2)
met_rf_orig
mean((pcrf_orig - t(original_test[3]))^2)
mean((prrf_orig - t(original_test[4]))^2)
met_irf_thesis
mean((irf_thesis_stand_res$center_pred - irf_thesis_stand_res$center_actual)^2)
mean((irf_thesis_stand_res$range_pred - irf_thesis_stand_res$range_actual)^2)
met_rf_thesis
mean((pcrf_orig - t(thesis_stand_test[3]))^2)
mean((prrf_orig - t(thesis_stand_test[4]))^2)
met_irf_rms
mean((irf_rms_stand_res$center_pred - irf_rms_stand_res$center_actual)^2)
mean((irf_rms_stand_res$range_pred - irf_rms_stand_res$range_actual)^2)
met_rf_rms
mean((pcrf_rms - t(rms_stand_test[3]))^2)
mean((prrf_rms - t(rms_stand_test[4]))^2)
met_irf_minmax
mean((irf_minmax_stand_res$center_pred - irf_minmax_stand_res$center_actual)^2)
mean((irf_minmax_stand_res$range_pred - irf_minmax_stand_res$range_actual)^2)
met_rf_minmax
mean((pcrf_minmax - t(minmax_stand_test[3]))^2)
mean((prrf_minmax - t(minmax_stand_test[4]))^2)
