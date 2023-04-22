auto_models <- function(df, dat_perc = .8){
  # data split
  dfsample <- sample(x = 1:nrow(df), size = floor(dat_perc*nrow(df)), replace = F)
  dummy_train <- df[dfsample,]
  dummy_test <- df[-dfsample,]
  dummy_sd <- apply(dummy_train, 2, sd, na.rm = TRUE)
  # actual testing and training split with standardization
  dftrain <- sweep(dummy_train, 2, dummy_sd, FUN="/")
  dftest <- sweep(dummy_test, 2, dummy_sd, FUN="/")
  # train
  ytrain <- dftrain %>% dplyr::select(starts_with("Y"))
  xctrain <- dftrain %>% dplyr::select(starts_with("Xc"))
  xrtrain <- dftrain %>% dplyr::select(starts_with("Xr"))
  # test
  ytest <- dftest %>% dplyr::select(starts_with("Y"))
  yctest <- ytest %>% dplyr::select(contains("c"))
  yrtest <- ytest %>% dplyr::select(contains("r"))
  xctest <- dftest %>% dplyr::select(starts_with("Xc"))
  xrtest <- dftest %>% dplyr::select(starts_with("Xr"))
  ##############################################################################
  #irf
  irf <- IntRF.thesis::intrf(int_resp = ytrain,
                             cent_pred = xctrain,
                             ran_pred = xrtrain,
                             train = dftrain,
                             test = dftest,
                             mtry_int = ncol(xctrain))
  # obtain results
  irf_res <- as.data.frame(irf$Results)
  # obtain accuracy metrics
  met_irf <- IntRF.thesis::acc_met(cent_pred = irf_res$center_pred,
                                      cent_act = irf_res$center_actual,
                                      ran_pred = irf_res$range_pred,
                                      ran_act = irf_res$range_actual,
                                      ytrain)
  ##############################################################################
  #irt
  ydat <- dftrain[names(ytrain)]
  irt <- IntRF.thesis::mvpart(data.matrix(ydat) ~ .,
                        data = dftrain,
                        plot.add = FALSE,
                        xv = "none"
  )
  # predictions
  ctpred <- predict(irt, newdata = dftest, type = "matrix")[, 1]
  rtpred <- predict(irt, newdata = dftest, type = "matrix")[, 2]
  # obtain accuracy metrics
  met_tree <- IntRF.thesis::acc_met(ctpred,
                                    t(ytest[1]),
                                    rtpred,
                                    t(ytest[2]),
                                    ytrain)
  ##############################################################################
  #rf
  cent_df <- dftrain %>% dplyr::select(!contains("Yr"))
  cent_form <- as.formula(paste(ytrain %>% dplyr::select(contains("c")) %>% names(.),
                               paste("."),
                               sep = " ~ "))
  ran_df <- dftrain %>% dplyr::select(!contains("Yc"))
  ran_form <- as.formula(paste(ytrain %>% dplyr::select(contains("r")) %>% names(.),
                               paste("."),
                               sep = " ~ "))
  crf <- randomForest::randomForest(cent_form,
                                    data = cent_df)
  rrf <- randomForest::randomForest(ran_form,
                                    data = ran_df)
  # predictions
  pcrf <- predict(crf,dftest)
  prrf <- predict(rrf,dftest)
  # obtain accuracy metrics
  met_rf <- IntRF.thesis::acc_met(pcrf,
                                  t(ytest[1]),
                                  prrf,
                                  t(ytest[2]),
                                  ytrain)
  ##############################################################################
  #ccrm
  cent_resp_names <- ytrain %>% dplyr::select(contains("c")) %>% names(.)
  cent_pred_names <- paste(names(xctrain), collapse = " + ")
  cent_func <- as.formula(paste(c(cent_resp_names,
                                  cent_pred_names),
                                collapse = " ~ ")
  )
  ran_resp_names <- ytrain %>% dplyr::select(contains("r")) %>% names(.)
  ran_pred_names <- paste(names(xrtrain), collapse = " + ")
  ran_func <- as.formula(paste(c(ran_resp_names,
                                 ran_pred_names),
                               collapse = " ~ ")
  )
  simccrm <- iRegression::ccrm(cent_func,
                               ran_func,
                               data = dftrain)
  pred_ccrm <- ccrm_pred(cent_coef = simccrm[[1]],
                         cent_pred = as.matrix(xctest),
                         ran_coef = simccrm[[5]],
                         ran_pred = as.matrix(xrtest)
  )
  # obtain accuracy metrics
  met_ccrm <- IntRF.thesis::acc_met(pred_ccrm$center_pred,
                                    t(yctest),
                                    pred_ccrm$range_pred,
                                    t(yrtest),
                                    ytrain)

  all_met <- list(IRF = met_irf,
                  Tree = met_tree,
                  RF = met_rf,
                  CCRM = met_ccrm
  )
}
