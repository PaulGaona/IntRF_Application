# CCRM Predictions
ccrm_pred <- function(cent_coef,
                      cent_pred,
                      ran_coef,
                      ran_pred
){
  cent_mat <- cbind(1,as.matrix(cent_pred))
  ran_mat <- cbind(1,as.matrix(ran_pred))
  # center predictions
  pc_ccrm <- cent_mat %*% cent_coef
  # range predictions
  pr_ccrm <- ran_mat %*% ran_coef
  data.frame(center_pred = pc_ccrm, range_pred = pr_ccrm)
}
