# Elements from Predicting_DJI.R
res <- intpriceRF$Results
plot(res$center_actual ~ res$center_pred, main = "Predicted Center Vs Actual Center")
plot(res$range_actual ~ res$range_pred , main = "Predicted Range Vs Actual Range")
plot(res$range_actual ~ res$center_pred, main = "Predicted Center Vs Actual Range")
plot(res$center_actual ~ res$range_pred, main = "Predicted Range Vs Actual Center")
