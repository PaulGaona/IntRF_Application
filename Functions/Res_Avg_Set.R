list_res <- function(list_data){
  sims <- vector("list", length(list_data[[1]]))
  mods <- vector("list", length(list_data[[1]][[1]]))
  sets <- vector("list", length(list_data))
  for(i in seq(sets)){
    for (j in seq(mods)) {
      for (k in seq(sims)) {
        sims[[k]]<-  do.call(cbind,list_data[[i]][[k]][[j]])
      }
      mods[[j]] <- sims
    }
    sets[[i]] <- mods
  }
  df <- lapply(sets, function(x1){
    lapply(x1, function(x2){
      do.call(rbind,x2)
    })
  })
  res <- lapply(df,function(x1){
    lapply(x1,function(x2){
      colMeans(x2)
    })
  })
  com_res <- lapply(res, function(x){
    do.call(cbind,x)
  })
  com_res2 <- lapply(com_res, function(x){
    round(x,3)
  })
  for (i in seq(com_res2)) {
    colnames(com_res2[[i]])<- c("IRF","Tree",
                               "RF", "CCRM")

  }
  names(com_res2) <- c("Setting 1","Setting 2",
                      "Setting 3","Setting 4",
                      "Setting 5","Setting 6",
                      "Setting 7")
  com_res2
}
