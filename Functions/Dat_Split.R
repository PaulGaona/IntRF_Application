# testing and training data split
dat_split <- function(df){
  sample <- sample(x = 1:nrow(df), size = floor(.8*nrow(df)), replace = F)
  list(Train = df[sample,], Test = df[-sample,])
}
