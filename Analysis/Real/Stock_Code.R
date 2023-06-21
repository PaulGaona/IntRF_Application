install.packages("iRegression")
install.packages("randomForest")
install.packages("tidyquant")
install.packages("tidyverse")
library(devtools)
devtools::install_github("PaulGaona/IntRF")
# packages needed
# provides functions for retrieving and analyzing financial data
library(tidyquant)
# provides a suite of tools for data manipulation and visualization
library(tidyverse)

# functions for code comparisons
# provides a function for the interaction random forest
library(IntRF)
# provides a function for random forests
library(randomForest)
# provides a function for interactive regression
library(iRegression)

# vector of stock indexes
stock_ind <- c("^DJI", "GE", "PG", "JPM", "BA", "MSFT")

# obtain stock price data
prices <- tq_get(stock_ind,
  from = "2012-01-01",
  to = "2017-12-30",
  get = "stock.prices"
)

# Select variables, convert to centers and ranges, pivot df
prices <- prices %>%
  select(symbol, high, low) %>%
  mutate(
    c = (high + low) / 2, # calculate center
    r = (high - low) / 2 # calculate range
  ) %>%
  select(symbol, c, r) %>%
  pivot_wider(
    names_from = symbol, names_sep = ".", values_from = c(c, r),
    values_fn = list
  ) %>%
  unchop(everything()) %>%
  rename(c.DJI = `c.^DJI`) %>%
  rename(r.DJI = `r.^DJI`)
# split to training and testing
set.seed(1)
# create a sample index for training data
samp <- sort(sample(nrow(prices), nrow(prices) * .8))
price_train <- prices[samp, ] # select training data
price_test <- prices[-samp, ] # select testing data
price_sd <- apply(price_train, 2, sd, na.rm = TRUE)
# standardizing training and testing
price_train_stand <- sweep(price_train,2,price_sd,FUN="/")
price_test_stand <- sweep(price_test,2,price_sd,FUN="/")
