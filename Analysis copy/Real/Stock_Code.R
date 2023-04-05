# packages needed
# provides functions for retrieving and analyzing financial data
library(tidyquant)
# provides a suite of tools for data manipulation and visualization
library(tidyverse)

# functions for code comparisons
# provides a function for the interaction random forest
library(IntRF.thesis)
# provides a function for recursive partitioning
library(mvpart)
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

# scale data by individual variable variance
s_prices <- as.data.frame(scale(prices,
  center = FALSE,
  scale = apply(prices, 2, sd, na.rm = TRUE)
))

# split to training and testing
set.seed(1)
# create a sample index for training data
samp <- sort(sample(nrow(s_prices), nrow(s_prices) * .8))
price_train <- s_prices[samp, ] # select training data
price_test <- s_prices[-samp, ] # select testing data

# train data
# select dependent variables for training data
yprice_train <- price_train[c(1, 7)]
# select independent variables related to center for training data
xcprice_train <- price_train[2:6]
# select independent variables related to range for training data
xrprice_train <- price_train[8:12]

# test data
# select dependent variables for testing data
yprice_test <- price_test[c(1, 7)]
