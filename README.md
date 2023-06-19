# IntRf_Application

## 1 Introduction

The goal of IntRF_Application is to run tree-based regression for interval-valued data and provide a more detailed explanation on how to use the code for simulations and a real data example. I plan on providing a step-by-step process for each file.

All files were used at one point for my simulation and real data analysis of my Ms Thesis at Utah State University (INSERT LINK HERE). There is no guarantee that every package will be cran compliant, for new versions of R (2022.12.0).

As the project title states this is a project based on the application of a package I developed IntRf.

Please refer to the README of IntRF for details about ***installation***, ***minimal example***, and ***source code*** at
[PaulGaona/IntRF](https://github.com/PaulGaona/IntRF)

## 2 Motivation

We are motivated in implementing a new machine learning model for interval-valued data. An introduction to interval-valued data can be found at [Diamond 1990](https://www.sciencedirect.com/science/article/pii/0022247X9090353H) and [Billard 2000](https://link.springer.com/chapter/10.1007/978-3-642-59789-3_58). As well as a more recent journal introducing machine learning for interval-valued data[Chacon 2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8067438/).

Our aim is to develop a new model that builds on the natural structure of an interval $\[x^{L}, x^{U}\]$ = $\[x^{C} \pm x^{r} \]$. Please see (INSERT LINKE HERE) for a more detailed discussion on the preliminaries and the application of the model.

## 3 Folders

  ### 3.1 Functions

  ### 3.2 Analysis
This folder provides all files that were used for the application of real data as well as simulated data.
   #### I Real

   ##### A [Stock_Code.R](https://github.com/PaulGaona/IntRf_Application/blob/main/Analysis/Real/Stock_Code.R)

Install and load all necessary packages. *tidyquant* and *tidyverse* are used to obtain and prepare stock market data for model building. *iRegression*, *randomForests* and *IntRF* will be used to compare eachother.
``` r
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
```

We then collect and prepare the data. Only keeping the ***high*** and ***low*** columns and then converting to centers and range.

```r
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
```
Data is then partitioned into ***training*** and ***testing*** data, all data is standardized by the $\sigma$ of the ***training*** data. It then changes **price_train** into **price_train_stand**, the same goes for **price_test**.

``` r
# split to training and testing
set.seed(1)
# create a sample index for training data
samp <- sort(sample(nrow(prices), nrow(prices) * .8))
price_train <- prices[samp, ] # select training data
price_test <- prices[-samp, ] # select testing data
price_sd <- apply(price_train, 2, sd, na.rm = TRUE)
# standardizing training and testing
price_train_stand <- sweep(price_train,2,price_sd,FUN="/")
price_test_stand <- sweep(price_train,2,price_sd,FUN="/")
```
  ##### B [Predicting_DJI.R](https://github.com/PaulGaona/IntRf_Application/blob/main/Analysis/Real/Predicting_DJI.R)

  #### II Simulations

### 3.3 Old (Deprecated)

## 4. Application
