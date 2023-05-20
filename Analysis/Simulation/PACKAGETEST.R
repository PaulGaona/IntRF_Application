# Load necessary packages
library("tidyverse")
# provides a suite of tools for data manipulation and visualization
library("ggplot2")
library("ggpubr")
library("scales")
library("gridExtra")
# functions for code comparisons
# provides a function for the interval-valued random forest
library("IntRF")

name(prices)
head(prices)

# Plot GE vs DJI
GE_p <- IntRF::int_plot(
  # Select relevant columns from prices dataframe
  int_data = prices %>%
    select(c.GE, r.GE, c.DJI, r.DJI),
  title = "GE vs DJI",
  xlabel = "[DJI]",
  ylabel = "[GE]"
) +
  ggplot2::scale_x_continuous(
    limits = c(
      min(prices$c.DJI) - 750, # Set lower limit of x-axis
      max(prices$c.DJI) + 750
    ), # Set upper limit of x-axis
    n.breaks = 10, # Set number of x-axis breaks to 10
    labels = scales::label_wrap(6) # Wrap x-axis labels to fit within 6 lines
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      min(prices$c.GE) - 10, # Set lower limit of y-axis
      max(prices$c.GE) + 10 # Set upper limit of y-axis
    ),
    n.breaks = 8 # Set number of y-axis breaks to 8
  )
