# Load necessary packages
library("ggplot2")
library("dplyr")
library("IntRF")
library("ggpubr")
library("scales")
library("gridExtra")

# Plot JPM vs DJI
jpm_p <- IntRF::int_plot(
  # Select relevant columns from prices dataframe
  int_data = prices %>%
    dplyr::select(c.JPM, r.JPM, c.DJI, r.DJI),
  title = " ",
  xlabel = "[DJI]",
  ylabel = "[JPM]"
) +
  ggplot2::scale_x_continuous(
    limits = c(
      min(prices$c.DJI) - 250, # Set lower limit of x-axis
      max(prices$c.DJI) + 250
    ), # Set upper limit of x-axis
    n.breaks = 6, # Set number of x-axis breaks to 10
    labels = scales::label_wrap(6) # Wrap x-axis labels to fit within 6 lines
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      min(prices$c.JPM) - 10, # Set lower limit of y-axis
      max(prices$c.JPM) + 10 # Set upper limit of y-axis
    ),
    n.breaks = 8 # Set number of y-axis breaks to 8
    ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
jpm_p

# Plot BA vs DJI
ba_p <- IntRF::int_plot(
  # Select relevant columns from prices dataframe
  prices %>%
    select(c.BA, r.BA, c.DJI, r.DJI),
  title = " ",
  xlabel = "[DJI]",
  ylabel = "[BA]"
) +
  ggplot2::scale_x_continuous(
    limits = c(
      min(prices$c.DJI) - 750, # Set lower limit of x-axis
      max(prices$c.DJI) + 250
    ), # Set upper limit of x-axis
    n.breaks = 6, # Set number of x-axis breaks to 10
    labels = scales::label_wrap(6) # Wrap x-axis labels to fit within 6 lines
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      min(prices$c.BA) - 10, # Set lower limit of y-axis
      max(prices$c.BA) + 10 # Set upper limit of y-axis
    ),
    n.breaks = 8 # Set number of y-axis breaks to 8
  ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
ba_p
# Plot GE vs DJI
ge_p <- IntRF::int_plot(
  # Select relevant columns from prices dataframe
  int_data = prices %>%
    select(c.GE, r.GE, c.DJI, r.DJI),
  title = " ",
  xlabel = "[DJI]",
  ylabel = "[GE]"
) +
  ggplot2::scale_x_continuous(
    limits = c(
      min(prices$c.DJI) - 250, # Set lower limit of x-axis
      max(prices$c.DJI) + 250 # Set upper limit of x-axis
    ),
    n.breaks = 6, # Set number of x-axis breaks to 10
    labels = scales::label_wrap(6) # Wrap x-axis labels to fit within 6 lines
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      min(prices$c.GE) - 10, # Set lower limit of y-axis
      max(prices$c.GE) + 10 # Set upper limit of y-axis
    ),
    n.breaks = 8 # Set number of y-axis breaks to 8
  ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
ge_p
# export figures

# export jpm
pdf(file = "./Analysis/Real/figs/jpm_fig.pdf")
jpm_p +
  theme(aspect.ratio=1)
dev.off()
# export ba
pdf(file = "./Analysis/Real/figs/ba_fig.pdf")
ba_p +
  theme(aspect.ratio=1)
dev.off()
# export ge
pdf(file = "./Analysis/Real/figs/ge_fig.pdf")
ge_p +
  theme(aspect.ratio=1)
dev.off()
# export all 3
 pdf(file = "./Analysis/Real/figs/stock_fig.pdf")
 gridExtra::grid.arrange(jpm_p, ba_p, ge_p,
  ncol = 1,
  widths = unit(10, c("cm")),
  heights = c(
    unit(10, c("cm")),
    unit(10, c("cm")),
    unit(10, c("cm"))
  )
 )
 dev.off()
