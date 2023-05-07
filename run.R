# Loading libraries
message("Loading libraries")
library(purrr)
library(tidyr)
library(ggplot2)

# Sourcing probability calculation script
source("src/compute_probabilities.R")

windowsFonts("tw_cen_mt" = windowsFont("Tw Cen MT"))

# Obtaining probabilities for different values of n
message("Calculating probabilities for different input values")
x <- 3:366
y <- purrr::map(x, f) %>%
  purrr::transpose() %>%
  purrr::map(.f = purrr::flatten_dbl)

# Arranging the data to plot
message("Arranging probability data to create visualisation")
df <- data.frame(n = x,
                 p1 = y$`Prob. no two people have the same birthday`,
                 p2 = y$`Prob. at least two people have the same birthday`,
                 p3 = y$`Prob. exactly two people have the same birthday`) %>%
  tidyr::gather(key = "Type", value = "Probability", -n)

# Plot of probabilities vs n
message("Creating visualisation")
plt <- ggplot2::ggplot(data = df,
                mapping = ggplot2::aes(x = n, y = Probability, colour = Type)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::geom_point(alpha = 0) +
  ggplot2::labs(x = "No. of people",
                y = "Probability") +
  ggplot2::theme_minimal(base_family = "tw_cen_mt") +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.justification = c(0, 1)) +
  ggplot2::scale_colour_hue(labels = c("p1" = "Prob. no two people\nhave the same birthday",
                                       "p3" = "Prob. exactly two people\nhave the same birthday",
                                       "p2" = "`Prob. at least two people\nhave the same birthday`"),
                            guide = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
  ggplot2::scale_x_continuous(breaks = seq(0, 400, by = 50))

message("Saving visualisation")
ggplot2::ggsave(filename = "plots/lineplot.png", plot = plt, device = "png", width = 6.39, height = 3.91)