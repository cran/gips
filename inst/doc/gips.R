## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

old_options <- options(scipen = 999) # turn off scientific notation

## ----symvariant_matrix, echo=FALSE--------------------------------------------
X <- matrix(c(
  1, 2, 3,
  2, 4, 2,
  3, 2, 1
), byrow = TRUE, ncol = 3, dimnames = list(
  c("X1", "X2", "X3"),
  c("X1", "X2", "X3")
))
heatmap(X, Rowv = NA, Colv = NA, main = "", symm = TRUE)

## ----change_D_matrix_example0, include=FALSE----------------------------------
plot_cosmetic_modifications <- function(gg_plot_object) {
  my_col_names <- names(DAAG::oddbooks[, c(1, 2, 3)])

  suppressMessages( # message from ggplot2
    out <- gg_plot_object +
      ggplot2::scale_x_continuous(
        labels = my_col_names,
        breaks = 1:3
      ) +
      ggplot2::scale_y_reverse(
        labels = my_col_names,
        breaks = 1:3
      ) +
      ggplot2::theme(
        title = ggplot2::element_text(face = "bold", size = 18),
        axis.text.y = ggplot2::element_text(face = "bold", size = 17),
        axis.text.x = ggplot2::element_text(face = "bold", size = 17)
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#F0EA3E", mid = "#A41836", high = "#95E956",
        midpoint = 1.239099
      )
  )

  out +
    ggplot2::geom_text(ggplot2::aes(label = round(covariance, 1)),
      fontface = "bold", size = 8
    ) +
    ggplot2::theme(legend.position = "none")
}

## ----change_D_matrix_example1-------------------------------------------------
library(gips)

Z <- DAAG::oddbooks[, c(1, 2, 3)]

## ----change_D_matrix_example1_1-----------------------------------------------
Z$height <- Z$height / sqrt(2)

## ----change_D_matrix_example1_2-----------------------------------------------
number_of_observations <- nrow(Z) # 12
p <- ncol(Z) # 3

S <- cov(Z)
round(S, 1)
g <- gips(S, number_of_observations)
plot_cosmetic_modifications(plot(g, type = "heatmap")) +
  ggplot2::ggtitle("Standard, MLE estimator\nof a covariance matrix")

## ----change_D_matrix_example2-------------------------------------------------
g_map <- find_MAP(g,
  optimizer = "brute_force",
  return_probabilities = TRUE, save_all_perms = TRUE
)

g_map
get_probabilities_from_gips(g_map)

## ----change_D_matrix_example4-------------------------------------------------
plot_cosmetic_modifications(plot(g_map, type = "heatmap"))
round(project_matrix(S, g_map), 1)

## ----toy_example_data_making, include = TRUE----------------------------------
p <- 5
number_of_observations <- 4
mu <- runif(p, -10, 10) # Assume we don't know the mean
sigma_matrix <- matrix(c(
  8.4, 4.1, 1.9, 1.9, 1.9,
  4.1, 3.5, 0.3, 0.3, 0.3,
  1.9, 0.3, 1,   0.8, 0.8,
  1.9, 0.3, 0.8, 1,   0.8,
  1.9, 0.3, 0.8, 0.8, 1
), ncol = p)
# sigma_matrix is a matrix invariant under permutation (3,4,5)
toy_example_data <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations,
    mu = mu, Sigma = sigma_matrix
  )
)

## ----toy_example_data_show1---------------------------------------------------
library(gips)

toy_example_data

dim(toy_example_data)
number_of_observations <- nrow(toy_example_data) # 4
p <- ncol(toy_example_data) # 5

S <- cov(toy_example_data)

sum(eigen(S)$values > 0.00000001)

## ----toy_example_data_show2---------------------------------------------------
g <- gips(S, number_of_observations)

plot(g, type = "heatmap")

## ----toy_example_data_show3---------------------------------------------------
g_map <- find_MAP(g,
  optimizer = "brute_force",
  return_probabilities = TRUE, save_all_perms = TRUE
)

plot(g_map, type = "heatmap")

## ----toy_example_data_show4---------------------------------------------------
g_map

## ----toy_example_data_show5---------------------------------------------------
get_probabilities_from_gips(g_map)

## ----toy_example_data_show6---------------------------------------------------
summary(g_map)$n0 # n0 = 4 <= 4 = number_of_observations

S_projected <- project_matrix(S, g_map)
S_projected
sum(eigen(S_projected)$values > 0.00000001)

## ----options_back, include = FALSE--------------------------------------------
options(old_options) # back to the original options

