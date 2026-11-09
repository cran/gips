## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

old_options <- options(scipen = 999) # turn off scientific notation

## ----symvariant_matrix, echo=FALSE, fig.width=3.5, fig.height=3.5, fig.show='hold', out.width='49%'----
X <- matrix(c(
  4, 2, 3,
  2, 7, 2,
  3, 2, 4
), byrow = TRUE, ncol = 3, dimnames = list(
  c("X1", "X2", "X3"),
  c("X1", "X2", "X3")
))
plot_invariant_matrix <- function(S, title) {
  gips:::pretty_plot_matrix(S, title = title) +
    ggplot2::geom_text(
      ggplot2::aes(label = covariance, colour = covariance > mean(range(covariance))),
      fontface = "bold", size = 6
    ) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "white", "TRUE" = "black"), guide = "none"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12, colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(angle = 0),
      legend.position = "none"
    )
}

plot_invariant_matrix(X, "Original order")
plot_invariant_matrix(X[c(3, 2, 1), c(3, 2, 1)], "After swapping X1 and X3")

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
  ggplot2::ggtitle("Sample covariance matrix")

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
toy_probabilities <- get_probabilities_from_gips(g_map)
toy_probabilities

## ----toy_example_data_show6---------------------------------------------------
summary(g_map)$n0 # compare n0 with number_of_observations

S_projected <- project_matrix(S, g_map)
S_projected
sum(eigen(S_projected)$values > 0.00000001)

## ----multi_sample_example_data------------------------------------------------
p <- 5
n1 <- 20
n2 <- 23

sigma_matrix_1 <- matrix(c(
  1.50,  0.95,  0.95,  0.30, -0.05,
  0.95,  1.50,  0.95,  0.30, -0.05,
  0.95,  0.95,  1.50,  0.30, -0.05,
  0.30,  0.30,  0.30,  2.40,  0.05,
 -0.05, -0.05, -0.05,  0.05,  0.60
), nrow = p, byrow = TRUE)

sigma_matrix_2 <- matrix(c(
  2.10,  1.40,  1.40, -0.35,  0.55,
  1.40,  2.10,  1.40, -0.35,  0.55,
  1.40,  1.40,  2.10, -0.35,  0.55,
 -0.35, -0.35, -0.35,  0.80, -0.30,
  0.55,  0.55,  0.55, -0.30,  2.80
), nrow = p, byrow = TRUE)

data_sample_1 <- withr::with_seed(21, 
  MASS::mvrnorm(n1, mu = rep(0, p), Sigma = sigma_matrix_1)
)

data_sample_2 <- withr::with_seed(37, 
  MASS::mvrnorm(n2, mu = rep(0, p), Sigma = sigma_matrix_2)
)

S1 <- cov(data_sample_1)
S2 <- cov(data_sample_2)

variable_names <- c("he", "ha", "hi", "hu", "hy")
dimnames(S1) <- list(variable_names, variable_names)
dimnames(S2) <- list(variable_names, variable_names)

S_by_group <- list("Sample 1" = S1, "Sample 2" = S2)

## ----multi_sample_plot--------------------------------------------------------
g_multi <- gips(S_by_group, c(n1, n2))

plot(g_multi, type = "MLE")

## ----multi_sample_example_fit-------------------------------------------------
g_multi_map <- find_MAP(g_multi,
  optimizer = "brute_force",
  return_probabilities = TRUE, save_all_perms = TRUE,
  show_progress_bar = FALSE
)

g_multi_map
multi_probabilities <- get_probabilities_from_gips(g_multi_map)
head(multi_probabilities, 5)

## ----multi_sample_example_project---------------------------------------------
S_multi_projected <- project_matrix(S_by_group, g_multi_map)

round(S_multi_projected[[1]], 2)
round(S_multi_projected[[2]], 2)

plot(g_multi_map, type = "MLE")

## ----multi_sample_single_group_comparison-------------------------------------
g_1_map <- find_MAP(gips(S1, n1),
  optimizer = "BF",
  return_probabilities = TRUE,
  save_all_perms = TRUE,
  show_progress_bar = FALSE
)

g_2_map <- find_MAP(gips(S2, n2),
  optimizer = "BF",
  return_probabilities = TRUE,
  save_all_perms = TRUE,
  show_progress_bar = FALSE
)

group_1_probabilities <- get_probabilities_from_gips(g_1_map)
group_2_probabilities <- get_probabilities_from_gips(g_2_map)

head(group_1_probabilities, 5)
head(group_2_probabilities, 5)

## ----options_back, include = FALSE--------------------------------------------
options(old_options) # back to the original options

