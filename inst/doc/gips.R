## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = TRUE
)

old_options <- options(scipen=999) # turn off scientific notation

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

## ----toy_example_data_making, include = TRUE----------------------------------
perm_size <- 5
mu <- runif(5, -10, 10) # Assume we don't know the mean
sigma_matrix <- matrix(c(7.5, 5,   0.5, 0.5, 0.5,
                         5,   4.5, 0.3, 0.3, 0.3,
                         0.5, 0.3, 1,   0.8, 0.8,
                         0.5, 0.3, 0.8, 1,   0.8,
                         0.5, 0.3, 0.8, 0.8, 1), ncol=5)
# sigma_matrix is a matrix invariant under permutation (3,4,5)
number_of_observations <- 4
toy_example_data <- withr::with_seed(1234,
    code = MASS::mvrnorm(number_of_observations,
                         mu = mu, Sigma = sigma_matrix)
)

## ----toy_example_data_show1---------------------------------------------------
library(gips)

toy_example_data

dim(toy_example_data)
number_of_observations <- nrow(toy_example_data) # 4
perm_size <- ncol(toy_example_data) # 5

S <- cov(toy_example_data)

sum(eigen(S)$values > 0.00000001)

## ----toy_example_data_show2, dependson="toy_example_data_show1"---------------
g <- gips(S = S, number_of_observations = nrow(toy_example_data))

plot(g, type = "heatmap")

## ----toy_example_data_show3, dependson="toy_example_data_show2"---------------
g_map <- find_MAP(g, optimizer = "brute_force",
                  return_probabilities = TRUE, save_all_perms = TRUE)

plot(g_map, type = "heatmap")

## ----toy_example_data_show4, dependson="toy_example_data_show3"---------------
g_map

## ----toy_example_data_show5, dependson="toy_example_data_show4"---------------
get_probabilities_from_gips(g_map)

## ----toy_example_data_show6, dependson="toy_example_data_show5"---------------
S_projected <- project_matrix(S, g_map[[1]])
sum(eigen(S_projected)$values > 0.00000001)

## ----change_D_matrix_example1-------------------------------------------------
library(gips)

Z <-DAAG::oddbooks[,c(1,2,3)]

number_of_observations <- nrow(Z) # 12
p <- ncol(Z) # 3

S <- cov(Z)
S
g <- gips(S, number_of_observations, D_matrix=diag(p)) # the default D_matrix
plot(g, type = "heatmap")

## ----change_D_matrix_example2, dependson="change_D_matrix_example1"-----------
g_map <- find_MAP(g, optimizer = "brute_force",
                  return_probabilities = TRUE, save_all_perms = TRUE)

g_map
get_probabilities_from_gips(g_map)

## ----change_D_matrix_example3, dependson="change_D_matrix_example1"-----------
g <- gips(S, number_of_observations, D_matrix=0.05*diag(p))
g_map <- find_MAP(g, optimizer = "brute_force",
                  return_probabilities = TRUE, save_all_perms = TRUE)

g_map
get_probabilities_from_gips(g_map)

## ----options_back, include = FALSE--------------------------------------------
options(old_options)

