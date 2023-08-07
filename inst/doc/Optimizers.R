## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

old_options <- options(scipen = 999) # turn off scientific notation

## ----setup--------------------------------------------------------------------
library(gips)

## ----help, eval=FALSE---------------------------------------------------------
#  ?find_MAP()

## ----brute_force_1------------------------------------------------------------
perm_size <- 6
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- matrix(
  data = c(
    1.0, 0.8, 0.6, 0.4, 0.6, 0.8,
    0.8, 1.0, 0.8, 0.6, 0.4, 0.6,
    0.6, 0.8, 1.0, 0.8, 0.6, 0.4,
    0.4, 0.6, 0.8, 1.0, 0.8, 0.6,
    0.6, 0.4, 0.6, 0.8, 1.0, 0.8,
    0.8, 0.6, 0.4, 0.6, 0.8, 1.0
  ),
  nrow = perm_size, byrow = TRUE
) # the real covariance matrix, that we want to estimate, is invariant under permutation (1,2,3,4,5,6)
number_of_observations <- 13
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)
)

## ----brute_force_2------------------------------------------------------------
dim(Z)
number_of_observations <- nrow(Z) # 13
perm_size <- ncol(Z) # 6
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)

g_map <- find_MAP(g, optimizer = "brute_force")
g_map

## ----Metropolis_Hastings_1----------------------------------------------------
perm_size <- 70
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- (function(A) {
  t(A) %*% A
})(matrix(rnorm(perm_size * perm_size), nrow = perm_size))
# sigma_matrix is the real covariance matrix, that we want to estimate
number_of_observations <- 50
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)
)

## ----Metropolis_Hastings_2----------------------------------------------------
dim(Z)
number_of_observations <- nrow(Z) # 50
perm_size <- ncol(Z) # 70
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)
suppressMessages( # message from ggplot2
  plot(g, type = "heatmap") +
    ggplot2::scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70)) +
    ggplot2::scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50, 60, 70))
)
g_map <- find_MAP(g, max_iter = 150, optimizer = "Metropolis_Hastings")
g_map

## ----Metropolis_Hastings_3----------------------------------------------------
plot(g_map, type = "best", logarithmic_x = TRUE)

## ---- hill_climb_pseudocode, eval=FALSE---------------------------------------
#  hill_climb <- function(g, max_iter) {
#    perm <- g[[1]]
#    perm_log_f <- log_posteriori_of_gips(g)
#    perm_size <- attr(perm, "size")
#    S <- attr(g, "S")
#    number_of_observations <- attr(g, "number_of_observations")
#  
#    best_neighbor <- NULL
#    best_neighbor_log_f <- -Inf
#  
#    i <- 1
#  
#    while (perm_i_minus_1_log_f < perm_i_log_f && i < max_iter) {
#      best_neighbor <- NULL
#      best_neighbor_log_f <- -Inf
#  
#      for (j in 1:(perm_size - 1)) {
#        for (k in (j + 1):perm_size) {
#          t <- c(j, k)
#          neighbor <- gips:::compose_with_transposition(perm, t)
#          neighbor_log_f <- log_posteriori_of_gips(gips(
#            S, number_of_observations,
#            perm = neighbor
#          ))
#  
#          if (neighbor_log_f > best_neighbor_log_f) {
#            best_neighbor <- neighbor
#            best_neighbor_log_f <- neighbor_log_f
#          } # end if
#        } # end for k
#      } # end for j
#      i <- i + 1
#  
#      perm_i_minus_1_log_f <- perm_i_log_f
#  
#      perm_i <- best_neighbor
#      perm_i_log_f <- best_neighbor_log_f
#    } # end while
#  
#    return(perm_i)
#  }

## ----hill_climbing_1----------------------------------------------------------
perm_size <- 25
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- (function(A) {
  t(A) %*% A
})(matrix(rnorm(perm_size * perm_size), nrow = perm_size))
# sigma_matrix is the real covariance matrix, that we want to estimate
number_of_observations <- 20
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)
)

## ----hill_climbing_2----------------------------------------------------------
dim(Z)
number_of_observations <- nrow(Z) # 20
perm_size <- ncol(Z) # 25
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)
plot(g, type = "heatmap")
g_map <- find_MAP(g, max_iter = 2, optimizer = "hill_climbing")
g_map
plot(g_map, type = "best")

## ----continuing_1-------------------------------------------------------------
# the same code as for generating example for Metripolis-Hastings above

perm_size <- 70
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- (function(A) {
  t(A) %*% A
})(matrix(rnorm(perm_size * perm_size), nrow = perm_size))
# sigma_matrix is the real covariance matrix, that we want to estimate
number_of_observations <- 50
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)
)

dim(Z)
number_of_observations <- nrow(Z) # 50
perm_size <- ncol(Z) # 70
S <- cov(Z) # Assume we have to estimate the mean

## ----continuing_2-------------------------------------------------------------
g <- gips(S, number_of_observations)

g_map <- find_MAP(g, max_iter = 50, optimizer = "Metropolis_Hastings")
plot(g_map, type = "best")

## ----continuing_3-------------------------------------------------------------
g_map2 <- find_MAP(g_map, max_iter = 100, optimizer = "continue")
plot(g_map2, type = "best")

## ----options_back, include = FALSE--------------------------------------------
options(old_options) # back to the original options

