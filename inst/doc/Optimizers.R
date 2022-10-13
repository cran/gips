## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = TRUE
)

old_options <- options(scipen=999) # turn off scientific notation

## ----setup--------------------------------------------------------------------
library(gips)

## ----help, cache = TRUE, eval=FALSE-------------------------------------------
#  ?find_MAP

## ----Metropolis_Hastings_1, cache=TRUE----------------------------------------
perm_size <- 70
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- (function(x){t(x) %*% x})(matrix(rnorm(perm_size*perm_size), nrow=perm_size)) # the real covariance matrix, that we want to estimate
number_of_observations <- 50
Z <- MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)

## ----Metropolis_Hastings_2, cache=TRUE, dependson="Metropolis_Hastings_1"-----
dim(Z)
number_of_observations <- nrow(Z) # 50
perm_size <- ncol(Z) # 70
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)
suppressMessages( # message from ggplot2
  plot(g, type = "heatmap") +
    ggplot2::scale_x_continuous(breaks = c(1,10,20,30,40,50,60,70)) +
    ggplot2::scale_y_reverse(breaks = c(1,10,20,30,40,50,60,70))
)
g_map <- find_MAP(g, max_iter = 10, optimizer = "Metropolis_Hastings")
g_map

## ----Metropolis_Hastings_3, cache=TRUE, dependson="Metropolis_Hastings_2"-----
plot(g_map, type = "both", logarithmic_x = TRUE)

## ---- hill_climb_pseudocode, eval=FALSE---------------------------------------
#  hill_climb <- function(g, max_iter) {
#    perm <- g[[1]]
#    perm_posteriori <- log_posteriori_of_gips(g)
#    perm_size <- attr(perm, "size")
#    S <- attr(g, "S")
#    number_of_observations <- attr(g, "number_of_observations")
#  
#    best_neighbor <- NULL
#    best_neighbor_posteriori <- -Inf
#  
#    i <- 1
#  
#    while (best_neighbor_posteriori > perm_posteriori && i < max_iter) {
#      best_neighbor <- NULL
#      best_neighbor_posteriori <- -Inf
#  
#      for (j in 1:(perm_size-1)) {
#        for (k in (j + 1):perm_size) {
#          neighbor <- gips:::compose_with_transposition(perm, c(j, k))
#          neighbor_posteriori <- log_posteriori_of_gips(gips(
#            S,
#            number_of_observations,
#            perm = neighbor
#          ))
#  
#          if (neighbor_posteriori > best_neighbor_posteriori) {
#            best_neighbor <- neighbor
#            best_neighbor_posteriori <- neighbor_posteriori
#          } # end if
#        } # end for k
#      } # end for j
#      i <- i + 1
#    } # end while
#  
#    return(best_neighbor)
#  }

## ----hill_climbing_1, cache=TRUE----------------------------------------------
perm_size <- 25
mu <- runif(perm_size, -10, 10) # Assume we don't know the mean
sigma_matrix <- (function(x){t(x) %*% x})(matrix(rnorm(perm_size*perm_size), nrow=perm_size)) # the real covariance matrix, that we want to estimate
number_of_observations <- 20
Z <- MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)

## ----hill_climbing_2, cache=TRUE, dependson="hill_climbing_1"-----------------
dim(Z)
number_of_observations <- nrow(Z) # 20
perm_size <- ncol(Z) # 25
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)
plot(g, type = "heatmap")
g_map <- find_MAP(g, max_iter = 2, optimizer = "hill_climbing")
g_map
plot(g_map, type = "both")

## ----brute_force_1, cache=TRUE------------------------------------------------
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
Z <- MASS::mvrnorm(number_of_observations, mu = mu, Sigma = sigma_matrix)

## ----brute_force_2, cache=TRUE, dependson="brute_force_1"---------------------
dim(Z)
number_of_observations <- nrow(Z) # 13
perm_size <- ncol(Z) # 6
S <- cov(Z) # Assume we have to estimate the mean

g <- gips(S, number_of_observations)

g_map <- find_MAP(g, optimizer = "brute_force")
g_map

## ----options_back, include = FALSE--------------------------------------------
options(old_options)

