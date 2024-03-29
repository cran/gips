## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk[["set"]](
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ----setup--------------------------------------------------------------------
library(gips)

## ----th1_1--------------------------------------------------------------------
p <- 6
S <- matrix(c(
  1.1, 0.9, 0.8, 0.7, 0.8, 0.9,
  0.9, 1.1, 0.9, 0.8, 0.7, 0.8,
  0.8, 0.9, 1.1, 0.9, 0.8, 0.7,
  0.7, 0.8, 0.9, 1.1, 0.9, 0.8,
  0.8, 0.7, 0.8, 0.9, 1.1, 0.9,
  0.9, 0.8, 0.7, 0.8, 0.9, 1.1
), nrow = p)

## ----th1_2, echo=FALSE--------------------------------------------------------
gips:::pretty_plot_matrix(S, title = "S matrix")

## ----th1_3--------------------------------------------------------------------
g_perm <- gips_perm("(1,2,3,4,5,6)", p)
U_Gamma <- prepare_orthogonal_matrix(g_perm)

block_decomposition <- t(U_Gamma) %*% S %*% U_Gamma
round(block_decomposition, 5)

## ----th1_4, echo=FALSE--------------------------------------------------------
gips:::pretty_plot_block_matrix(S, g_perm, title = "block_decomposition matrix")

## ----th1_5--------------------------------------------------------------------
p <- 6
S <- matrix(c(
  1.2, 0.9, 0.9, 0.4, 0.2, 0.1,
  0.9, 1.2, 0.9, 0.1, 0.4, 0.2,
  0.9, 0.9, 1.2, 0.2, 0.1, 0.4,
  0.4, 0.1, 0.2, 1.2, 0.9, 0.9,
  0.2, 0.4, 0.1, 0.9, 1.2, 0.9,
  0.1, 0.2, 0.4, 0.9, 0.9, 1.2
), nrow = p)

## ----th1_6, echo=FALSE--------------------------------------------------------
gips:::pretty_plot_matrix(S, title = "S matrix")

## ----th1_7--------------------------------------------------------------------
g_perm <- gips_perm("(1,2,3)(4,5,6)", p)
U_Gamma <- prepare_orthogonal_matrix(g_perm)

block_decomposition <- t(U_Gamma) %*% S %*% U_Gamma
round(block_decomposition, 5)

## ----th1_8, echo=FALSE--------------------------------------------------------
gips:::pretty_plot_block_matrix(S, g_perm, title = "block_decomposition matrix")

## ----def3_0, echo=FALSE-------------------------------------------------------
p <- 6
n <- 10
withr::with_seed(2022,
  code = Z <- matrix(runif(n * p, min = -10, max = 10), ncol = p)
)
Z[, 1] <- 2 * Z[, 1]
S <- t(Z) %*% Z / n

## ----def3_1-------------------------------------------------------------------
round(S, 2)

## ----def3_2, echo=FALSE-------------------------------------------------------
gips:::pretty_plot_matrix(S, title = "S matrix")

## ----def3_3-------------------------------------------------------------------
S_projected <- project_matrix(S, perm = "(1,2)(3,4,5,6)")
round(S_projected, 2)

## ----def3_4, echo=FALSE-------------------------------------------------------
gips:::pretty_plot_matrix(S_projected, title = "S_projected matrix")

## ----n0_2---------------------------------------------------------------------
g1 <- gips(S, n, perm = "(1,2,3,4,5,6)", was_mean_estimated = FALSE)
summary(g1)$n0
g2 <- gips(S, n, perm = "(1,2)(3,4,5,6)", was_mean_estimated = FALSE)
summary(g2)$n0

## ----n0_1---------------------------------------------------------------------
S <- cov(Z)
g1 <- gips(S, n, perm = "(1,2,3,4,5,6)", was_mean_estimated = TRUE)
summary(g1)$n0
g2 <- gips(S, n, perm = "(1,2)(3,4,5,6)", was_mean_estimated = TRUE)
summary(g2)$n0

## ----example2_readme1---------------------------------------------------------
# Prepare model, multivariate normal distribution
p <- 6
number_of_observations <- 4
mu <- numeric(p)
sigma_matrix <- matrix(
  data = c(
    1.05, 0.8, 0.6, 0.4, 0.6, 0.8,
    0.8, 1.05, 0.8, 0.6, 0.4, 0.6,
    0.6, 0.8, 1.05, 0.8, 0.6, 0.4,
    0.4, 0.6, 0.8, 1.05, 0.8, 0.6,
    0.6, 0.4, 0.6, 0.8, 1.05, 0.8,
    0.8, 0.6, 0.4, 0.6, 0.8, 1.05
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4,5,6)

# Generate example data from a model:
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations,
    mu = mu, Sigma = sigma_matrix
  )
)
# End of prepare model

## ---- example2_readme2--------------------------------------------------------
dim(Z)
number_of_observations <- nrow(Z) # 4
p <- ncol(Z) # 6

# Calculate the covariance matrix from the data (assume the mean is 0):
S <- (t(Z) %*% Z) / number_of_observations

# Make the gips object out of data:
g <- gips(S, number_of_observations, was_mean_estimated = FALSE)

g_map <- find_MAP(g, optimizer = "brute_force")
print(g_map)

S_projected <- project_matrix(S, g_map)

## ---- example2_readme3, echo=FALSE--------------------------------------------
gips:::pretty_plot_matrix(S_projected, title = "S_projected matrix")

## ----example3_1---------------------------------------------------------------
# Prepare model, multivariate normal distribution
p <- 6
number_of_observations <- 7
mu <- numeric(p)
sigma_matrix <- matrix(
  data = c(
    1.05, 0.8, 0.6, 0.4, 0.6, 0.8,
    0.8, 1.05, 0.8, 0.6, 0.4, 0.6,
    0.6, 0.8, 1.05, 0.8, 0.6, 0.4,
    0.4, 0.6, 0.8, 1.05, 0.8, 0.6,
    0.6, 0.4, 0.6, 0.8, 1.05, 0.8,
    0.8, 0.6, 0.4, 0.6, 0.8, 1.05
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4,5,6)

# Generate example data from a model:
Z <- withr::with_seed(2022,
  code = MASS::mvrnorm(number_of_observations,
    mu = mu, Sigma = sigma_matrix
  )
)
# End of prepare model

## ---- example3_2--------------------------------------------------------------
dim(Z)
number_of_observations <- nrow(Z) # 7
p <- ncol(Z) # 6

S <- (t(Z) %*% Z) / number_of_observations

g <- gips(S, number_of_observations, was_mean_estimated = FALSE)
g_map <- find_MAP(g, optimizer = "brute_force")

## ---- example3_3--------------------------------------------------------------
AIC(g)
AIC(g_map) # this is smaller, so this is better

BIC(g)
BIC(g_map) # this is smaller, so this is better

