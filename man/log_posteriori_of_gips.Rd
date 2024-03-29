% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_posteriori_of_gips.R
\name{log_posteriori_of_gips}
\alias{log_posteriori_of_gips}
\title{A log of a posteriori that the covariance matrix is invariant under permutation}
\usage{
log_posteriori_of_gips(g)
}
\arguments{
\item{g}{An object of a \code{gips} class.}
}
\value{
Returns a value of
the logarithm of an unnormalized A Posteriori.
}
\description{
More precisely, it is the logarithm of an unnormalized
posterior probability. It is the goal function for
optimization algorithms in the \code{find_MAP()} function.
The \code{perm_proposal} that maximizes this function is
the Maximum A Posteriori (MAP) Estimator.
}
\details{
It is calculated using
\href{https://arxiv.org/abs/2004.03503}{formulas (33) and (27) from references}.

If \code{Inf} or \code{NaN} is reached, it produces a warning.
}
\examples{
# In the space with p = 2, there is only 2 permutations:
perm1 <- permutations::as.cycle("(1)(2)")
perm2 <- permutations::as.cycle("(1,2)")
S1 <- matrix(c(1, 0.5, 0.5, 2), nrow = 2, byrow = TRUE)
g1 <- gips(S1, 100, perm = perm1)
g2 <- gips(S1, 100, perm = perm2)
log_posteriori_of_gips(g1) # -134.1615, this is the MAP Estimator
log_posteriori_of_gips(g2) # -138.1695

exp(log_posteriori_of_gips(g1) - log_posteriori_of_gips(g2)) # 55.0
# g1 is 55 times more likely than g2.
# This is the expected outcome because S[1,1] significantly differs from S[2,2].

compare_posteriories_of_perms(g1, g2)
# The same result, but presented in a more pleasant way

# ========================================================================

S2 <- matrix(c(1, 0.5, 0.5, 1.1), nrow = 2, byrow = TRUE)
g1 <- gips(S2, 100, perm = perm1)
g2 <- gips(S2, 100, perm = perm2)
log_posteriori_of_gips(g1) # -98.40984
log_posteriori_of_gips(g2) # -95.92039, this is the MAP Estimator

exp(log_posteriori_of_gips(g2) - log_posteriori_of_gips(g1)) # 12.05
# g2 is 12 times more likely than g1.
# This is the expected outcome because S[1,1] is very close to S[2,2].

compare_posteriories_of_perms(g2, g1)
# The same result, but presented in a more pleasant way
}
\references{
Piotr Graczyk, Hideyuki Ishi, Bartosz Kołodziejek, Hélène Massam.
"Model selection in the space of Gaussian models invariant by symmetry."
The Annals of Statistics, 50(3) 1747-1774 June 2022.
\href{https://arxiv.org/abs/2004.03503}{arXiv link};
\doi{10.1214/22-AOS2174}
}
\seealso{
\itemize{
\item \code{\link[=calculate_gamma_function]{calculate_gamma_function()}} - The function that calculates
the value needed for \code{log_posteriori_of_gips()}.
\item \code{\link[=get_structure_constants]{get_structure_constants()}} - The function that calculates
the structure constants needed for \code{log_posteriori_of_gips()}.
\item \code{\link[=find_MAP]{find_MAP()}} - The function that optimizes
the \code{log_posteriori_of_gips} function.
\item \code{\link[=compare_posteriories_of_perms]{compare_posteriories_of_perms()}} - Uses \code{log_posteriori_of_gips()}
to compare a posteriori of two permutations.
\item \code{vignette("Theory", package = "gips")} or its
\href{https://przechoj.github.io/gips/articles/Theory.html}{pkgdown page} -
A place to learn more about the math behind the \code{gips} package.
}
}
