#' Get Structure Constants
#'
#' Finds constants necessary for internal calculations of integrals and
#' eventually the posteriori probability in [log_posteriori_of_gips()].
#'
#' Uses [Theorem 5 from references](https://arxiv.org/abs/2004.03503)
#' to calculate the constants.
#'
#' @param perm An object of a `gips_perm` class.
#'     It can also be of a `gips` class, but
#'     it will be interpreted as the underlying `gips_perm`.
#'
#' @returns Returns a list of 5 items:
#'     `r`, `d`, `k`, `L`, `dim_omega` - vectors of constants from
#'     [Theorem 1 from references](https://arxiv.org/abs/2004.03503)
#'     and the beginning of
#'     [section 3.1. from references](https://arxiv.org/abs/2004.03503).
#' @export
#'
#' @references Piotr Graczyk, Hideyuki Ishi, Bartosz Kołodziejek, Hélène Massam.
#' "Model selection in the space of Gaussian models invariant by symmetry."
#' The Annals of Statistics, 50(3) 1747-1774 June 2022.
#' [arXiv link](https://arxiv.org/abs/2004.03503);
#' \doi{10.1214/22-AOS2174}
#'
#' @seealso
#' * [calculate_gamma_function()], [log_posteriori_of_gips()] - The functions
#'     that rely heavily on `get_structure_constants()`.
#'
#' @examples
#' perm <- gips_perm("(1)(2)(3)(4,5)", 5)
#' get_structure_constants(perm)
get_structure_constants <- function(perm) {
  if (inherits(perm, "gips")) {
    validate_gips(perm)
    perm <- perm[[1]]
  }
  if (!(inherits(perm, "gips_perm"))) {
    wrong_argument_abort(
      i = "`perm` must be of a `gips_perm` class.",
      x = paste0(
        "You provided `perm` with `class(perm) == (",
        paste(class(perm), collapse = ", "), ")`."
      )
    )
  }

  perm_size <- attr(perm, "size")
  l <- get_cycle_representatives_and_lengths(perm)
  representatives <- l[["representatives"]]
  cycle_lengths <- l[["cycle_lengths"]]
  perm_order <- ifelse(length(cycle_lengths) >= 2,
    numbers::mLCM(cycle_lengths),
    cycle_lengths
  )

  r <- calculate_r(cycle_lengths, perm_order)
  d <- calculate_d(perm_order)

  L <- sum(r > 0)
  d <- d[r > 0]
  r <- r[r > 0]
  k <- d
  dim_omega <- r + r * (r - 1) * d / 2

  list(
    "r" = r,
    "d" = d,
    "k" = k,
    "L" = L,
    "dim_omega" = dim_omega
  )
}

#' Get cycle representatives and lengths
#'
#' Essentially get iC, pC from paper
#'
#' @inheritParams get_structure_constants
#'
#' @returns A List with 2 items: `representatives` and `cycle_lengths`.
#'
#' @examples
#' perm <- gips_perm(permutations::as.cycle(permutations::as.word(c(4, 3, 6, 5, 1, 2))), 6)
#' get_cycle_representatives_and_lengths(perm)
#'
#' @noRd
get_cycle_representatives_and_lengths <- function(perm) {
  list(
    "representatives" = sapply(perm, function(v) v[1]),
    "cycle_lengths" = sapply(perm, length)
  )
}

#' Calculate structure constant r
#'
#' @returns An integer vector. Structure constant r WITH elements equal to 0.
#' @noRd
calculate_r <- function(cycle_lengths, perm_order) {
  M <- floor(perm_order / 2)
  if (M == 0) {
    # identity function
    return(length(cycle_lengths))
  }
  # for a in 0,1,...,floor(perm_order/2)
  # r_a = #{1:C such that a*p_c is a multiple of N}
  # AKA a*p_c %% N == 0

  # Corollary: N %% p_c == 0 for each p_c, cause N is LCM of all p_c
  multiples <- round(perm_order / cycle_lengths) # the result of division should be an integer, but floats may interfere

  # Now we have to adjust for 2 cases:
  # 1) some alphas are too large
  # 2) some alphas are so small, that we can include their multiples
  #   (if a*p_c %% N == 0, then for any natural k  k*a*p_c %% N == 0)
  alphas <- unlist(lapply(multiples, function(cycle_multiple) {
    max_multiple <- floor(M / cycle_multiple)
    cycle_multiple * 0:max_multiple
  }))

  alpha_count <- table(alphas)
  r <- rep(0, M + 1)
  r[as.double(names(alpha_count)) + 1] <- as.double(alpha_count)
  r
}

#' Calculate structure constant d
#'
#' @noRd
calculate_d <- function(perm_order) {
  M <- floor(perm_order / 2)
  d <- c(1, rep(2, M))
  if (perm_order %% 2 == 0) {
    d[M + 1] <- 1
  }
  d
}
