#' Permutation object
#'
#' Create permutation objects to be passed to
#' other functions of the `gips` package.
#'
#' @param x A single object that can be interpreted by
#'     the [permutations::permutation()] function.
#'     For example, the character of a form `"(1,2)(4,5)"`. See examples.
#'     It can also be of a `gips` class but
#'     it will be interpreted as the underlying `gips_perm`.
#' @param size An integer. Size of a permutation
#'     (AKA cardinality of a set, on which permutation is defined. See examples).
#'
#' @section Methods for a `gips` class:
#' * [as.character.gips_perm()]
#' * [print.gips_perm()]
#'
#' @returns `gips_perm()` returns an object of
#'     a `gips_perm` class after the safety checks.
#'
#' @seealso
#' * [project_matrix()] - `gips_perm` is the `perm` parameter of `project_matrix()`.
#' * [permutations::permutation()] - The constructor for the `x` parameter.
#' * [gips()] - The constructor for the `gips` class uses
#'     the `gips_perm` object as the base object.
#'
#' @examples
#' # All 7 following lines give the same output:
#' gperm <- gips_perm("(12)(45)", 5)
#' gperm <- gips_perm("(1,2)(4,5)", 5)
#' gperm <- gips_perm(as.matrix(c(2, 1, 3, 5, 4)), 5)
#' gperm <- gips_perm(t(as.matrix(c(2, 1, 3, 5, 4))), 5) # both way for a matrix works
#' gperm <- gips_perm(list(list(c(2, 1), c(4, 5))), 5)
#' gperm <- gips_perm(permutations::as.word(c(2, 1, 3, 5, 4)), 5)
#' gperm <- gips_perm(permutations::as.cycle("(1,2)(4,5)"), 5)
#' gperm
#'
#' # note the necessity of the `size` parameter:
#' gperm <- gips_perm("(12)(45)", 5)
#' gperm <- gips_perm("(12)(45)", 7) # this one is a different permutation
#'
#' try(gperm <- gips_perm("(12)(45)", 4))
#' # Error, `size` was set to 4, while the permutation has the element 5.
#'
#' @export
gips_perm <- function(x, size) {
  if (inherits(x, "gips")) {
    validate_gips(x)
    if (attr(x[[1]], "size") != size) {
      rlang::abort(c("x" = paste0(
        "You provided a `gips` object as the `x` parameter of `gips_perm()`, which in general is OK, but You also provided size = ",
        size, ", which is different from attr(x[[1]], 'size') = ", attr(x[[1]], "size")
      )))
    }
    return(x[[1]])
  }
  if (!inherits(x, "permutation")) {
    if (is.matrix(x) && dim(x)[1] != 1) {
      x <- t(x) # matrix x has to be a row, not a column
    }
    if (is.matrix(x) || is.character(x) || is.list(x)) {
      x <- permutations::permutation(x)
    } else {
      rlang::abort(c("There was a problem identified with provided argument:",
        "i" = "`x` argument must be either of a class 'permutation', or sth that can be passed to `permutations::permutation()` function (a matrix, or a character, or a list).",
        "x" = paste0("You provided the `x` argument that is of type ", typeof(x), ".")
      ))
    }
  }
  if (rlang::is_missing(size)) {
    rlang::abort(c("There was a problem identified with provided argument:",
      "i" = "`size` argument must be provided.",
      "x" = "You did not provide the `size` argument."
    ))
  }
  if (length(size) > 1) {
    rlang::warn(c("Passing multiple sizes to `gips_perm()` is not supported. Taking only the first one.",
      "i" = paste0("You provided ", length(size), " sizes.")
    ))
    size <- size[1]
  }
  if (!is.wholenumber(size)) {
    rlang::abort(c("There was a problem identified with provided argument:",
      "i" = "`size` must be a whole number.",
      "x" = paste0("You provided `size == ", size, "`.")
    ))
  }
  x <- permutations::as.cycle(x)

  if (length(unclass(x)) > 1) { # this could be checked prior to `permutations::permutation()`, but I think this will almost never be a problem
    rlang::warn(c("Passing multiple permutations to `gips_perm()` is not supported. Taking only the first one.",
      "i" = paste0("You provided ", length(unclass(x)), " permutations.")
    ))
    x <- x[1]
  }

  if (is.null(permutations::is.id(x))) {
    return(new_gips_perm(list(), 0)) # rearrange_cycles is not needed, because `identical(rearrange_cycles(list()), list())`
  }

  if (permutations::is.id(x)) {
    x <- as.list(1:size)
    return(new_gips_perm(x, size)) # rearrange_cycles is not needed, because `identical(rearrange_cycles(x), x)`
  }

  cycles <- unclass(x)[[1]]
  all_ints <- unlist(cycles)
  if (size < max(all_ints)) {
    wrong_argument_abort(
      i = "`size` attribute must be greater or equal to the largest integer in elements of `x`.",
      x = paste0(
        "`size` equals ", size,
        " while the maximum element is ",
        max(all_ints)
      )
    )
  }

  representatives <- permutations::get1(x)

  # unfortunately, cycles of length 1 are ignored
  fixed_boolean <- permutations::fixed(x)
  # if "last" elements are fixed, they are not returned.
  # correct for that
  if (length(fixed_boolean) < size) {
    fixed_boolean[(length(fixed_boolean) + 1):size] <- TRUE
  }
  fixed_elements <- which(fixed_boolean)

  subcycles <- c(cycles, as.list(fixed_elements))

  validate_gips_perm(new_gips_perm(rearrange_cycles(subcycles), size))
}

#' Rearrange cycles
#'
#' `gips_perm` object stores permutations in cyclic form in following convention:
#' 1) cycles are ordered by their minimal element
#' 2) First element of a cycle is its minimal
#'
#' @param cycles A list of integer vectors.
#'
#' @examples
#' cycles <- list(c(2, 4, 3), c(5, 1))
#' rearranged <- rearrange_cycles(cycles)
#' # rearranged is list (c(1,5), c(2,4,3))
#' @noRd
rearrange_cycles <- function(cycles) {
  rearranged_cycles <- lapply(cycles, rearrange_vector)
  representatives <- sapply(rearranged_cycles, function(v) v[1])
  rearranged_cycles[order(representatives)]
}

#' @describeIn gips_perm Constructor. Only intended for low-level use.
#'
#' @param rearranged_cycles A list of rearranged integer vectors.
#'     Each vector corresponds to a single cycle of a permutation.
#'
#' @returns `new_gips_perm()` returns an object of
#'     a `gips_perm` class without the safety checks.
#'
#' @export
new_gips_perm <- function(rearranged_cycles, size) {
  if (!is.list(rearranged_cycles) ||
    !is.wholenumber(size)) {
    rlang::abort("`gips_perm` object cannot be created from those arguments.")
  }

  structure(rearranged_cycles, size = size, class = "gips_perm")
}

#' @describeIn gips_perm Validator. Only intended for low-level use.
#'
#' @param g Object to be checked whether it is
#'     a proper object of a `gips_perm` class.
#'
#' @returns `validate_gips_perm()` returns its argument unchanged.
#'     If the argument is not a proper element of a `gips_perm` class,
#'     it produces an error.
#'
#' @export
validate_gips_perm <- function(g) {
  if (!(inherits(g, "gips_perm"))) {
    wrong_argument_abort(
      i = "`g` must be of a `gips_perm` class.",
      x = paste0(
        "You provided `g` with `class(g) == (",
        paste(class(g), collapse = ", "), ")`."
      )
    )
  }
  if (!is.list(g)) {
    wrong_argument_abort(
      i = "The `g` must be a list.",
      x = paste0(
        "You provided `g` with `typeof(g) == '",
        typeof(g), "'."
      )
    )
  }
  size_attr <- attr(g, "size")
  if (is.null(size_attr) || !is.wholenumber(size_attr) || length(size_attr) != 1) {
    wrong_argument_abort(i = "`g` must have an attribute `size` set as a single integer.")
  }
  is_whole_number <- unlist(sapply(g, is.wholenumber))
  if (!all(is_whole_number)) {
    wrong_element_index <- which(!is_whole_number)[1]
    wrong_argument_abort(i = "All elements of `g` must be integer vectors.")
  }
  all_ints <- unlist(g)
  if (length(all_ints) != length(unique(all_ints))) {
    wrong_argument_abort(i = "Elements of cycles must not repeat across or within cycles.")
  }
  first_element_is_min <- sapply(g, which.min) == 1
  if (!all(first_element_is_min)) {
    wrong_element_index <- which(!first_element_is_min)[1]
    wrong_argument_abort(
      i = "First element of each cycle must be the minimum element of this cycle.",
      x = paste0(
        "This property is violated by element ",
        wrong_element_index,
        " where the minimum element is on place ",
        which.min(g[[wrong_element_index]]), "."
      )
    )
  }
  not_sorted_by_first <- is.unsorted(sapply(g, function(v) v[1]))
  if (not_sorted_by_first) {
    wrong_argument_abort(i = "Cycles must appear in order determined by their first elements.")
  }
  if (attr(g, "size") < max(all_ints)) {
    wrong_argument_abort(
      i = "`size` attribute must be greater or equal to largest integer in elements of `g`.",
      x = paste0(
        "You provided `size == ", attr(g, "size"),
        "` while the maximum element is ",
        max(all_ints), "."
      )
    )
  }
  g
}

#' Printing `gips_perm` object
#'
#' Printing function for a `gips_perm` class.
#'
#' @param x An object of a `gips_perm` class.
#' @param ... Further arguments (currently ignored).
#'
#' @returns Returns an invisible `NULL`.
#'
#' @export
#'
#' @examples
#' gperm <- gips_perm("(5,4)", 5)
#' print(gperm)
print.gips_perm <- function(x, ...) {
  validate_gips_perm(x)
  x <- permutations::as.cycle(x)
  permutations::print.cycle(x, ...)

  invisible(NULL)
}

#' Transform the `gips_perm` object to a character vector
#'
#' Implementation of the S3 method.
#'
#' @inheritParams print.gips_perm
#' @param ... Further arguments (currently ignored).
#'
#' @method as.character gips_perm
#'
#' @returns Returns an object of a `character` type.
#'
#' @seealso
#' * [as.character.gips()] - The underlying `gips_perm` of
#'     the `gips` object is passed to [as.character.gips_perm()].
#' * [permutations::as.character.cycle()] - The underlying permutation of
#'     the `gips` object is passed to [permutations::as.character.cycle()].
#'
#' @export
#'
#' @examples
#' g_perm <- gips_perm("(5,4)", 5)
#' as.character(g_perm)
as.character.gips_perm <- function(x, ...) {
  validate_gips_perm(x)

  as.character(permutations::as.cycle(x), ...)
}

#' Compose permutation with transposition
#'
#' @param gips_perm Object of a `gips_perm` class.
#' @param transposition An integer vector of length 2. Transposition in a form of a
#' cycle.
#'
#' @returns An object of a `gips_perm` class. Composition of `gips_perm` parameter and `transposition`.
#'
#' @noRd
#' @examples
#' perm <- permutations::as.cycle("(1,2,3)(4,5)")
#' gperm <- gips_perm(perm, 6)
#' tr <- c(2, 3)
#' tr_perm <- permutations::as.cycle(tr)
#'
#' composed <- compose_with_transposition(gperm, tr)
#' composed2 <- perm * tr_perm
#'
#' # composed and composed 2 refer to the same permutation
compose_with_transposition <- function(gips_perm, transposition) {
  cycle_1_index <- which(sapply(gips_perm, function(cycle) {
    transposition[1] %in% cycle
  }))
  cycle_2_index <- which(sapply(gips_perm, function(cycle) {
    transposition[2] %in% cycle
  }))
  cycle_1 <- gips_perm[[cycle_1_index]]
  cycle_2 <- gips_perm[[cycle_2_index]]
  composed_gips_perm <- gips_perm[c(-cycle_1_index, -cycle_2_index)]
  if (cycle_1_index == cycle_2_index) {
    # We are breaking cycle into 2 cycles
    shifted_cycle <- shift_vector(cycle_1, which(cycle_1 == transposition[1]) - 1)
    new_cycle_1 <- shifted_cycle[1:(which(shifted_cycle == transposition[2]) - 1)]
    new_cycle_2 <- shifted_cycle[(which(shifted_cycle == transposition[2])):length(shifted_cycle)]
    composed_gips_perm <- add_cycle(composed_gips_perm, new_cycle_1)
    composed_gips_perm <- add_cycle(composed_gips_perm, new_cycle_2)
  } else {
    # We are merging 2 cycles
    ind <- which(cycle_1 == transposition[1])
    fragment_1 <- shift_vector(cycle_2, which(cycle_2 == transposition[2]) - 1)
    fragment_2 <- shift_vector(cycle_1, which(cycle_1 == transposition[1]) - 1)
    new_cycle <- c(fragment_1, fragment_2)
    composed_gips_perm <- add_cycle(composed_gips_perm, new_cycle)
  }
  new_gips_perm(
    rearrange_cycles(composed_gips_perm),
    attr(gips_perm, "size")
  )
}

#' Add a new cycle to permutation
#'
#' @param cycles A list of integer vectors. Each corresponds to cycles of a permutation.
#' @param new_cycle An integer vector. None of its elements are present in `cycles`.
#'
#' @noRd
add_cycle <- function(cycles, new_cycle) {
  # Assume, that cycles are sorted by their min element
  # new_cycle - not necessarily
  new_cycle <- rearrange_vector(new_cycle)
  min_representatives <- sapply(cycles, function(v) v[1])
  insert_index <- findInterval(new_cycle[1], min_representatives)
  append(cycles, list(new_cycle), after = insert_index)
}

#' The same as [gips_perm()], but lacks safety checks
#'
#' We highly advise against using this function.
#' Instead, use [gips_perm()]. Although, this one is slightly faster.
#' @noRd
gips_perm_no_checks <- function(x, size) {
  cycles <- unclass(x)[[1]]
  all_ints <- unlist(cycles)
  representatives <- permutations::get1(x)
  fixed_boolean <- permutations::fixed(x)
  if (length(fixed_boolean) < size) {
    fixed_boolean[(length(fixed_boolean) + 1):size] <- TRUE
  }
  fixed_elements <- which(fixed_boolean)
  subcycles <- c(cycles, as.list(fixed_elements))

  new_gips_perm(rearrange_cycles(subcycles), size)
}
