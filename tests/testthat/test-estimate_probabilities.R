test_that("estimate_probabilities works", {
  points <- list(
    to_perm(c(2, 3, 1)),
    to_perm(c(3, 1, 2)),
    to_perm(c(1, 3, 2)),
    to_perm(c(1, 3, 2)),
    to_perm(c(1, 2, 3))
  )
  points <- lapply(points, function(p) gips_perm(p, 3))
  expected_counts <- c(2 / 2, 2 / 1, 1) / 5
  expected_probabilities <- expected_counts / sum(expected_counts)
  names(expected_probabilities) <- c("(1,2,3)", "(2,3)", "()")

  actual_probabilities <- estimate_probabilities(points)
  expect_setequal(
    names(actual_probabilities),
    names(expected_probabilities)
  )
  expect_equal(
    actual_probabilities[names(expected_probabilities)],
    expected_probabilities
  )
})

test_that("get_representative examples", {
  p1 <- gips_perm(permutations::as.word(c(2, 1, 3, 6, 4, 5)), 6)
  expected_p <- gips_perm("(1,2)(4,5,6)", 6)

  expect_equal(
    get_group_representative(p1),
    expected_p
  )

  gips_nullperm <- gips_perm(permutations::nullword, 0)

  expect_equal(
    get_group_representative(gips_nullperm),
    gips_nullperm
  )
})
