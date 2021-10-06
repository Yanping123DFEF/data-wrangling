context("Unit Test Examples")

test_that("find_real_roots returns a numeric vector of length 2", {
  expect_is(find_real_roots(5, 2), "numeric")
  expect_length(find_real_roots(5, 2), 2)
})

test_that("find_real_roots gives errors for non-numbers or vectors", {
  expect_error(find_real_roots("A", "B"), "Input must be two numbers")
  expect_error(find_real_roots(1:4, 0), "p and q must each have length 1")
  
})


test_that("find_real_roots gives correct values for some examples", {
  expect_equal(find_real_roots(2, 1), c(-1, -1))
  expect_equal(find_real_roots(0, -1), c(-1, 1))
  expect_equal(find_real_roots(5, 2), c(-4.5615528, -0.4384472))
})


test_that("intentional error", {
  expect_equal(find_real_roots(2, 1), c(-1, 5))
})

