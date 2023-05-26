library(testthat)
source("../src/shared_functions.R", encoding = "UTF-8")

test_that("random_select returns the correct number of items, L1=8", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 8
  result <- random_select(L1, n1)
  expect_length(result, n1)
})

test_that("random_select returns the correct number of items, L1=5", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 5
  result <- random_select(L1, n1)
  expect_length(result, n1)
})

test_that("random_select returns the correct number of items, L1=3", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 3
  result <- random_select(L1, n1)
  expect_length(result, n1)
})

test_that("random_select selects items from L1, L1=7", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 7
  result <- random_select(L1, n1)
  expect_true(all(result %in% L1))
})

test_that("random_select selects items from L1, L1=4", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 4
  result <- random_select(L1, n1)
  expect_true(all(result %in% L1))
})

test_that("random_select generates unique sequences when n1 > m1", {
  L1 <- c(1, 2, 3, 4, 5)
  n1 <- 10
  result <- random_select(L1, n1)
  sequences <- embed(result, 4)
  expect_true(all(duplicated(sequences) == FALSE))
})
