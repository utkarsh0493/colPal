

test_that("Color pallete of distinct colors", {
  numColors <- 10
  colPallete <- get_pallete(n = 10)

  testthat::expect_equal(length(colPallete), numColors)

})

test_that("Only 28 distinct colors are possible", {
  n <- 20
  if(n>28) {
    stop("Only 28 distinct colors are possible")
  }
})

test_that("Number of colors do not match with length of list of colors", {
  n <- 5
  list_ <- get_pallete(5)
  if( n != length(list_)) {
    stop("Number of colors do not match with length of list of colors")
  }
})
