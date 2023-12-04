
test_that("verifying all arguments are formulas", {
  expect_error(
    purrr_when(
      2*4,
      2<4 ~ "Hello"
    )
  )
})

test_that("verifying LHS of all arguments evaluate to logical", {
  expect_error(
    purrr_when(
      2*4 ~ "Hey",
      2<4 ~ "Hello"
    )
  )
})

test_that("verifying message prints if there are arguments after a condition has
           been matched", {
  expect_message(
    purrr_when(
      2<4 ~ "Hey",
      2*4 ~ "Hello"
    )
  )
})

test_that("verify that NULL gets returned if no conditions are matched", {
  y <- 20
  expect_equal(
    purrr_when(
      y < 20 ~ "y < 20",
      y > 20 ~ "y >20"
    ),
    NULL
  )
})

test_that("check that warning gets thrown when LHS evaluates to numeric NA",{
  y <- "Hi"
  expect_warning(
    purrr_when(
      mean(y) < 20 ~ "y < 20",
      mean(y) > 20 ~ "y > 20"
    )
  )
})

test_that(
  "check that error gets thrown when LHS evaluates to chr NA",
  {
    y <- "Hi"
    expect_error(suppressWarnings(purrr_when(
      as.character(mean(y) < 20) ~ "y < 20",
      as.character(mean(y) > 20) ~ "y > 20"
    )))
  }
)

