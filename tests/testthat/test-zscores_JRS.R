test_that("inputs are checked", {
  # inconsistent length somatometric inputs
  expect_error(
    zscore_JRS(
      age=rep(25,100), # too long
      height=rep(1.5,5),
      gender=rep(1,5),
      FEV1=rep(1,5)),
    "All supplied vectors (age, height, gender, ethnicity) must have the same length.",
    fixed = TRUE
  )
  # somatometric length != spirometry length
  expect_error(
    zscore_JRS(
      age=rep(25,5),
      height=rep(1.5,5),
      gender=rep(1,5),
      FEV1=rep(1,100)), # too long
    "If somatometric (age, height, gender, ethnicity) are not length 1, they must all be specified and be the same length as spirometry z-score parameter vectors.",
    fixed = TRUE
  )
  # no spirometry given
  expect_error(
    zscore_JRS(
      age=seq(10,20,30),
      height=c(1,1.5,2),
      gender=c(1,1,1)),
    "At least one spirometry z-score parameter must be specified.",
    fixed = TRUE
  )
  # non-numeric value for spiro
  expect_error(
    zscore_JRS(age = 10, height = 2, gender=1, FEV1='a character vector'),
    "Spirometry z-scores must be numeric.",
    fixed = TRUE
  )
  # illegal value for gender
#   expect_error(
#     zscore_JRS(
#       age = 10,
#       height = 2,
#       gender='f', # character vector rather than factor/1/2
#       FEV1=1),
#     "Invalid value supplied for gender",
#     fixed = TRUE
#   )
#   # suspicious value for gender
#   expect_message(
#     zscore_JRS(
#       age = 10,
#       height = 2,
#       gender=factor('f', levels=c('f','m')), # 'f' supplied as first level of factor
#       FEV1=1),
#     "First level of factor gender ('f') is assumed to be male.",
#     fixed = TRUE
#   )
})

test_that("correct value type returned", {
  expect_type(
    zscore_JRS(
      age = 19,
      height = 2,
      gender=1,
      FEV1=1),
    'double'
  )
  expect_s3_class(
    zscore_JRS(
      age = 19,
      height = 2,
      gender=1,
      FEV1=2,
      FVC=3),
    'data.frame'
  )
})
