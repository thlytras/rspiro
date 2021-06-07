test_that("inputs are checked", {
  # inconsistent length somatometric inputs
  expect_error(
    zscore_GLI(
      age=rep(25,100), # too long
      height=rep(1.5,5),
      gender=rep(1,5),
      ethnicity=rep(3,5),
      FEV1=rep(1,5)),
    "Not all somatometric (age, height, gender, ethnicity) vectors have the same length.",
    fixed = TRUE
  )
  # somatometric length != spirometry length
  expect_error(
    zscore_GLI(
      age=rep(25,5),
      height=rep(1.5,5),
      gender=rep(1,5),
      ethnicity=rep(3,5),
      FEV1=rep(1,100)), # too long
    "If somatometric (age, height, gender, ethnicity) are not length 1, they must all be specified and be the same length as spirometry parameter vectors.",
    fixed = TRUE
  )
  # no spirometry given
  expect_error(
    zscore_GLI(
      age=seq(10,20,30),
      height=c(1,1.5,2),
      gender=c(1,1,1,1,1)),
    "At least one spirometry parameter must be specified.",
    fixed = TRUE
  )
  # non-numeric value for spiro
  expect_error(
    zscore_GLI(age = 10, height = 2, gender=1, ethnicity=3, FEV1='a character vector'),
    "Spirometry values must be numeric.",
    fixed = TRUE
  )
  # illegal value for gender
  expect_error(
    zscore_GLI(
      age = 10,
      height = 2,
      gender='f', # character vector rather than factor/1/2
      ethnicity=3,
      FEV1=1),
    "invalid value supplied for gender",
    fixed = TRUE
  )
  # suspicious value for gender
  expect_message(
    zscore_GLI(
      age = 10,
      height = 2,
      gender=factor('f', levels=c('f','m')), # 'f' supplied as first level of factor
      ethnicity=3,
      FEV1=1),
    "First level of factor gender ('f') is assumed to be male.",
    fixed = TRUE
  )
  expect_error( # illegal value for ethnicity
    zscore_GLI(
      age = 10,
      height = 2,
      gender=1,
      ethnicity=27, # invalid
      FEV1=1),
    "invalid value supplied for ethnicity",
    fixed = TRUE
  )
})

test_that("correct value type returned", {
  expect_type(
    zscore_GLI(
      age = 10,
      height = 2,
      gender=1,
      ethnicity=4,
      FEV1=1),
    'double'
  )
  expect_s3_class(
    zscore_GLI(
      age = 10,
      height = 2,
      gender=1,
      ethnicity=4,
      FEV1=2,
      FVC=3),
    'data.frame'
  )
})
