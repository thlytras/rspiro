#' Check input data for LLN_GLI()
#'
#' This functions checks the supplied input for LLN_GLI() and returns a
#' properly formatted data.frame
#'
#' @keywords internal
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male)
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 3 = NE Asian, 4 = SE Asian, 5 = Other/mixed)
#'
#' @return Returns a data frame with these four columns plus a column 'agebound', which is
#' age rounded to the lowest 0.25 of the year.
rspiro_check_data <- function(age, height, gender, ethnicity) {
  mlen <- max(length(age), length(height), length(gender), length(ethnicity))
  if (length(age)==1) age <- rep(age, mlen)
  if (length(height)==1) height <- rep(height, mlen)
  if (length(gender)==1) gender <- rep(as.integer(gender), mlen)
  if (length(ethnicity)==1) ethnicity <- rep(ethnicity, mlen)
  if (length(unique(length(age), length(height), length(gender), length(ethnicity)))>1)
    stop("All supplied vectors (age, height, gender, ethnicity) must have the same length.")

  res <- as.data.frame(cbind(age=age, height=height, gender=gender,
    ethnicity=ethnicity, agebound=floor(age*4)/4))

  res
}
