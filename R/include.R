#' Check input data
#'
#' This functions checks the supplied input and returns a
#' properly formatted data.frame
#'
#' @keywords internal
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male)
#' @param ethnicity Ethnicity (GLI: 1 = Caucasian, 2 = African-American, 
#'   3 = NE Asian, 4 = SE Asian, 5 = Other/mixed, NHANES: 1 = Caucasian, 
#'   2 = African-American, 3 = Mexican-American)
#' @param NHANES Logical. Is input data for the NHANES III equations? 
#'   Defaults to \code{FALSE}.
#'
#' @return Returns a data frame with these four columns plus a column 'agebound', which is
#' age rounded to the lowest 0.25 of the year.
rspiro_check_data <- function(age, height, gender, ethnicity, NHANES=FALSE) {
  mlen <- max(length(age), length(height), length(gender), length(ethnicity))
  if (length(age)==1) age <- rep(age, mlen)
  if (length(height)==1) height <- rep(height, mlen)
  if (length(gender)==1) gender <- rep(as.integer(gender), mlen)
  if (length(ethnicity)==1) ethnicity <- rep(ethnicity, mlen)
  if (length(unique(c(length(age), length(height), length(gender), length(ethnicity))))>1)
    stop("All supplied vectors (age, height, gender, ethnicity) must have the same length.", call.=FALSE)
  if (min(age, na.rm=TRUE)<3 || max(age, na.rm=TRUE)>95) {
    age[which(age<3 | age>95)] <- NA
    warning("Age cannot be lower than 3 years or higher than 95 years. Returning NA.", call.=FALSE)
  }
  if (min(as.integer(gender), na.rm=TRUE)<1 || max(as.integer(gender), na.rm=TRUE)>2) {
    gender[which(!(as.integer(gender) %in% 1:2))] <- NA
    warning("You have specified more than two unique values for gender. Please check your data. Returning NA.", call.=FALSE)
  }
  if (min(height, na.rm=TRUE)<1 | max(height, na.rm=TRUE)>2.5)
    warning("You have specified heights of <1m or >2.5m. Are you sure this is correct?")
  if (NHANES) {
    if (min(as.integer(ethnicity), na.rm=TRUE)<1 || max(as.integer(ethnicity), na.rm=TRUE)>3) {
      ethnicity[which(!(as.integer(ethnicity) %in% 1:5))] <- NA
      warning("Ethnicity should be a value between 1 and 3, or a factor with three levels. Please check your data. Returning NA.", call.=FALSE)
    }
  } else {
    if (min(as.integer(ethnicity), na.rm=TRUE)<1 || max(as.integer(ethnicity), na.rm=TRUE)>5) {
      ethnicity[which(!(as.integer(ethnicity) %in% 1:5))] <- NA
      warning("Ethnicity should be a value between 1 and 5, or a factor with five levels. Please check your data. Returning NA.", call.=FALSE)
    }  
  }


  res <- as.data.frame(cbind(age=age, height=height, gender=gender,
    ethnicity=as.integer(ethnicity), agebound=floor(age*4)/4))

  res
}
