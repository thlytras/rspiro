#' Check somatometric input data
#'
#' This function checks the supplied somatometric input and returns a
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
rspiro_check_somat <- function(age, height, gender, ethnicity, NHANES=FALSE) {
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
      ethnicity[which(!(as.integer(ethnicity) %in% 1:3))] <- NA
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



#' Check input data for errors
#'
#' This function checks the supplied input for errors
#'
#' @keywords internal
#'
#' @param spiro_val List of spirometry measurements
#' @param somat_val List of somatometric measurements
#'
#' @return Invisible. Stops if it finds an error.
rspiro_check_input <- function(spiro_val, somat_val) {
  spiro_val_len <- unique(sapply(spiro_val, length))
  somat_val_len <- unique(sapply(somat_val, length))
  stopifnot(is.numeric(somat_val$age))
  stopifnot(is.numeric(somat_val$height))
  if (length(spiro_val)==0)
    stop("At least one spirometry z-score parameter must be specified.")
  if (length(spiro_val_len)>1)
    stop("Not all spirometry z-score parameter vectors have the same length.")
  if (length(somat_val_len)>1)
    stop("Not all somatometric (age, height, gender, ethnicity) vectors have the same length.")
  if ((somat_val_len!=1) && (somat_val_len!=spiro_val_len))
    stop("If somatometric (age, height, gender, ethnicity) are not length 1, they must all be specified and be the same length as spirometry z-score parameter vectors.")
  if(!all(is.numeric(unlist(spiro_val))))
    stop("Spirometry z-scores must be numeric.")
  if (!(all(as.character(somat_val$gender) %in% c('1','2')) || ((is.factor(somat_val$gender) && (length(levels(somat_val$gender))==2)))))
    stop("Invalid value supplied for gender")
  if (is.factor(somat_val$gender) && grepl('[fFwW]', levels(somat_val$gender)[1]))
    message(sprintf("First level of factor gender ('%s') is assumed to be male.", levels(somat_val$gender)[1]))
  invisible()
}
