#' Calculate L,M,S of race-neutral GLI global (2022) equations
#'
#' This function calculates the L,M,S using the race-neutral GLI global (2022) equations
#' (Bowerman et al, AJRCCM 2023). The output
#' of this function is used in calculating the LLN and predicted spirometry parameters.
#' Checking of the input is also performed here.
#'
#' This is for internal use only.
#'
#' @keywords internal
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param param A character vector, containing one of more of the following parameters (case insensitive):
#' "FEV1", "FVC", "FEV1FVC"
#'
#' @details Arguments \code{age}, \code{height} and \code{gender} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return A data.frame with L, M and S, for every patient and every parameter requested
getLMS_GLIgl <- function(age, height, gender=1, param="FEV1") {
  param <- toupper(unique(param[!is.na(param)]))
  if (length(param)==0)
    stop("You must specify at least one parameter to calculate.")
  if (sum(!(param %in% c("FEV1", "FVC", "FEV1FVC")))>0)
    stop(sprintf("Argument 'param' must be one or more of \"%s\".",
        paste(levels(lookup$f), collapse='", "')))
  dat <- rspiro_check_somat(age, height, gender, 1)
  dat$id <- 1:nrow(dat)
  dat$ethnicity <- NULL

  dat <- do.call(rbind, lapply(param, function(x) {dat$f <- x; dat}))

  dat <- merge(dat, GLIgl_lookup, sort=FALSE, all.x=TRUE)
  dat <- dat[order(factor(dat$f, levels=levels(factor(GLIgl_lookup$f))), dat$id),]

  dat$L <- dat$M <- dat$S <- NA
  
  # From here on we calculate the L,M,S according to different equations...
  
  s <- dat$gender==1 & dat$f=="FEV1"
  dat$M[s] <- with(dat, exp(-11.399108 + 2.462664*log(height[s]*100) - 0.011394*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.256278 + 0.080729*log(age[s]) + Sspline[s]))
  dat$L[s] <- 1.22703
  
  s <- dat$gender==1 & dat$f=="FVC"
  dat$M[s] <- with(dat, exp(-12.629131 + 2.727421*log(height[s]*100) + 0.009174*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.195595 + 0.068466*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.9346
  
  s <- dat$gender==1 & dat$f=="FEV1FVC"
  dat$M[s] <- with(dat, exp(1.022608 - 0.218592*log(height[s]*100) - 0.027586*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.882025 + 0.068889*log(age[s]) + Sspline[s]))
  dat$L[s] <- with(dat, 3.8243 - 0.3328*log(age[s]))
  
  s <- dat$gender==2 & dat$f=="FEV1"
  dat$M[s] <- with(dat, exp(-10.901689 + 2.385928*log(height[s]*100) - 0.076386*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.364047 + 0.129402*log(age[s]) + Sspline[s]))
  dat$L[s] <- 1.21388
  
  s <- dat$gender==2 & dat$f=="FVC"
  dat$M[s] <- with(dat, exp(-12.055901 + 2.621579*log(height[s]*100) - 0.035975*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.310148 + 0.120428*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.899
  
  s <- dat$gender==2 & dat$f=="FEV1FVC"
  dat$M[s] <- with(dat, exp(0.9189568 - 0.1840671*log(height[s]*100) - 0.0461306*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-3.171582 + 0.144358*log(age[s]) + Sspline[s]))
  dat$L[s] <- with(dat, 6.6490 - 0.9920*log(age[s]))
  
  
  dat[,c("id", "age", "height", "gender", "f", "L", "M", "S")]
}
