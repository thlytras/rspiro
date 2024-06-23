#' Calculate L,M,S of JRS (Japanese Respiratory Society 2014) equations
#'
#' This function calculates the L,M,S using the JRS (2014) equations
#' (Kubota et al, Respir Investig 2014). The output
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
getLMS_JRS <- function(age, height, gender=1, param="FEV1") {
  param <- toupper(unique(param[!is.na(param)]))
  if (length(param)==0)
    stop("You must specify at least one parameter to calculate.")
  if (sum(!(param %in% c("FEV1", "FVC", "FEV1FVC", "VC")))>0)
    stop(sprintf("Argument 'param' must be one or more of \"%s\".",
        paste(c("FEV1", "FVC", "FEV1FVC", "VC"), collapse='", "')))
  dat <- rspiro_check_somat(age, height, gender, 1, JRS=TRUE)
  dat$agebound <- floor(dat$agebound)
  dat$id <- 1:nrow(dat)
  dat$ethnicity <- NULL

  dat <- do.call(rbind, lapply(param, function(x) {dat$f <- x; dat}))

  dat <- merge(dat, JRS_lookup, sort=FALSE, all.x=TRUE)
  dat <- dat[order(factor(dat$f, levels=levels(factor(JRS_lookup$f))), dat$id),]

  dat$L <- dat$M <- dat$S <- NA
  
  # From here on we calculate the L,M,S according to different equations...
  
  s <- dat$gender==1 & dat$f=="FEV1"
  dat$M[s] <- with(dat, exp(-7.5722 + 1.9393*log(height[s]*100) - 0.3068*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-3.0440 + 0.2325*log(age[s]) + Sspline[s]))
  dat$L[s] <- 1
  
  s <- dat$gender==1 & dat$f=="FVC"
  dat$M[s] <- with(dat, exp(-8.8877 + 2.1494*log(height[s]*100) - 0.1891*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.8335 + 0.1726*log(age[s]) + Sspline[s]))
  dat$L[s] <- 1
  
  s <- dat$gender==1 & dat$f=="VC"
  dat$M[s] <- with(dat, exp(-8.8317 + 2.1043*log(height[s]*100) - 0.1382*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.3730 + 0.0450*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.3464
  
  s <- dat$gender==1 & dat$f=="FEV1FVC"
  dat$M[s] <- with(dat, exp(1.2578 - 0.1948*log(height[s]*100) - 0.1220*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-3.266 + 0.150*log(age[s]) + Sspline[s]))
  dat$L[s] <- with(dat, 8.905 - 1.799*log(age[s]) + Lspline[s])
  
  s <- dat$gender==2 & dat$f=="FEV1"
  dat$M[s] <- with(dat, exp(-6.9428 + 1.8053*log(height[s]*100) - 0.3401*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-3.1024 + 0.2537*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.7783
  
  s <- dat$gender==2 & dat$f=="FVC"
  dat$M[s] <- with(dat, exp(-8.3268 + 2.0137*log(height[s]*100) - 0.2029*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.8527 + 0.1881*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.6127
  
  s <- dat$gender==2 & dat$f=="VC"
  dat$M[s] <- with(dat, exp(-8.0707 + 1.9399*log(height[s]*100) - 0.1678*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.7071 + 0.1447*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.1268
  
  s <- dat$gender==2 & dat$f=="FEV1FVC"
  dat$M[s] <- with(dat, exp(1.2854 - 0.1844*log(height[s]*100) - 0.1425*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-3.1624 + 0.1068*log(age[s]) + Sspline[s]))
  dat$L[s] <- with(dat, 12.989 - 2.987*log(age[s]) + Lspline[s])
  
  
  dat[,c("id", "age", "height", "gender", "f", "L", "M", "S")]
}
