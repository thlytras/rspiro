#' Calculate L,M,S of GLI diffusing capacity (2017) equations
#'
#' This function calculates the L,M,S using the GLI diffusing capacity (2017, 
#' corrected 2020) equations (Stanojevic et al, Eur Respir J 2017). The output
#' of this function is used in calculating LLN and predicted values for TLCO
#' (transfer factor of the lung for carbon monoxide), KCO (transfer coefficient 
#' of the lung for carbon monoxide) and VA (alveolar volume).
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
#' "TLCO", "KCO" or "VA"
#' @param SI (default TRUE) Use SI (mmol/min/kPa) or traditional (ml/min/mmHg) units?
#'
#' @details Arguments \code{age}, \code{height} and \code{gender} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return A data.frame with L, M and S, for every patient and every parameter requested
getLMS_GLIdiff <- function(age, height, gender=1, param="TLCO", SI=TRUE) {
  param <- toupper(unique(param[!is.na(param)]))
  if (length(SI)>1)
    stop("Paramater 'SI' must be a logical of length 1.")
  if (length(param)==0)
    stop("You must specify at least one parameter to calculate.")
  if (sum(!(param %in% c("TLCO", "KCO", "VA")))>0)
    stop(sprintf("Argument 'param' must be one or more of \"%s\".",
        paste(unique(GLIdiff_lookup$SI$f), collapse='", "')))
  dat <- rspiro_check_somat(age, height, gender, 1, diff=TRUE)
  dat$id <- 1:nrow(dat)
  dat$ethnicity <- NULL
  
  u <- if (SI) "SI" else "trad"
  
  dat <- do.call(rbind, lapply(param, function(x) {dat$f <- x; dat}))

  dat <- merge(dat, GLIdiff_lookup[[u]], sort=FALSE, all.x=TRUE)
  dat <- dat[order(factor(dat$f, levels=levels(factor(GLIdiff_lookup[[u]]$f))), dat$id),]

  dat$L <- dat$M <- dat$S <- NA
  
  # From here on we calculate the L,M,S according to different equations...
  
  s <- dat$gender==1 & dat$f=="TLCO"
  if (SI) {
    dat$M[s] <- with(dat, exp(-8.129189 + 2.018368*log(height[s]*100) - 0.012425*log(age[s]) + Mspline[s]))
  } else {
    dat$M[s] <- with(dat, exp(-7.034920 + 2.018368*log(height[s]*100) - 0.012425*log(age[s]) + Mspline[s]))
  }
  dat$S[s] <- with(dat, exp(-1.98996 + 0.03536*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.39482
  
  s <- dat$gender==1 & dat$f=="KCO"
  if (SI) {
    dat$M[s] <- with(dat, exp(2.994137 - 0.415334*log(height[s]*100) - 0.113166*log(age[s]) + Mspline[s]))
  } else {
    dat$M[s] <- with(dat, exp(4.088408 - 0.415334*log(height[s]*100) - 0.113166*log(age[s]) + Mspline[s]))
  }
  dat$S[s] <- with(dat, exp(-1.98186 + 0.01460*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.67330
  
  s <- dat$gender==1 & dat$f=="VA"
  dat$M[s] <- with(dat, exp(-11.086573 + 2.430021*log(height[s]*100) + 0.097047*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.20953 + 0.01937*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.62559
    
  s <- dat$gender==2 & dat$f=="TLCO"
  if (SI) {
    dat$M[s] <- with(dat, exp(-6.253720 + 1.618697*log(height[s]*100) - 0.015390*log(age[s]) + Mspline[s]))
  } else {
    dat$M[s] <- with(dat, exp(-5.159451 + 1.618697*log(height[s]*100) - 0.015390*log(age[s]) + Mspline[s]))
  }
  dat$S[s] <- with(dat, exp(-1.82905 - 0.01815*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.24160
  
  s <- dat$gender==2 & dat$f=="KCO"
  if (SI) {
    dat$M[s] <- with(dat, exp(4.037222 - 0.645656*log(height[s]*100) - 0.097395*log(age[s]) + Mspline[s]))
  } else {
    dat$M[s] <- with(dat, exp(5.131492 - 0.645656*log(height[s]*100) - 0.097395*log(age[s]) + Mspline[s]))
  }
  dat$S[s] <- with(dat, exp(-1.63787 - 0.07757*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.48963
  
  s <- dat$gender==2 & dat$f=="VA"
  dat$M[s] <- with(dat, exp(-9.873970 + 2.182316*log(height[s]*100) + 0.082868*log(age[s]) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.08839 - 0.01334*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.51919
  
  
  dat[,c("id", "age", "height", "gender", "f", "L", "M", "S")]
}
