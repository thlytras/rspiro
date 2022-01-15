#' Calculate predicted values of spirometry parameters using NHANES III equations
#'
#' This function calculates LLNs (Lower Limits of Normal) for the various spirometry
#' parameters, using the NHANES III equations. It accepts as input age, height, gender and ethnicity.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American,
#'   3 = Mexican-American). Default is 1.
#' @param param A character vector, containing one of more of the following parameters (case insensitive):
#' "FEV1", "FVC", "FEV1FVC", "PEF", "FEF2575", "FEV6", "FEV1FEV6"
#'
#' @details Arguments \code{age}, \code{height}, \code{gender} and \code{ethnicity} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return If \code{param} has length one, the function returns a numeric vector. If \code{param}
#' has length >1, it returns a data.frame with \code{length(param)} columns.
#'
#' @examples
#' # Find LLN of FEV1 and FVC for Caucasian women aged 20 to 70 and with a height of 1.70 meters.
#' LLN_NHANES3(20:70, 1.7, 2, param=c("FEV1","FVC"))
#'
#' @export
LLN_NHANES3 <- function(age, height, gender=1, ethnicity=1, param="FEV1") {
  param <- toupper(param)
  param <- param[param %in% c("FEV1", "FVC", "FEV1FVC", "PEF", "FEF2575", "FEV6", "FEV1FEV6")]
  if (length(param)==0) stop("No valid parameters found in argument 'param'!")

  dat <- rspiro_check_somat(age, height, gender, ethnicity, NHANES=TRUE)
  dat$under20 <- dat$age<20
  dat$age2 <- dat$age^2
  dat$Intercept <- 1
  dat$height2 <- (dat$height * 100)^2

  for (p in param) {
    if (p %in% c("FEV1FVC", "FEV1FEV6")) {
      cf <- t(mapply(function(p, gend, ethn) { NHtb6[
        with(NHtb6, which(param==p & sex==gend & ethnicity==ethn)),
        c("interceptLLN", "Age")]
      }, p=p, gend=dat$gender, ethn=dat$ethnicity))
      cf <- matrix(as.numeric(cf), ncol=ncol(cf), nrow=nrow(cf), dimnames=dimnames(cf))
      dat[[paste0("LLN.", p)]] <- unname(rowSums(dat[,c("Intercept","age")]*cf))/100
    } else {
      cf <- t(mapply(function(p, gend, ethn, u20) { NHtb45[
        with(NHtb45, which(param==p & sex==gend & ethnicity==ethn & under20==u20)),
        c("intercept", "age", "age2", "HtLLN")]
      }, p=p, gend=dat$gender, ethn=dat$ethnicity, u20=dat$under20))
      cf <- matrix(as.numeric(cf), ncol=ncol(cf), nrow=nrow(cf), dimnames=dimnames(cf))
      dat[[paste0("LLN.", p)]] <- unname(rowSums(dat[,c("Intercept","age","age2","height2")]*cf))
    }
  }

  return(dat[,grep("LLN", names(dat), fixed=TRUE)])

}


