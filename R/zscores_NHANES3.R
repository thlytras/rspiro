#' Convert spirometric values to z-scores using NHANES III equations
#'
#' This function takes absolute spirometry measurements (FEV1, FVC, etc)
#' in lt plus demographic data (age, height, gender and ethnicity) and converts
#' them to z-scores based on the NHANES III equations.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 
#'   3 = Mexican-American). Default is 1.
#' @param FEV1 Forced Expiratory Volume in 1 second (lt)
#' @param FVC Forced Vital Capacity (lt)
#' @param FEV1FVC FEV1 / FVC ratio
#' @param PEF Peak Expiratory Flow (lt)
#' @param FEF2575 Forced Expiratory Flow between 25\% and 75\% of FVC (lt/s)
#' @param FEV6 Forced Expiratory Volume in 6 seconds (lt)
#' @param FEV1FEV6 FEV1 / FEV6 ratio
#'
#' @details At least one of the spirometric measurement arguments must be set (i.e. be
#' non-\code{NULL}). Arguments \code{age}, \code{height}, \code{gender} and
#' \code{ethnicity} must be vectors of length equal to the length of the
#' spirometric measurement vector(s), or of length one, in which case their
#' value is recycled. If any input vector is not of equal length, the function
#' stops with an error.
#' 
#' Normal distributions are assumed for all parameters, even though per the NHANES III 
#' equations paper (\href{https://www.atsjournals.org/doi/10.1164/ajrccm.159.1.9712108}{Hankinson et al})
#' this was observed only for FEV1, FVC, PEF and FEV6. 
#'
#' @return If only one spirometry argument is supplied, the function
#' returns a numeric vector. If more are supplied, the function returns
#' a data.frame with the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one parameter supplied (FEV1)
#' zscore_NHANES3(age=seq(25,40,4), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), ethnicity=rep(1,4), FEV1=c(3.5, 4, 3.6, 3.9))
#'
#' @importFrom stats reshape
#'
#' @export
zscore_NHANES3 <- function(age, height, gender=1, ethnicity=1,
        FEV1=NULL, FVC=NULL, FEV1FVC=NULL, PEF=NULL, FEF2575=NULL,
        FEV6=NULL, FEV1FEV6=NULL) {

  spiro_val <- list(FEV1=FEV1, FVC=FVC, FEV1FVC=FEV1FVC, PEF=PEF, 
              FEF2575=FEF2575, FEV6=FEV6, FEV1FEV6=FEV1FEV6)
  spiro_val <- spiro_val[!sapply(spiro_val, is.null)]
  spiro_val_len <- unique(sapply(spiro_val, length))
  dat <- rspiro_check_somat(age, height, gender, ethnicity, NHANES=TRUE)
  rspiro_check_input(spiro_val, dat)
  
  param <- names(spiro_val)
  dat$under20 <- dat$age<20
  dat$age2 <- dat$age^2
  dat$Intercept <- 1
  dat$height2 <- (dat$height * 100)^2
  dat$agebound <- NULL
  dat <- cbind(dat, spiro_val)

  for (p in param) {
    if (p %in% c("FEV1FVC", "FEV1FEV6")) {
      cf <- t(mapply(function(p, gend, ethn) { NHtb6[
        with(NHtb6, which(param==p & sex==gend & ethnicity==ethn)),
        c("interceptLLN", "interceptPRD", "Age")]
      }, p=p, gend=dat$gender, ethn=dat$ethnicity))
      cf <- matrix(as.numeric(cf), ncol=ncol(cf), nrow=nrow(cf), dimnames=dimnames(cf))
      dat[[paste0("pred.", p)]] <- unname(rowSums(dat[,c("Intercept","age")]*cf[,c("interceptPRD", "Age")]))/100
      dat[[paste0("LLN.", p)]] <- unname(rowSums(dat[,c("Intercept","age")]*cf[,c("interceptLLN", "Age")]))/100
    } else {
      cf <- t(mapply(function(p, gend, ethn, u20) { NHtb45[
        with(NHtb45, which(param==p & sex==gend & ethnicity==ethn & under20==u20)),
        c("intercept", "age", "age2", "HtPRD", "HtLLN")]
      }, p=p, gend=dat$gender, ethn=dat$ethnicity, u20=dat$under20))
      cf <- matrix(as.numeric(cf), ncol=ncol(cf), nrow=nrow(cf), dimnames=dimnames(cf))
      dat[[paste0("pred.", p)]] <- unname(rowSums(
        dat[,c("Intercept","age","age2","height2")] * cf[,c("intercept","age","age2","HtPRD")]))
      dat[[paste0("LLN.", p)]] <- unname(rowSums(
        dat[,c("Intercept","age","age2","height2")] * cf[,c("intercept","age","age2","HtLLN")]))
    }
    dat[[paste0("SE.", p)]] <- (dat[[paste0("pred.", p)]] - dat[[paste0("LLN.", p)]]) / 1.645
    dat[[paste0("z.score.", p)]] <- (dat[[p]] - dat[[paste0("pred.", p)]]) / dat[[paste0("SE.", p)]]
  }

  return(dat[,grep("z.score.", names(dat), fixed=TRUE)])
}
