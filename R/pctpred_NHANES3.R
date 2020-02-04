#' Convert spirometric values to \% predicted using NHANES III equations
#'
#' This function takes absolute spirometry measurements (FEV1, FVC, etc)
#' in lt plus demographic data (age, height, gender and ethnicity) and converts
#' them to percent (\%) predicted based on the NHANES III equations.
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
#' @return If only one spirometry argument is supplied, the function
#' returns a numeric vector. If more are supplied, the function returns 
#' a data.frame with the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one parameter supplied (FEV1)
#' pctpred_NHANES3(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), FEV1=c(3.5, 4, 3.6, 3.9))
#'
#' @export
pctpred_NHANES3 <- function(age, height, gender=1, ethnicity=1,
        FEV1=NULL, FVC=NULL, FEV1FVC=NULL, PEF=NULL, FEF2575=NULL,
        FEV6=NULL, FEV1FEV6=NULL) {
  val <- list(FEV1=FEV1, FVC=FVC, FEV1FVC=FEV1FVC, PEF=PEF, 
              FEF2575=FEF2575, FEV6=FEV6, FEV1FEV6=FEV1FEV6)
  val <- val[!sapply(val, is.null)]
  param <- names(val)
  if (length(val)==0)
    stop("At least one spirometry parameter must be specified.")
  val_len <- unique(sapply(val, length))
  if (length(val_len)>1)
    stop("Not all spirometry parameter vactors have the same length.")
  preds <- pred_NHANES3(age, height, gender, ethnicity, param)
  if ((length(param)==1 && length(preds)!=val_len) || (length(param)>1 && nrow(preds)!=val_len))
    stop("Spirometry parameter vector(s) and somatometric vectors
         (age, height, gender, ethnicity) do not have the same length.")
  if (length(param)>1) {
    res <- as.data.frame(val)/preds*100
    names(res) <- paste0("pctpred.", names(res))
  } else {
    res <- unname(unlist(val)/preds*100)
  }
  return(res)
}


