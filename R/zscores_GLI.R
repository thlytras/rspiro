#' Convert spirometric values to z-scores using GLI-2012 equations
#'
#' This function takes absolute spirometry measurements (FEV1, FVC, etc)
#' in lt plus demographic data (age, height, gender and ethnicity) and converts
#' them to z-scores based on the GLI-2012 equations.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 3 = NE Asian, 4 = SE Asian,
#' 5 = Other/mixed). Default is 1.
#' @param FEV1 Forced Expiratory Volume in 1 second (lt)
#' @param FVC Forced Vital Capacity (lt)
#' @param FEV1FVC FEV1 / FVC * 100\%
#' @param FEF2575 Forced Expiratory Flow between 25\% and 75\% of FVC (lt/s)
#' @param FEF75 Forced Expiratory Flow at 75\% of FVC (lt/s)
#' @param FEV075 Forced Expiratory Volume in 0.75 sec (lt)
#' @param FEV075FVC FEV0.75 / FVC * 100\%
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
#' zscore_GLI(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), FEV1=c(3.5, 4, 3.6, 3.9))
#'
#' @importFrom stats reshape
#' 
#' @export
zscore_GLI <- function(age, height, gender=1, ethnicity=1,
        FEV1=NULL, FVC=NULL, FEV1FVC=NULL, FEF2575=NULL,
        FEF75=NULL, FEV075=NULL, FEV075FVC=NULL) {
  val <- list(FEV1=FEV1, FVC=FVC, FEV1FVC=FEV1FVC, FEF2575=FEF2575,
              FEF75=FEF75, FEV075=FEV075, FEV075FVC=FEV075FVC)
  val <- val[!sapply(val, is.null)]
  param <- names(val)
  if (length(val)==0)
    stop("At least one spirometry parameter must be specified.")
  val_len <- unique(sapply(val, length))
  if (length(val_len)>1)
    stop("Not all spirometry parameter vactors have the same length.")
  dat <- getLMS(age, height, gender, ethnicity, param)
  if (nrow(dat)==1 && val_len>1) {
    dat <- dat[rep(1,val_len),]
    rownames(dat) <- NULL
    dat$id <- 1:nrow(dat)
  }
  if (nrow(dat)!=val_len)
    stop("Spirometry parameter vector(s) and somatometric vectors
         (age, height, gender, ethnicity) do not have the same length.")

  val <- as.data.frame.matrix(do.call(cbind, val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="obs")
  dat <- merge(dat, val)

  dat$z.score <- with(dat, ((obs/M)^L-1)/(L*S))

  datw <- reshape(dat[,c("id","f", "age","height","ethnicity","gender","z.score")],
                  v.names="z.score", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  datw[,paste("z.score", unique(param), sep=".")]
}


