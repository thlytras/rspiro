#' Convert z-scores back to raw spirometric values using GLI-2012 equations
#'
#' This function takes z-scores based on the GLI-2012 equations, plus
#' demographic data (age, height, gender and ethnicity), and converts them back
#' into absolute spirometry measurements (FEV1, FVC, etc) in lt.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 3 = NE Asian, 4 = SE Asian,
#' 5 = Other/mixed). Default is 1.
#' @param FEV1 Forced Expiratory Volume in 1 second (z-score)
#' @param FVC Forced Vital Capacity (z-score)
#' @param FEV1FVC FEV1 / FVC * 100\% (as z-score)
#' @param FEF2575 Forced Expiratory Flow between 25\% and 75\% of FVC (z-score)
#' @param FEF75 Forced Expiratory Flow at 75\% of FVC (z-score)
#' @param FEV075 Forced Expiratory Volume in 0.75 sec (z-score)
#' @param FEV075FVC FEV0.75 / FVC * 100\% (as z-score)
#'
#' @details At least one of the spirometric z-score arguments must be set (i.e. be
#' non-\code{NULL}). Arguments \code{age}, \code{height}, \code{gender} and
#' \code{ethnicity} must be vectors of length equal to the length of the
#' z-score vector(s), or of length one, in which case their value is recycled.
#' If any input vector is not of equal length, the function stops with an error.
#'
#' @return If only one spirometry z-score argument is supplied, the function
#' returns a numeric vector. If more are supplied, the function returns 
#' a data.frame with the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one z-score parameter supplied (FEV1)
#' raw_GLI(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), FEV1=c(-1.2, -1.9, 0, 0.5))
#'
#' @importFrom stats reshape
#' 
#' @export
raw_GLI <- function(age, height, gender=1, ethnicity=1,
        FEV1=NULL, FVC=NULL, FEV1FVC=NULL, FEF2575=NULL,
        FEF75=NULL, FEV075=NULL, FEV075FVC=NULL) {
  val <- list(FEV1=FEV1, FVC=FVC, FEV1FVC=FEV1FVC, FEF2575=FEF2575,
              FEF75=FEF75, FEV075=FEV075, FEV075FVC=FEV075FVC)
  val <- val[!sapply(val, is.null)]
  param <- names(val)
  if (length(val)==0)
    stop("At least one spirometry z-score parameter must be specified.")
  val_len <- unique(sapply(val, length))
  if (length(val_len)>1)
    stop("Not all z-score parameter vectors have the same length.")
  dat <- getLMS(age, height, gender, ethnicity, param)
  if (nrow(dat)==1 && val_len>1) {
    dat <- dat[rep(1,val_len),]
    rownames(dat) <- NULL
    dat$id <- 1:nrow(dat)
  }
  if (nrow(dat)!=val_len*length(val))
    stop("Spirometry z-score parameter vector(s) and somatometric vectors
         (age, height, gender, ethnicity) do not have the same length.")

  val <- as.data.frame.matrix(do.call(cbind, val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="z")
  dat <- merge(dat, val)

  dat$raw <- with(dat, M*(z*L*S + 1)^(1/L))

  datw <- reshape(dat[,c("id","f", "age","height","ethnicity","gender","raw")],
                  v.names="raw", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]
  datw <- datw[,paste("raw", unique(param), sep=".")]
  if (!is.null(ncol(datw))) names(datw) <- gsub("raw.", "", names(datw), fixed=TRUE)
  
  datw
}
