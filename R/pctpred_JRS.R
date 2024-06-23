#' Convert spirometric values to \% predicted using JRS (2014) equations
#'
#' This function takes absolute spirometry measurements (FEV1, FVC, VC or FEV1FVC)
#' in lt plus demographic data (age, height, gender) and converts
#' them to percent (\%) predicted based on the JRS (Japanese Respiratory Society 2014) equations.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param FEV1 Forced Expiratory Volume in 1 second (lt)
#' @param FVC Forced Vital Capacity (lt)
#' @param VC Vital Capacity (lt)
#' @param FEV1FVC FEV1 / FVC ratio
#'
#' @details At least one of the spirometric measurement arguments must be set (i.e. be
#' non-\code{NULL}). Arguments \code{age}, \code{height} and \code{gender} 
#' must be vectors of length equal to the length of the
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
#' pctpred_JRS(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), FEV1=c(3.5, 4, 3.6, 3.9))
#'
#' @importFrom stats reshape
#' 
#' @export
pctpred_JRS <- function(age, height, gender=1, 
        FEV1=NULL, FVC=NULL, VC=NULL, FEV1FVC=NULL) {
  spiro_val <- list(FEV1=FEV1, FVC=FVC, VC=VC, FEV1FVC=FEV1FVC)
  spiro_val <- spiro_val[!sapply(spiro_val, is.null)]
  spiro_val_len <- unique(sapply(spiro_val, length))
  somat_val <- rspiro_check_somat(age, height, gender, 1, JRS=TRUE)
  rspiro_check_input(spiro_val, somat_val)
  
  param <- names(spiro_val)
  dat <- with(somat_val, getLMS_JRS(age, height, gender, param))
  if (nrow(dat)==1 && spiro_val_len>1) {
    dat <- dat[rep(1,spiro_val_len),]
    rownames(dat) <- NULL
    dat$id <- 1:nrow(dat)
  }
  
  val <- as.data.frame.matrix(do.call(cbind, spiro_val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="obs")
  dat <- merge(dat, val)

  dat$pctpred <- with(dat, obs/M*100)

  datw <- reshape(dat[,c("id","f", "age","height","gender","pctpred")],
                  v.names="pctpred", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  datw[,paste("pctpred", unique(param), sep=".")]
}


