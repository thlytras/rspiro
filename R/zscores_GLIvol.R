#' Convert static lung volume values to z-scores using GLI-2019 equations
#'
#' This function takes absolute static lung volume measurements (TLC, FRC, RV,
#' IC, VC, ERV and RV/TLC) plus demographic data (age, height and gender) and
#' converts them to z-scores based on the GLI-2019 equations
#' (Hall et al, ERJ 2021).
#' Note: equations are based on Caucasian data only.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels
#'   (first = male). Default is 1.
#' @param TLC Total Lung Capacity (in litres)
#' @param FRC Functional Residual Capacity (in litres)
#' @param RV Residual Volume (in litres)
#' @param IC Inspiratory Capacity (in litres)
#' @param VC Vital Capacity (in litres)
#' @param ERV Expiratory Reserve Volume (in litres)
#' @param RVTLC RV/TLC ratio
#'
#' @details At least one of the lung volume measurement arguments must be set
#' (i.e. be non-\code{NULL}). Arguments \code{age}, \code{height} and
#' \code{gender} must be vectors of length equal to the length of the lung
#' volume measurement vector(s), or of length one, in which case their value
#' is recycled. If any input vector is not of equal length, the function stops
#' with an error.
#'
#' @return If only one lung volume argument is supplied, the function returns a
#' numeric vector. If more are supplied, the function returns a data.frame with
#' the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one parameter supplied (TLC)
#' zscore_GLIvol(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), TLC=c(5.2, 6.1, 4.9, 5.8))
#'
#' @importFrom stats reshape
#'
#' @export
zscore_GLIvol <- function(age, height, gender=1,
                          TLC=NULL, FRC=NULL, RV=NULL, IC=NULL, VC=NULL, ERV=NULL, RVTLC=NULL) {

  # Collect non-NULL parameters into a named list
  vol_val <- list(TLC=TLC, FRC=FRC, RV=RV, IC=IC, VC=VC, ERV=ERV, RVTLC=RVTLC)
  vol_val <- vol_val[!sapply(vol_val, is.null)]
  vol_val_len <- unique(sapply(vol_val, length))

  # Validate inputs
  somat_val <- rspiro_check_somat(age, height, gender, 1, vol=TRUE)
  rspiro_check_input(vol_val, somat_val)

  param <- names(vol_val)
  dat <- with(somat_val, getLMS_GLIvol(age, height, gender, param))

  # Handle edge case: single demographic row but multiple measurements
  if (nrow(dat)==1 && vol_val_len>1) {
    dat <- dat[rep(1, vol_val_len),]
    rownames(dat) <- NULL
    dat$id <- 1:nrow(dat)
  }

  # Reshape measured values to long format for merging
  val <- as.data.frame.matrix(do.call(cbind, vol_val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="obs")

  # Merge with LMS data and calculate z-score using Box-Cox transformation
  dat <- merge(dat, val)
  dat$z.score <- with(dat, ((obs/M)^L - 1) / (L*S))

  # Reshape back to wide format
  datw <- reshape(dat[,c("id","f","age","height","gender","z.score")],
                  v.names="z.score", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  datw[,paste("z.score", unique(param), sep=".")]
}
