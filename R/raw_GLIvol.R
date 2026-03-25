#' Convert z-scores back to raw static lung volume values using GLI-2019 equations
#'
#' This function takes z-scores based on the GLI-2019 equations,
#' plus demographic data (age, height and gender), and converts them back
#' into absolute static lung volume measurements (TLC, FRC, RV, IC, VC,
#' ERV and RV/TLC).
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels
#'   (first = male). Default is 1.
#' @param TLC Total Lung Capacity z-score
#' @param FRC Functional Residual Capacity z-score
#' @param RV Residual Volume z-score
#' @param IC Inspiratory Capacity z-score
#' @param VC Vital Capacity z-score
#' @param ERV Expiratory Reserve Volume z-score
#' @param RVTLC RV/TLC ratio z-score
#'
#' @details At least one of the lung volume z-score arguments must be set
#' (i.e. be non-\code{NULL}). Arguments \code{age}, \code{height} and
#' \code{gender} must be vectors of length equal to the length of the z-score
#' vector(s), or of length one, in which case their value is recycled.
#' If any input vector is not of equal length, the function stops with an error.
#'
#' @return If only one z-score argument is supplied, the function returns a
#' numeric vector. If more are supplied, the function returns a data.frame
#' with the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one z-score parameter supplied (TLC)
#' raw_GLIvol(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), TLC=c(-1.2, -1.9, 0, 0.5))
#'
#' @importFrom stats reshape
#'
#' @export
raw_GLIvol <- function(age, height, gender=1,
                       TLC=NULL, FRC=NULL, RV=NULL, IC=NULL, VC=NULL, ERV=NULL, RVTLC=NULL) {

  # Collect non-NULL z-score parameters into a named list
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

  # Reshape z-scores to long format for merging
  val <- as.data.frame.matrix(do.call(cbind, vol_val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="z")

  # Merge with LMS data and apply inverse Box-Cox transformation
  # Formula: raw = M * (z*L*S + 1)^(1/L)
  # This is the exact inverse of: z = ((obs/M)^L - 1) / (L*S)
  dat <- merge(dat, val)
  dat$raw <- with(dat, M * (z*L*S + 1)^(1/L))

  # Reshape back to wide format
  datw <- reshape(dat[,c("id","f","age","height","gender","raw")],
                  v.names="raw", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  # Select and clean up column names — remove "raw." prefix
  datw <- datw[,paste("raw", unique(param), sep=".")]
  if (!is.null(ncol(datw))) names(datw) <- gsub("raw.", "", names(datw), fixed=TRUE)

  datw
}
