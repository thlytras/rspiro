#' Convert z-scores back to raw diffusing capacity values using GLI (2017) equations
#'
#' This function takes z-scores based on the GLI (2017) equations, 
#' plus demographic data (age, height and gender), and converts them back
#' into absolute diffusing capacity measurements (TLCO, KCO and VA).
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param TLCO Transfer factor of the lung for carbon monoxide (z-score)
#' @param KCO Transfer coefficient of the lung for carbon monoxide (z-score)
#' @param VA Alveolar volume (z-score)
#' @param SI (default TRUE) Use SI (mmol/min/kPa) or traditional (ml/min/mmHg) units?
#'
#' @details At least one of the diffusing capacity z-score arguments must be set (i.e. be
#' non-\code{NULL}). Arguments \code{age}, \code{height} and \code{gender} 
#' must be vectors of length equal to the length of the
#' z-score vector(s), or of length one, in which case their value is recycled.
#' If any input vector is not of equal length, the function stops with an error.
#'
#' @return If only one diffusing capacity z-score argument is supplied, the function
#' returns a numeric vector. If more are supplied, the function returns 
#' a data.frame with the same number of columns.
#'
#' @examples
#' # Random data, 4 patients, one z-score parameter supplied (TLCO)
#' raw_GLIdiff(age=seq(25,40,5), height=c(1.8, 1.9, 1.75, 1.85),
#'       gender=c(2,1,2,1), TLCO=c(-1.2, -1.9, 0, 0.5))
#'
#' @importFrom stats reshape
#' 
#' @export
raw_GLIdiff <- function(age, height, gender=1,
        TLCO=NULL, KCO=NULL, VA=NULL, SI=TRUE) {
  spiro_val <- list(TLCO=TLCO, KCO=KCO, VA=VA)
  spiro_val <- spiro_val[!sapply(spiro_val, is.null)]
  spiro_val_len <- unique(sapply(spiro_val, length))
  somat_val <- rspiro_check_somat(age, height, gender, 1, diff=TRUE)
  rspiro_check_input(spiro_val, somat_val)

  param <- names(spiro_val)
  dat <- with(somat_val, getLMS_GLIdiff(age, height, gender, param, SI))
  if (nrow(dat)==1 && spiro_val_len>1) {
    dat <- dat[rep(1,spiro_val_len),]
    rownames(dat) <- NULL
    dat$id <- 1:nrow(dat)
  }

  val <- as.data.frame.matrix(do.call(cbind, spiro_val))
  val$id <- 1:nrow(val)
  val <- reshape(val, direction="long", varying=param, times=param, timevar="f", v.names="z")
  dat <- merge(dat, val)

  dat$raw <- with(dat, M*(z*L*S + 1)^(1/L))

  datw <- reshape(dat[,c("id","f", "age","height","gender","raw")],
                  v.names="raw", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]
  datw <- datw[,paste("raw", unique(param), sep=".")]
  if (!is.null(ncol(datw))) names(datw) <- gsub("raw.", "", names(datw), fixed=TRUE)
  
  datw
}
