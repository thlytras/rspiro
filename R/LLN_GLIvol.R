#' Calculate LLN of static lung volume parameters using GLI-2019 equations
#'
#' This function calculates LLNs (Lower Limits of Normal) for static lung
#' volumes (TLC, FRC, RV, IC, VC, ERV and RV/TLC) using the GLI-2019 equations
#' (Hall et al, ERJ 2021).
#' It accepts as input age, height and gender.
#' Note: equations are based on Caucasian data only.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels
#'   (first = male). Default is 1.
#' @param param A character vector, containing one or more of the following
#'   parameters (case insensitive):
#'   "TLC", "FRC", "RV", "IC", "VC", "ERV", "RVTLC"
#'
#' @details Arguments \code{age}, \code{height} and \code{gender} are vectors
#' of equal length, or of length one, in which case the value is recycled; if
#' the vectors are not of equal length, the function stops with an error.
#'
#' @return If \code{param} has length one, the function returns a numeric
#'   vector. If \code{param} has length >1, it returns a data.frame with
#'   \code{length(param)} columns.
#'
#' @examples
#' # Find LLN of TLC and FRC for women aged 20 to 70 and with a height of 1.70 meters.
#' LLN_GLIvol(20:70, 1.7, 2, param=c("TLC","FRC"))
#'
#' @importFrom stats reshape
#'
#' @export
LLN_GLIvol <- function(age, height, gender=1, param="TLC") {
  param <- toupper(param)
  dat <- getLMS_GLIvol(age, height, gender, param)

  # LLN = 5th percentile using Box-Cox back-transformation
  dat$LLN <- with(dat, exp(log(1 - 1.645 * L * S) / L + log(M)))

  datw <- reshape(dat[,c("id","f","age","height","gender","LLN")],
                  v.names="LLN", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  datw[,paste("LLN", unique(param), sep=".")]
}
