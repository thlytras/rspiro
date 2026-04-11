#' Calculate predicted values of static lung volumes using GLI-2019 equations
#'
#' This function calculates the mean normal (predicted) values for static lung
#' volumes (TLC, FRC, RV, IC, VC, ERV and RV/TLC) using the GLI-2019 equations
#' (Hall et al, ERJ 2021). It accepts as input age, height and gender.
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
#' # Find predicted TLC and FRC for women aged 20 to 70 and with a height of 1.70 meters.
#' pred_GLIvol(20:70, 1.7, 2, param=c("TLC","FRC"))
#'
#' @importFrom stats reshape
#'
#' @export
pred_GLIvol <- function(age, height, gender=1, param="TLC") {
  param <- toupper(param)
  dat <- getLMS_GLIvol(age, height, gender, param)

  datw <- reshape(dat[,c("id","f", "age","height","gender","M")],
                  v.names="M", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]
  names(datw)[-(1:4)] <- gsub("M.", "pred.", names(datw)[-(1:4)], fixed=TRUE)

  datw[,paste("pred", unique(param), sep=".")]
}
