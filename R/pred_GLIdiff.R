#' Calculate predicted values of diffusing capacity using GLI (2017) equations
#'
#' This function calculates the mean normal (predicted) values for TLCO
#' (transfer factor of the lung for carbon monoxide), KCO (transfer coefficient 
#' of the lung for carbon monoxide) and VA (alveolar volume)
#' using the GLI (2017) equations. It accepts as input age, height and gender.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param param A character vector, containing one of more of the following parameters (case insensitive):
#' "TLCO", "KCO" or "VA"
#' @param SI (default TRUE) Use SI (mmol/min/kPa) or traditional (ml/min/mmHg) units?
#'
#' @details Arguments \code{age}, \code{height} and \code{gender} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return If \code{param} has length one, the function returns a numeric vector. If \code{param}
#' has length >1, it returns a data.frame with \code{length(param)} columns.
#'
#' @examples
#' # Find predicted TLCO and VA for women aged 20 to 70 and with a height of 1.70 meters.
#' pred_GLIdiff(20:70, 1.7, 2, param=c("TLCO","VA"))
#'
#' @importFrom stats reshape
#'
#' @export
pred_GLIdiff <- function(age, height, gender=1, param="TLCO", SI=TRUE) {
  param <- toupper(param)
  dat <- getLMS_GLIdiff(age, height, gender, param, SI)

  datw <- reshape(dat[,c("id","f", "age","height","gender","M")],
    v.names="M", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]
  names(datw)[-(1:4)] <- gsub("M.", "pred.", names(datw)[-(1:4)], fixed=TRUE)

  datw[,paste("pred", unique(param), sep=".")]
}


