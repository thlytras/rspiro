#' Calculate LLN of spirometry parameters using GLI global (2022) equations
#'
#' This function calculates LLNs (Lower Limits of Normal) for the various spirometry
#' parameters, using the GLI global (2022) equations. 
#' It accepts as input age, height, gender.
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param param A character vector, containing one of more of the following parameters (case insensitive):
#' "FEV1", "FVC", "FEV1FVC"
#'
#' @details Arguments \code{age}, \code{height} and \code{gender} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return If \code{param} has length one, the function returns a numeric vector. If \code{param}
#' has length >1, it returns a data.frame with \code{length(param)} columns.
#'
#' @examples
#' # Find LLN of FEV1 and FVC for women aged 20 to 70 and with a height of 1.70 meters.
#' LLN_GLIgl(20:70, 1.7, 2, param=c("FEV1","FVC"))
#'
#' @importFrom stats reshape
#'
#' @export
LLN_GLIgl <- function(age, height, gender=1, param="FEV1") {
  param <- toupper(param)
  dat <- getLMS_GLIgl(age, height, gender, param)
  dat$LLN <- with(dat, exp(log(1 - 1.645 * L * S)/L + log(M)))

  datw <- reshape(dat[,c("id","f", "age","height","gender","LLN")],
    v.names="LLN", idvar="id", direction="wide", timevar="f")
  rownames(datw) <- NULL
  datw <- datw[order(datw$id),]

  datw[,paste("LLN", unique(param), sep=".")]
}
