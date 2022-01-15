#' Calculate L,M,S of GLI-2012
#'
#' This function calculates the L,M,S splines using GLI-2012 equations. The output
#' of this function is used in calculating the LLN and predicted spirometry parameters.
#' Checking of the input is also performed here.
#'
#' This is for internal use only.
#'
#' @keywords internal
#'
#' @param age Age in years
#' @param height Height in meters
#' @param gender Gender (1 = male, 2 = female) or a factor with two levels (first = male). Default is 1.
#' @param ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 3 = NE Asian, 4 = SE Asian,
#' 5 = Other/mixed). Default is 1.
#' @param param A character vector, containing one of more of the following parameters (case insensitive):
#' "FEV1", "FVC", "FEV1FVC", "FEF2575", "FEF75", "FEV075", "FEV075FVC"
#'
#' @details Arguments \code{age}, \code{height}, \code{gender} and \code{ethnicity} are vectors
#' of equal length, or of length one, in which case the value is recycled; if the four vectors are
#' not of equal length, the function stops with an error.
#'
#' @return A data.frame with L, M and S, for every patient and every parameter requested
getLMS <- function(age, height, gender=1, ethnicity=1, param="FEV1") {
  param <- toupper(unique(param[!is.na(param)]))
  if (length(param)==0)
    stop("You must specify at least one parameter to calculate.")
  if (sum(!(param %in% levels(lookup$f)))>0)
    stop(sprintf("Argument 'param' must be one or more of \"%s\".",
        paste(levels(lookup$f), collapse='", "')))
  dat <- rspiro_check_somat(age, height, gender, ethnicity)
  dat$id <- 1:nrow(dat)

  dat <- do.call(rbind, lapply(param, function(x) {dat$f <- x; dat}))

  dat <- merge(dat, lookup, sort=FALSE, all.x=TRUE)
  dat <- dat[order(factor(dat$f, levels=levels(lookup$f)), dat$id),]

  dat$Lspline <- with(dat, l0 + (l1-l0)*(age-agebound)/0.25)
  dat$Mspline <- with(dat, m0 + (m1-m0)*(age-agebound)/0.25)
  dat$Sspline <- with(dat, s0 + (s1-s0)*(age-agebound)/0.25)
  dat$L <- with(dat, q0 + q1 * log(age) + Lspline)
  dat$M <- with(dat, exp(a0 + a1 * log(height*100) + a2 * log(age) +
        a3 * (ethnicity==2) + a4 * (ethnicity==3) +
        a5 * (ethnicity==4) + a6 * (ethnicity==5) +
        Mspline))
  dat$S <- with(dat, exp(p0 + p1 * log(age) +
        p2 * (ethnicity==2) + p3 * (ethnicity==3) +
        p4 * (ethnicity==4) + p5 * (ethnicity==5) +
        Sspline))

  dat[,c("id", "age", "height", "gender", "ethnicity", "f", "L", "M", "S")]
}
