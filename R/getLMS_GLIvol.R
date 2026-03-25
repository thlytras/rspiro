#' Calculate L,M,S of GLI-2019 static lung volume equations
#'
#' This function calculates the L,M,S using the GLI-2019 static lung volume
#' equations (Hall et al, ERJ 2021). The output of this function is used in
#' calculating the LLN and predicted lung volume parameters.
#' Checking of the input is also performed here.
#'
#' This is for internal use only.
#'
#' @keywords internal
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
#' @return A data.frame with L, M and S, for every patient and every parameter
#'   requested
getLMS_GLIvol <- function(age, height, gender=1, param="TLC") {

  # ── Input validation ──────────────────────────────────────────────
  param <- toupper(unique(param[!is.na(param)]))
  if (length(param) == 0)
    stop("You must specify at least one parameter to calculate.")
  if (sum(!(param %in% c("TLC", "FRC", "RV", "IC", "VC", "ERV", "RVTLC"))) > 0)
    stop(sprintf("Argument 'param' must be one or more of \"%s\".",
                 paste(unique(GLIvol_lookup$f), collapse='", "')))

  # ── Prepare patient data ──────────────────────────────────────────
  dat <- rspiro_check_somat(age, height, gender, 1, vol=TRUE)
  dat$id <- 1:nrow(dat)
  dat$ethnicity <- NULL

  # Duplicate rows for each requested parameter
  dat <- do.call(rbind, lapply(param, function(x) {dat$f <- x; dat}))

  # Merge with lookup table to get Mspline and Sspline values
  # Note: ERV, IC, VC have no Sspline in the paper — these will be NA after
  # merge, which is handled correctly in the S equations below
  dat <- merge(dat, GLIvol_lookup, sort=FALSE, all.x=TRUE)
  dat <- dat[order(factor(dat$f, levels=levels(factor(GLIvol_lookup$f))), dat$id),]

  # Initialise L, M, S
  dat$L <- dat$M <- dat$S <- NA

  # ── Equations from Hall et al, ERJ 2021, Table 2 ───────────────────────────
  # NOTE: height is converted from meters to cm (*100) where log(height) is used
  # For RV and RV/TLC, height enters as raw cm (not log)

  # ── FRC ─────────────────────────────────────────────────────────────────────
  s <- dat$gender==1 & dat$f=="FRC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-13.4898 + 0.1111*log(age[s]) + 2.7634*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.60197 + 0.01513*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.3416

  s <- dat$gender==2 & dat$f=="FRC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-12.7674 + 0.1251*log(age[s]) + 2.6049*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.48310 - 0.03372*log(age[s]) + Sspline[s]))
  dat$L[s] <- 0.2898

  # ── TLC ─────────────────────────────────────────────────────────────────────
  s <- dat$gender==1 & dat$f=="TLC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-10.5861 + 0.1433*log(age[s]) + 2.3155*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.0616143 - 0.0008534*age[s] + Sspline[s]))
  dat$L[s] <- 0.9337

  s <- dat$gender==2 & dat$f=="TLC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-10.1128 + 0.1062*log(age[s]) + 2.2259*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.0999321 + 0.0001564*age[s] + Sspline[s]))
  dat$L[s] <- 0.4636

  # ── RV ──────────────────────────────────────────────────────────────────────
  # Note: RV uses age and height directly (not log-transformed)
  s <- dat$gender==1 & dat$f=="RV" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-2.37211 + 0.01346*age[s] + 0.01307*height[s]*100 + Mspline[s]))
  dat$S[s] <- with(dat, exp(-0.878572 - 0.007032*age[s] + Sspline[s]))
  dat$L[s] <- 0.5931

  s <- dat$gender==2 & dat$f=="RV" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-2.50593 + 0.01307*age[s] + 0.01379*height[s]*100 + Mspline[s]))
  dat$S[s] <- with(dat, exp(-0.902550 - 0.006005*age[s] + Sspline[s]))
  dat$L[s] <- 0.4197

  # ── RV/TLC ──────────────────────────────────────────────────────────────────
  # Note: RV/TLC uses age directly and height in cm directly (not log)
  s <- dat$gender==1 & dat$f=="RVTLC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(2.634 + 0.01302*age[s] - 0.00008862*height[s]*100 + Mspline[s]))
  dat$S[s] <- with(dat, exp(-0.96804 - 0.01004*age[s] + Sspline[s]))
  dat$L[s] <- 0.8646

  s <- dat$gender==2 & dat$f=="RVTLC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(2.666 + 0.01411*age[s] - 0.00003689*height[s]*100 + Mspline[s]))
  dat$S[s] <- with(dat, exp(-0.976602 - 0.009679*age[s] + Sspline[s]))
  dat$L[s] <- 0.8037

  # ── ERV ─────────────────────────────────────────────────────────────────────
  # Note: ERV has no Sspline — S equation uses age only
  s <- dat$gender==1 & dat$f=="ERV" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-17.328650 - 0.006288*age[s] + 3.478116*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.307616 + 0.009177*age[s]))
  dat$L[s] <- 0.5517

  s <- dat$gender==2 & dat$f=="ERV" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-14.145513 - 0.009573*age[s] + 2.871446*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.54992 + 0.01409*age[s]))
  dat$L[s] <- 0.5326

  # ── IC ──────────────────────────────────────────────────────────────────────
  # Note: IC has no Sspline — S equation uses age only
  s <- dat$gender==1 & dat$f=="IC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-10.121688 + 0.001265*age[s] + 2.188801*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.856546 + 0.002008*age[s]))
  dat$L[s] <- 1.146

  s <- dat$gender==2 & dat$f=="IC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-9.4438787 - 0.0002484*age[s] + 2.0312769*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-1.775276 + 0.002673*age[s]))
  dat$L[s] <- 0.9726

  # ── VC ──────────────────────────────────────────────────────────────────────
  # Note: VC has no Sspline — S equation uses age only
  s <- dat$gender==1 & dat$f=="VC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-10.134371 - 0.003532*age[s] + 2.307980*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.1367411 + 0.0009367*age[s]))
  dat$L[s] <- 0.8611

  s <- dat$gender==2 & dat$f=="VC" & !is.na(dat$gender)
  dat$M[s] <- with(dat, exp(-9.230600 - 0.005517*age[s] + 2.116822*log(height[s]*100) + Mspline[s]))
  dat$S[s] <- with(dat, exp(-2.220260 + 0.002956*age[s]))
  dat$L[s] <- 1.038

  # ── Return ───────────────────────────────────────────────────────────────────
  dat[, c("id", "age", "height", "gender", "f", "L", "M", "S")]
}
