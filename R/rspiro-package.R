#' rspiro: Brief overview of the package
#' 
#' @description
#' R package \bold{rspiro} implements multiple spirometry equations: 
#' currently the GLI-2012 (Quanjer) and NHANES III (Hankinson), with 
#' potentially more to be added later. It offers a convenient interface
#' to calculate predicted or LLN (Lower Limit of Normal) values given
#' demographic data, or to convert absolute spirometry values to percent (%) 
#' predicted or z-scores.
#'
#' @details
#' To ensure a consistent interface, 
#' package functions are named with a prefix indicating the functionality 
#' and a suffix indicating the spirometric equations used, for example 
#' \code{\link{LLN_GLI}} calculates Lower Limits of Normal using the GLI-2012
#' equations. The suffix is currently one of 'GLI' or 'NHANES3'. 
#' The prefix is one of 'LLN_', 'pred_', 'pctpred_' or 'zscore_'. 
#' 
#' Functions
#' prefixed 'LLN_' or 'pred_' accept as input demographic information (age,
#' gender, height, ethnicity) and calculate the Lower Limit of Normal and
#' the predicted value, respectively, for a given spirometry parameter (FEV1,
#' FVC, etc). 
#' Functions prefixed 'pctpred_' or 'zscore_' accept absolute spirometry 
#' values (plus demographics) and convert those to percent (%) predicted and
#' z-scores, respectively. 
#' Please note the difference between 'pred_' and 'pctpred_' above.
#'
#' For detailed information, refer to the respective function documentations.
#' 
#' The development version of \bold{rspiro} is available on GitHub
#' \url{https://github.com/thlytras/rspiro}.
#' To report problems and bugs, or to request a feature, please go 
#' there and open an issue. Alternatively, send an email to 
#' Theodore Lytras \email{thlytras@@gmail.com}.
#'
#' @name rspiro-package
#' 
#' @aliases rspiro-package rspiro
#' 
#' @docType package
#' 
#' @author Theodore Lytras \email{thlytras@@gmail.com}.
#' 
#' @keywords package

NULL
