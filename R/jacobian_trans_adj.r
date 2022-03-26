#' Jacobian transformation adjustment
#'
#' compute the Jacobian adjustment for transformations as done in the X-13ARIMA-SEATS program
#'
#' @param Y numeric vector, original time series.
#' @param Nspobs Integer scalar, Length of \code{Y}.
#' @param Nintvl Integer scalar, Number of differences in model used. Default is 2.
#' @param Adj numeric vector, prior adjustment factor time series. Default is NULL, which indicates there is no prior adjustment.
#' @param Logit logical scalar, if TRUE the logit transformation is used. Default is FALSE.
#' @param Trans logical scalar, if TRUE a Box-Cox transform is used. Default is TRUE
#' @param Lam numeric scalar, Box-Cox transformation parameter. Default is 0 (Log transform).
#' @return Jacobean adjustment to the likelihood of estimated models to allow AIC and other likelihood statistics to be compared for different transformations.
#' @examples
#' Nspobs <- length(ic_obs)
#' ic_jacadj_log   <- jacobian_trans_adj(ic_obs, Nspobs, 2, Trans = TRUE, Lam = 0.0)   
#' @import stats
#' @export
jacobian_trans_adj <- function(Y, Nspobs, Nintvl = 2, Adj = NULL, Logit = FALSE, Trans = TRUE, Lam=0.0) {
    # Author: Brian C. Monsell (OEUS), Version 2.5, 3/2/2022
   this_index <- seq(Nintvl+1, Nspobs)
   if (is.null(Adj)) {
      Adj <- rep(1.0, Nspobs)
   } else {
      nadj <- length(Adj)
      if (nadj != Nspobs) {
         stop("series and prior adjustments not the same length")
      }
   }
   
   yi   <- Y[this_index]
   jaci <- Adj[this_index]
   
   
#           logit adjustment : jaci=jaci/(jaci*Y(i)-Y(i)**2)
   if (Logit) {
      jacadj <- sum(log(jaci/(yi - yi*yi)))
   } else {
#  transformation adjustment : jaci=(abs(yi)**(Lam-ONE))/jaci
      if (Trans) {
         jacadj <- sum(log((abs(yi)^(Lam-1))/jaci))
      } else {
         jacadj <- 0
      }
   }
   
   return(jacadj)
}