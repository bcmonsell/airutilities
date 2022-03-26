#' Generate likelihood statistics (AIC, AICC, BIC, etc.)
#'
#' Generate likelihood statistics that include the Jacobian adjustment for transformations as done in the X-13ARIMA-SEATS program
#'
#' @param Y numeric vector, original time series.
#' @param Lnlkhd numeric scalar, maximized log likelihood of model.
#' @param Np Integer scalar, Number of parameters in model used. Default is 2.
#' @param Nintvl Integer scalar, Number of differences in model used. Default is 2.
#' @param Adj numeric vector, prior adjustment factor time series. Default is NULL, which indicates there is no prior adjustment.
#' @param Logit logical scalar, if TRUE the logit transformation is used. Default is FALSE.
#' @param Trans logical scalar, if TRUE a Box-Cox transform is used. Default is TRUE
#' @param Lam numeric scalar, Box-Cox transformation parameter. Default is 0 (Log transform).
#' @param Eick numeric scalar, weighting paramter for Empiracle Information Criterion. Default is NULL (EIC not computed).
#' @return List of likelihood diagnostics and related estimates (ll = log likelihood, jacadj = jacobian transformation adjustment, 
#'    lladj = adjusted likelihood, nobs = number of observations, neffective = effective number of observations,
#'    nparams = number of parameters, df = Nspobs - Nintvl - Np, aic = AIC, aicc = AICC, hannanquinn = Hannan Quinn,
#'    bic = BIC, bic2 = BIC2, EIC = Empiracle Information Criterion)
#' @examples
#' ic_default_log_est <- 
#'    rjd3highfreq::fractionalAirlineEstimation(log(ic_obs), periods=c(365.25/7), 
#'                                              x=ic_default_matrix)
#' ic_default_log_lkhd_stat <- 
#'    gen_likelihood_stats(ic_obs, 
#'                         Lnlkhd = ic_default_log_est$likelihood$ll, 
#'                         Nintvl = 2, 
#'                         Np     = ic_default_log_est$likelihood$nparams)
#' ic_default_log_aicc <- ic_default_log_lkhd_stat$aicc
#' @import stats
#' @export
gen_likelihood_stats <- function(Y, Lnlkhd, Nintvl = 2, Np = 2, Adj = NULL, 
                                 Logit = FALSE, Trans = TRUE, Lam = 0.0, 
                                 Eick = NULL) {
   # Author: Brian C. Monsell (OEUS), Version 2.0, 2/2/2022

   # generate jacobian adjustment
   Nspobs <- length(Y)
   jacadj <- jacobian_trans_adj(Y, Nspobs, Nintvl, Adj = Adj, Logit = Logit, Trans = Trans, Lam = Lam)   

   dnefob <- Nspobs - Nintvl
   TWO <- 2.0
   lladj <- Lnlkhd + jacadj
   
   # AIC
   Aic=-TWO*(lladj-Np)
   
   # AICC
   Aicc=-TWO*(lladj-dnefob*Np/(dnefob-(Np+1)))
   
   # Hannon-Quinn
   Hnquin=-TWO*(lladj-log(log(dnefob))*Np)
   
   # BIC
   Bic=-TWO*(lladj)+Np*log(dnefob)
   
   # BIC2
   Bic2=(-TWO*Lnlkhd+Np*log(dnefob))/dnefob
   
   # BICC
   
   # EIC (only produced if EICk is specified) and return a list of likelihood statistics
   if (!is.null(Eick)) {
      Eic=-TWO*(lladj)+Np*Eick
      return(list(ll = Lnlkhd, jacadj = jacadj, lladj = lladj, nobs = Nspobs, neffective = dnefob,
               nparams = Np, df = dnefob - Np, aic = Aic, aicc = Aicc, hannanquinn = Hnquin,
               bic = Bic, bic2 = Bic2, eic = Eic, eick = Eick))
   } else {
      return(list(ll = Lnlkhd, jacadj = jacadj, lladj = lladj, nobs = Nspobs, neffective = dnefob,
               nparams = Np, df = dnefob - Np, aic = Aic, aicc = Aicc, hannanquinn = Hnquin,
               bic = Bic, bic2 = Bic2))
   }
   
}
