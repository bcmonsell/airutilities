#' Fractional airline model summary
#'
#' Generates a matrix with coefficient estimates from the fractional airline model, generated by the JDemtetra+ high dimension modeling R routines
#'
#' @param this_est A list object generated by the estimation procedure of the fractional airline model
#' @param xreg_names Character vector; Names of the user-defined regression variables.
#' @param this_week Numeric vector; Week of each observation. Default: NULL
#' @param this_year Numeric vector; Year of each observation. Default: NULL
#' @return Generate matrix of model parameters estimated for the fractional airline model
#' @examples
#' ic_default_est <-
#'    rjd3highfreq::fractionalAirlineEstimation(ic_obs, periods=c(365.25/7), x=ic_default_matrix)
#' ic_default_model <- 
#'    gen_air_model_matrix(ic_default_est,
#'                         xreg_names = colnames(ic_default_matrix),
#'                         this_week = ic_week, this_year = ic_year)
#' @import stats
#' @export
gen_air_model_matrix <- function(this_est, xreg_names = NULL, this_week = NULL, this_year = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 3.0, 2/2/2022
    
    # create variable for coefficient estimate, both regressors and ARMA parameters
    this_b <- c(this_est$model$b, this_est$estimation$parameters)
    
    # generate t-statistics, both regressors and ARMA parameters
    t1 <- t_values_air(this_est$model$b, this_est$model$bcov)
    t2 <- t_values_air(this_est$estimation$parameters, this_est$estimation$covariance)
    this_t <- c(t1, t2)
    
    if (is.null(xreg_names)) {
        # collect names of both regressors and ARMA parameters
        this_names <- c(this_est$model$variables, "theta_nonseas", "theta_seasonal")
    } else {
        # set length of regression names, regressors used in the model estimation
        i1 <- length(xreg_names)
        i2 <- length(this_est$model$variables)
        
        # Check if number of user provided names is less than the number of regressors actually used This
        # means that outliers have been automatically identified
        if (length(xreg_names) < length(this_est$model$variables)) {
            # If \code{this_week} and \code{this_year} are not specified, use the names provided for the
            # automatic outliers identified
            if (is.null(this_week) | is.null(this_year)) {
                this_names <- c(xreg_names, this_est$model$variables[(i1 + 1):i2], "theta_nonseas", 
                  "theta_seasonal")
            } else {
                # Else change the names of the automatic outliers to be based on the dates of the outliers rather
                # than the absolute position of the observation First, initialize the vector
                # \code{this_xreg_names}
                this_xreg_names <- array(" ", dim = i2)
                for (i in 1:i1) {
                  this_xreg_names[i] <- xreg_names[i]
                }
                for (i in (i1 + 1):i2) {
                  # Then, split the name of each automatically identified outlier into a vector with the type of the
                  # outlier and the relative position of the outlier
                  this_reg <- strsplit(unlist(this_est$model$variables[i]), ".", fixed = TRUE)[[1]]
                  # create new outlier name pasting together the outlier type with the date of the outlier
                  this_xreg_names[i] <- paste0(this_reg[1], "(week ", this_week[as.numeric(this_reg[2])], 
                    ", ", this_year[as.numeric(this_reg[2])], ")")
                }
                # collect parameter names into \code{this_names}
                this_names <- c(this_xreg_names, "theta_nonseas", "theta_seasonal")
            }
        } else {
            # collect parameter names into \code{this_names}
            this_names <- c(xreg_names, "theta_nonseas", "theta_seasonal")
        }
    }
    
    # Initialize vector for standard errors to zero
    this_se <- array(0, dim = length(this_b))
    
    # set \code{this_se} to estimate of standard errors from covariance matrices of the regression
    # and ARMA estimates
    for (i in 1:length(this_est$model$b)) {
        this_se[i] <- sqrt(this_est$model$bcov[i, i])
    }
    this_se[length(this_b) - 1] <- sqrt(this_est$estimation$covariance[1, 1])
    this_se[length(this_b)] <- sqrt(this_est$estimation$covariance[2, 2])
    
    # Create model matrix by binding vectors for model parameters, standard errors, and t-statistics.
    # Also set row and column names of the matrix
    this_model_matrix <- cbind(this_b, this_se, this_t)
    rownames(this_model_matrix) <- this_names
    colnames(this_model_matrix) <- c("Coefficient", "Std_Err", "T-value")
    
    # return model matrix
    return(this_model_matrix)
}
