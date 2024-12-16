#' Get the response from an individual model
#'
#' @description
#' A view of variable responses in fitted models. Responses based on single or multiple
#' models can be provided.
#'
#' @usage response_curve(models, variable, modelID = NULL, n = 100,
#'                      by_replicates = FALSE, data = NULL, new_data = NULL,
#'                      averages_from = "pr",
#'                      extrapolate = TRUE, extrapolation_factor = 0.1,
#'                      l_limit = NULL, u_limit = NULL,
#'                      xlab = NULL, ylab = "Suitability",
#'                      col = "darkblue", ...)
#
#' @param models an object of class `fitted_models` returned by the
#' \code{\link{fit_selected}}() function.
#' @param variable (character) name of the variables to be plotted.
#' @param data data.frame or matrix of data used in the model calibration step.
#' Default = NULL.
#' @param modelID (character) vector of ModelID(s) to be considered in the
#' models object. By default all models are included.Default = NULL.
#' @param n (numeric) an integer guiding the number of breaks. Default = 100
#' @param by_replicates (logical) whether use replicates or full_model to
#' estimate the model's response curve. Default = FALSE.
#' @param new_data a `SpatRaster`, data.frame, or  matrix of variables
#' representing the range of variable values in an area of interest.
#' Default = NULL. It must be defined in case the model entered does not
#' explicitly include a data component.
#' @param averages_from (character) specifies how the averages or modes of the
#' variables are calculated. Available options are "pr" (to calculate averages
#' from the presence localities) or "pr_bg" (to use the combined set of presence
#' and background localities). Default is "pr". See details.
#' @param extrapolate (logical) whether to allow extrapolation to study the
#' behavior of the response outside the calibration limits. Ignored if
#' `new_data` is defined. Default = TRUE.
#' @param extrapolation_factor (numeric) a multiplier used to calculate the
#' extrapolation range. Larger values allow broader extrapolation beyond the
#' observed data range. Default is 0.1.
#' @param l_limit (numeric) specifies the lower limit for the variable. Default
#' is \code{NULL}, meaning the lower limit will be calculated based on the
#' data's minimum value and the \code{extrapolation_factor}
#' (if \code{extrapolation = TRUE}).
#' @param u_limit (numeric) specifies the upper limit for the variable. Default
#' is \code{NULL}, meaning the upper limit will be calculated based on the
#' data's minimum value and the \code{extrapolation_factor}
#' (if \code{extrapolation = TRUE}).
#' @param xlab (character) a label for the x axis. The default, NULL, uses the
#' name defined in `variable`.
#' @param ylab (character) a label for the y axis. Default = "Suitability".
#' @param col (character) color for lines. Default = "darkblue".
#' @param ... additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' The response curves are generated with all other variables set to their mean
#' values (or mode for categorical variables), calculated either from the
#' presence localities (if averages_from = "pr") or from the combined set of
#' presence and background localities (if averages_from = "pr_bg").
#'
response <- function(model, data, variable, type = "cloglog", n = 100,
                     new_data = NULL, extrapolate = FALSE,
                     extrapolation_factor = 0.11,
                     categorical_variables = NULL,
                     averages_from = "pr_bg",
                     l_limit = NULL, u_limit = NULL) {

  # initial tests
  if (missing(model) | missing(variable) | missing(data)) {
    stop("Arguments 'model', 'data', and 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  # Extract variable names used in the model
  vnames <- if (inherits(model, "glmnet")) {
    colSums(sapply(colnames(data), grepl, names(model$betas))) > 0
  } else if (inherits(model, "glm")) {
    colSums(sapply(colnames(data), grepl, names(coef(model)))) > 0
  }

  if (any(!variable %in% names(vnames))) {
    stop("The name of the 'variable' was not defined correctly.")
  }

  # Extract calibration data from the model object
  if(averages_from == "pr"){
  cal_data <- data[, vnames, drop = FALSE] } else if (averages_from == "pr_bg"){
    cal_data <- data[data$pr_bg == 1, vnames, drop = FALSE]
  }

  # Extract the limits of the calibration data
  cal_maxs <-  apply(cal_data, 2, FUN = max)
  cal_mins <-  apply(cal_data, 2, FUN = min)

  # Get the average of all variables

  ####Check - For deal with categorical variables####
  means <- colMeans(cal_data[sapply(cal_data, is.numeric)])
  if(!is.null(categorical_variables) && vnames[categorical_variables]){
    mode_cat <- sapply(categorical_variables, function(x){
      as.numeric(names(which.max(table(cal_data[, x]))))
    })
    means <- c(means, mode_cat)
  }
  #########################################

  if (is.null(new_data)) {
    if (extrapolate) {

      rr <- range(cal_data[, variable]) # range of the calibration data
      extension <- extrapolation_factor * diff(rr)

      if(is.null(l_limit))
      l_limit <- rr[1] - extension
      if(is.null(u_limit))
      u_limit <- rr[2] + extension

      rangev <- c(l_limit, u_limit)

    } else {
      rangev <- c(cal_mins[variable], cal_maxs[variable])
    }

  } else {

    if (class(new_data)[1] == "SpatRaster") {
      rangev <- terra::minmax(new_data[[variable]])
    } else {
      rangev <- range(new_data[, variable])
    }

  }

  newvar <- seq(rangev[1], rangev[2], length = n)

  m <- data.frame(matrix(means, n, length(means), byrow = T))
  colnames(m) <- names(means)

  m[, variable] <- newvar

  # Predict using glmnet or glm
  m$predicted <- if (inherits(model, "glmnet")) {
    custom_predict.glmnet_mx(model, m, type = type)
  } else if (inherits(model, "glm")) {
    predict.glm(model, newdata = m, type = "response")
  }

  return(m[,c(variable, "predicted")])

}
