#' Identify values outside the IQR and replace them with IQR limits
#' @author steffanossa
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step. See selections() for more details.
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. Again included for consistency.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = FALSE.
#' @param id A character string that is unique to this step to identify it.
#' @description This method can be used in order to identify outliers by using the interquartile range rule and replace them with min/max values of its range.
#' @note I think it's quite cool.
#' @export
#' @importFrom recipes prep bake rand_id
step_outliers_iqr_to_limits <-
  function(recipe,
           ...,
           role    = NA,
           skip    = TRUE,
           trained = FALSE,
           columns = NULL,
           id = recipes::rand_id("outliers_iqr_to_limits")) {

    terms <- recipes::ellipse_check(...)

    recipes::add_step(
      recipe,
      step_outliers_iqr_to_limits_new(
        terms   = terms,
        role    = role,
        skip    = skip,
        trained = trained,
        columns = columns,
        id = id
        )
    )
  }

### constructor
step_outliers_iqr_to_limits_new <-
  function(terms,
           role,
           skip,
           trained,
           columns,
           id) {
    recipes::step(
      subclass = "outliers_iqr_to_limits",
      terms    = terms,
      role     = role,
      skip     = skip,
      trained  = trained,
      columns  = columns,
      id = id
    )
  }

### actual function
outliers_to_limits <- function(x, args = NULL) {

  # get interquartile range
  lower_limit <- quantile(x, probs = 0.25, na.rm = TRUE)
  upper_limit <- quantile(x, probs = 0.75, na.rm = TRUE)
  iqr <- upper_limit - lower_limit

  # identify outliers
  lower_threshold <- lower_limit - 1.5 * iqr
  upper_threshold <- upper_limit + 1.5 * iqr

  # replace
  ifelse(is.na(x), NA,
         ifelse(x < lower_threshold, min(x[x >= lower_threshold & x <= upper_threshold], na.rm = TRUE),
                ifelse(x > upper_threshold, max(x[x >= lower_threshold & x <= upper_threshold], na.rm = TRUE),
                       x)))
}

#' @export
prep.step_outliers_iqr_to_limits <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info = info)

  recipes::check_type(training[, col_names], quant = TRUE)

  step_outliers_iqr_to_limits_new(
    terms   = x$terms,
    role    = x$role,
    skip    = x$skip,
    trained = TRUE,
    columns = col_names,
    id = x$id
  )
}

#' @export
bake.step_outliers_iqr_to_limits <- function(object,
                                             new_data,
                                             ...) {
  for (i in object$columns) {
    new_data[[i]] <- new_data[[i]] %>% outliers_to_limits()
  }
  tibble::as_tibble(new_data)
}

### print
print.step_outliers_iqr_to_limits <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Replacing values outide the IQR with min/max values in ", sep = "")
    recipes::printer(

      untr_obj = x$terms,

      trained = x$trained,

      width = width
    )
    invisible(x)
  }
