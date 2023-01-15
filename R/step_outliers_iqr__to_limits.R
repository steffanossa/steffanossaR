#' Identify values outside the IQR and replace them with IQR limits
#' @author steffanossa
#' @description This method can be used in order to identify outliers by using
#' the interquartile range rule and replace them with min/max values of its
#' range.
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

### prep
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

### bake
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
