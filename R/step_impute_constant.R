#' Impute a constant value to correct for missingness
#' @param recipe
#' @param ...
#' @param role 
#' @param trained
#' @param constant A numeric value or a possibly named vector with same length
#' as the result from the selector functions, specifying which constant should
#' be imputed for each selector
#' @param id
#' @author Bijaelo
#' @description This method can be used for constant imputation.
#' @note taken from https://github.com/tidymodels/recipes/issues/473, edited by
#' me to make it work.
#' @export
step_impute_constant <- function(
    recipe, 
    ..., 
    role = NA, 
    trained = FALSE,
    constant = 0,
    skip = FALSE,
    id = rand_id("impute_constant")){
  # Import terms
  terms <- ellipse_check(...) 
  # Check that "constant" is the correct format (first vector, then numeric)
  if(is.list(constant)){
    if(any(lengths(constant) != 1))
      rlang::abort('One or more elements of `constant` has a length different from 1.\n`constant` should be a single numeric vector or a possibly named vector or list with the same length as the selector function.')
    constant <- unlist(constant, recursive = TRUE, use.names = TRUE)
  }
  # After conversion from list, the constant should still be a single numeric value
  if(!is.numeric(constant))
    rlang::abort('`constant` should be a single numeric vector or a possibly named vector or list with the same length as the selector function.') 
  add_step(
    recipe, 
    step(
      subclass = "impute_constant", 
      terms = terms,
      trained = trained,
      role = role,
      constant = constant,
      skip = skip,
      id = id
    )
  )
}
prep.step_impute_constant <- function(x, training, info = NULL, ...){
  # Import the names that should be transformed.
  col_names <- terms_select(terms = x$terms, info = info) 
  # Make sure all types are numeric
  recipes::check_type(training[, col_names], quant = TRUE)
  if(!is.null(nm <- names(x$constant))){
    if(any(!nm %in% col_names))
      rlang::abort('`constant` has more elements then specified by the selector functions.')
    if(any(!col_names %in% nm))
      rlang::abort('One or more columns are missing from named `constant` vector.')
  }else{
    if((n <- length(x$constant)) != 1 && n != length(col_names))
      rlang::abort('`constant` should be a single numeric vector or a possibly named vector or list with the same length as the selector function.') 
    else if(n == 1)
      x$constant <- rep(x$constant, length(col_names))
    names(x$constant) <- col_names
  }
  step(
    subclass = "impute_constant", 
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    constant = x$constant,
    skip = x$skip,
    id = x$id
  )
}
bake.step_impute_constant <- function(object, new_data, ...){
  # Import the variables that should be baked
  vars <- names(object$constant)
  # Iterate over each variable and impute the constant. 
  for(i in vars){
    isn <- is.na(new_data[[i]])
    new_data[[i]][isn] <- object$constant[[i]]
  }
  # Return the result as a tibble.
  tibble::as_tibble(new_data)
}

print.step_impute_constant <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Replacing NAs with given constant in", sep = "")
    printer(
      
      untr_obj = x$terms,
      
      trained = x$trained,
      
      width = width
    )
    invisible(x)
  }