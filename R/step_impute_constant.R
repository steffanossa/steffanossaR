#' Impute a constant value to correct for missingness
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step. See selections() for more details.
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. Again included for consistency.
#' @param constant A numeric value or a possibly named vector with same length as the result from the selector functions, specifying which constant should be imputed for each selector
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = FALSE.
#' @param id A character string that is unique to this step to identify it.
#' @author Bijaelo
#' @description This method can be used for constant imputation.
#' @note taken from https://github.com/tidymodels/recipes/issues/473, edited by me to make it work.
#' @export
#' @importFrom recipes prep bake rand_id
step_impute_constant <- function(
    recipe, 
    ..., 
    role = NA, 
    trained = FALSE,
    constant = 0,
    skip = FALSE,
    id = recipes::rand_id("impute_constant")){
  # Import terms
  terms <- recipes::ellipse_check(...) 
  # Check that "constant" is the correct format (first vector, then numeric)
  if(is.list(constant)){
    if(any(lengths(constant) != 1))
      rlang::abort('One or more elements of `constant` has a length different from 1.\n`constant` should be a single numeric vector or a possibly named vector or list with the same length as the selector function.')
    constant <- unlist(constant, recursive = TRUE, use.names = TRUE)
  }
  # After conversion from list, the constant should still be a single numeric value
  if(!is.numeric(constant))
    rlang::abort('`constant` should be a single numeric vector or a possibly named vector or list with the same length as the selector function.') 
  recipes::add_step(
    recipe, 
    recipes::step(
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
#' @export
prep.step_impute_constant <- function(x, training, info = NULL, ...){
  # Import the names that should be transformed.
  col_names <- recipes::recipes_eval_select(terms = x$terms, info = info)
  # col_names <- recipes::terms_select(terms = x$terms, info = info)
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
  recipes::step(
    subclass = "impute_constant", 
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    constant = x$constant,
    skip = x$skip,
    id = x$id
  )
}
#' @export
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
    recipes::printer(
      
      untr_obj = x$terms,
      
      trained = x$trained,
      
      width = width
    )
    invisible(x)
  }