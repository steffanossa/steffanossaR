#' Turn unwanted values into NAs
#' @author steffanossa
#' @param values Single value or vector containing values wanted in columns set.
#' @param wanted Boolean. Turns everything not in *values* into NAs if *TRUE*. Turn everything in *values* into NAs if *FALSE*.
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step. See selections() for more details.
#' @param role Unused, include for consistency with other steps.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated. Again included for consistency.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = FALSE.
#' @param id A character string that is unique to this step to identify it.
#' @description This method can be used in order to cleanse columns from values that are unwanted.
#' @note IMPORTANT: use prep(strings_as_factors = FALSE) when dealing with chr columns! Otherwise you will end up with chr columns turned to int!
#' @export
#' 
#' @importFrom recipes prep bake rand_id
step_unwanted2NA <-
  function(recipe,
           ...,
           role    = NA,
           skip    = TRUE,
           values  = c(0,1),
           wanted = TRUE,
           trained = FALSE,
           columns = NULL,
           id = recipes::rand_id("unwanted2NA")) {

    terms <- ellipse_check(...)

    recipes::add_step(
      recipe,
      step_unwanted2NA_new(
        terms   = terms,
        role    = role,
        skip    = skip,
        values  = values,
        wanted = wanted,
        trained = trained,
        columns = columns,
        id = id
      )
    )
  }

### constructor
step_unwanted2NA_new <-
  function(terms,
           role,
           skip,
           values,
           wanted,
           trained,
           columns,
           id) {
    recipes::step(
      subclass = "unwanted2NA",
      terms    = terms,
      role     = role,
      skip     = skip,
      values   = values,
      wanted  = wanted,
      trained  = trained,
      columns  = columns,
      id = id
    )
  }



#' @export
prep.step_unwanted2NA <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info = info)

  #check_type(training[, col_names], quant = TRUE)

  step_unwanted2NA_new(
    terms   = x$terms,
    role    = x$role,
    skip    = x$skip,
    values  = x$values,
    wanted = x$wanted,
    trained = TRUE,
    columns = col_names,
    id = x$id
  )
}

#' @export
bake.step_unwanted2NA <- function(object,
                                new_data,
                                ...) {
  if (object$wanted) {
    for (i in object$columns) {
      new_data[[i]] <- new_data[[i]] %>% ifelse(. %in% object$values, ., NA)
      }
    } else {
      for (i in object$columns) {
        new_data[[i]] <- new_data[[i]] %>% ifelse(. %in% object$values, NA, .)
        }
      }
  tibble::as_tibble(new_data)
}

### print
print.step_unwanted2NA <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Replacing unwanted values with NAs in ", sep = "")
    recipes::printer(

      untr_obj = x$terms,

      trained = x$trained,

      width = width
    )
    invisible(x)
  }
