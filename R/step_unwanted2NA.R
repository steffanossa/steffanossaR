#' Turn unwanted values into NAs
#' @author steffanossa
#' @param values Single value or vector containing values wanted in columns
#' set.
#' @param wanted Boolean. Turns everything not in *values* into NAs if *TRUE*.
#'                         Turn everything in *values* into NAs if *FALSE*.
#' @description This method can be used in order to cleanse columns from values
#' that are unwanted.
#' @note I think it's quite cool.
#' @note IMPORTANT: use prep(strings_as_factors = FALSE) when dealing with chr
#' columns! Otherwise you will end up with chr columns turned to int!
#' @export
step_unwanted2NA <-
  function(recipe,
           ...,
           role    = NA,
           skip    = TRUE,
           values  = c(0,1),
           wanted = TRUE,
           trained = FALSE,
           columns = NULL,
           id = rand_id("unwanted2NA")) {

    terms <- ellipse_check(...)

    add_step(
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
    step(
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



### prep
prep.step_unwanted2NA <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info = info)

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

### bake
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
    printer(

      untr_obj = x$terms,

      trained = x$trained,

      width = width
    )
    invisible(x)
  }
