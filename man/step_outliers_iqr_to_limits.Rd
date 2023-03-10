% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_outliers_iqr__to_limits.R
\name{step_outliers_iqr_to_limits}
\alias{step_outliers_iqr_to_limits}
\title{Identify values outside the IQR and replace them with IQR limits}
\usage{
step_outliers_iqr_to_limits(
  recipe,
  ...,
  role = NA,
  skip = TRUE,
  trained = FALSE,
  columns = NULL,
  id = recipes::rand_id("outliers_iqr_to_limits")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose variables for this step. See selections() for more details.}

\item{role}{Unused, include for consistency with other steps.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = FALSE.}

\item{trained}{A logical to indicate if the quantities for preprocessing have been estimated. Again included for consistency.}

\item{id}{A character string that is unique to this step to identify it.}
}
\description{
This method can be used in order to identify outliers by using the interquartile range rule and replace them with min/max values of its range.
}
\note{
I think it's quite cool.
}
\author{
steffanossa
}
