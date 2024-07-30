# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("echarts4r")
modules::import("plotly")
modules::import("shiny")
modules::import("stringr")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("summUI")
modules::export("summServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("utilities/data-select.R")


#' Title
#'
#' @param id
#'
#' @return Module for showing the app summary: value boxes and sources x-ray.
#' @export
#'
#' @examples
#'
