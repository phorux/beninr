#' Communes of Benin
#'
#' A spatial dataset containing the administrative boundaries of municipalities in
#' Benin.
#'
#' @format A \code{sf} object with \code{POLYGON} geometry and the following variables:
#' \describe{
#'   \item{nom}{Name of the commune (character).}
#'   \item{code}{Administrative code of the commune (character).}
#'   \item{population}{Population count of the commune (numeric).}
#'   \item{perimetre_km}{Perimeter of the commune boundary, in kilometres (numeric).}
#'   \item{superficie_km2}{Area of the commune, in square kilometres (numeric).}
#'   \item{geometry}{Polygon geometry of the commune boundary (\code{sfc_POLYGON}).}
#' }
#' @examples
#' if (requireNamespace("sf")) {
#' library(sf)
#'
#' # Load the dataset
#' data(communes)
#'
#' # Inspect the first rows
#' head(communes)
#'}
#' @section Language: French
#'
#' @references IGN (2022).\emph{Limites administratives du Bénin}.
"communes"
