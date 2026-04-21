#' Protected Areas in Benin
#'
#' A spatial dataset of protected areas in Benin, including
#' national parks, classified forests, hunting zones, and community ranches.
#' The dataset combines official protected area boundaries and provides standardized
#' attributes and geometric metrics.
#'
#' @format An `sf` object:
#' \describe{
#'   \item{type}{Character. Type of protected area (e.g., "Parc National",
#'   "Forêt Classée", "Zone Cynégétique", "Ranch").}
#'   \item{nom}{Character. Name of the protected area or ranch.}
#'   \item{geometry}{MULTIPOLYGON. Spatial geometry of each feature in a projected CRS (meters).}
#'   \item{superficie_km2}{Numeric. Area of the feature in square kilometers.}
#'   \item{superficie_ha}{Numeric. Area of the feature in hectares.}
#'   \item{perimetre_km}{Numeric. Perimeter of the feature in kilometers.}
#' }
#'
#' @author Stanislas Mahussi Gandaho
#'
#' @section Language: French
#' @section Keyword: Protected areas, Biodiversity, Land management, Conservation
#'
#' @examples
#' \dontrun{
#' library(sf)
#' data(benin_pa)
#'
#' # Plot protected areas
#' plot(pa["type"])
#'
#' # Summary of area by type
#' aggregate(superficie_km2 ~ type, data = benin_pa, sum)
#' }
#'
#' @name benin_pa
#' @keywords datasets
"benin_pa"
