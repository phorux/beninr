#' Agro-ecological Zones of Benin
#'
#' A spatial dataset containing the three agro-ecological zones of Benin,
#' with their respective areas and perimeters. The zones reflect the
#' country's climatic and vegetation gradients, ranging from the humid
#' Guinean zone in the south to the arid Sahelian zone in the north.
#'
#' @format A simple feature collection (`sf`) with 3 features and 4 fields,
#'   with geometry type `MULTIPOLYGON` and projected CRS WGS 84 / UTM zone 31N
#'   (EPSG:32631):
#'   \describe{
#'     \item{OBJECTID}{`integer`. Unique identifier for each zone.}
#'     \item{ZONES}{`character`. Name of the agro-ecological zone. One of
#'       `"Guinean"`, `"Sudano Guinean"`, or `"Sahelian"`.}
#'     \item{AREA KM2}{`double`. Area of the zone in square kilometres}
#'     \item{PERIMETER KM}{`double`. Perimeter of the zone in kilometres}
#'     \item{geometry}{`sfc_MULTIPOLYGON`. Spatial geometry column
#'       (coordinates in metres, UTM zone 31N).}
#'   }
#'
#' @source Stanislas Mahussi Gandaho (2026). *Agro-ecological Zones of Benin*
#'   \[Data set\]. Distributed as part of the \pkg{<beninr>} R package.
#'
#' @examples
#' data(agroeco_zones)
#'
#' # Transform to projected system
#' if (!requireNamespace("sf")) {
#'   agroeco_zones <- agroeco_zones %>%
#'     sf::st_transform(crs = "EPSG:32631")
#' }
#'
"agroeco_zones"
