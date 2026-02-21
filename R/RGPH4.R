#' RGPH4 2013 Village-Level Population Data for Benin
#'
#' Village-level population and household data from the Fourth General
#' Population and Housing Census (RGPH4) conducted in 2013 in Benin.
#'
#' The dataset contains demographic indicators aggregated at the village level,
#' including total population, number of households, and gender distribution.
#'
#' @format A tibble with the following variables:
#'   - `Departements`: Character. Department name
#'   - `Communes`: Character. Commune name
#'   - `Arrondissements`: Character. Arrondissement name
#'   - `Villages`: Character. Village name
#'   - `Nombre ménages`: Numeric. Number of households
#'   - `Total`: Numeric. Total population
#'   - `Masculin`: Numeric. Male population
#'   - `Féminin`: Numeric. Female population
#'   - `Taille ménage`: Numeric. Average household size
#'
#' @details
#' Data were extracted and cleaned from official census tables.
#' Administrative hierarchy follows the structure:
#' Department > Commune > Arrondissement > Village.
#'
#' @source
#' Institut National de la Statistique et de l'Analyse Économique ([INSAE](https://rgph5.instad.bj/rgph4-2013/)),
#' Recensement Général de la Population et de l'Habitation (RGPH4), 2013.
#'
#' @references
#' INSAE (2013). RGPH4 Final Report.
#'
#' @examples
#' head(RGPH4)
#'
#' @keywords datasets
"RGPH4"
