#' Download a Layer from ArcGIS Online
#'
#' Retrieves all features from a public ArcGIS Online Feature Service layer.
#'
#' @param item_id \code{character(1)}. The ArcGIS Online item ID, found in the
#'   item page URL after \code{?id=}. Example:
#'   \code{"f1e94d03dd7d44229af3891ad6ce8dfe"}.
#' @param layer_index \code{integer(1)}. Zero-based index of the layer within
#'   the Feature Service. Defaults to \code{0} (first layer). Inspect the
#'   \code{/FeatureServer} endpoint to discover available layer indices.
#' @param where \code{character(1)}. A SQL \code{WHERE} clause used to filter
#'   features. Defaults to \code{"1=1"} (all features).
#' @param out_fields \code{character(1)}. Comma-separated list of field names
#'   to return. Defaults to \code{"*"} (all fields).
#' @param chunk_size \code{integer(1)}. Number of features to request per page.
#'   Defaults to \code{1000}. Reduce if the server returns timeout errors.
#'
#' @return An \code{sf} data frame containing all downloaded features, with
#'   geometry and attribute columns as returned by the service.
#'
#' @details
#' This function requires a public (unauthenticated) item. For private items,
#' you must append a valid \code{token} to the query parameters before calling
#' the REST API.
#'
#' @note
#' ArcGIS Online servers typically cap responses at 1,000-2,000 records per
#' request. The \code{chunk_size} argument should not exceed the server's
#' \code{maxRecordCount}, which can be found in the layer's JSON metadata at
#' \code{/FeatureServer/<layer_index>?f=json}.
#'
#' @seealso
#' Wrapper functions for specific Benin datasets:
#' * [bj_get_country()] - National boundary
#' * [bj_get_departements()] - Departments (12)
#' * [bj_get_arrondissements()] - Arrondissements (546)
#' * [bj_get_villages()] - Villages and localities
#' * [bj_get_zones_sanitaires()] - Health zones
#' * \href{https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer/}{ArcGIS REST API - Query (Feature Service/Layer)}
#'
#' @examples
#' \dontrun{
#' # Download all features from the first layer
#' layer_sf <- bj_get_arcgis_layer(
#'   item_id = "f1e94d03dd7d44229af3891ad6ce8dfe"
#' )
#'
#' # Inspect result
#' print(layer_sf)
#' sf::st_crs(layer_sf)
#'
#' # Save to GeoPackage
#' sf::st_write(layer_sf, "output_layer.gpkg")
#' }
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json resp_body_string
#' @importFrom dplyr bind_rows
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#'
#' @export
bj_get_arcgis_layer <- function(item_id,
                                  layer_index = 0,
                                  where       = "1=1",
                                  out_fields  = "*",
                                  chunk_size  = 1000) {

  cli::cli_h1("ArcGIS Layer Download")

  # Resolve item metadata -> service URL
  cli::cli_alert_info("Fetching item metadata for ID: {.val {item_id}}")

  item_url <- paste0(
    "https://www.arcgis.com/sharing/rest/content/items/",
    item_id, "?f=json"
  )

  meta <- tryCatch(
    httr2::request(item_url) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) {
      cli::cli_alert_danger("Failed to fetch item metadata: {conditionMessage(e)}")
      stop(e)
    }
  )

  service_url <- meta$url
  query_url   <- paste0(service_url, "/", layer_index, "/query")

  cli::cli_alert_success("Service URL resolved: {.url {service_url}}")
  cli::cli_alert_info("Targeting layer index: {.val {layer_index}}")

  # Get total feature count
  cli::cli_alert_info("Fetching total feature count ...")

  count_resp <- tryCatch(
    httr2::request(query_url) |>
      httr2::req_url_query(
        where           = where,
        returnCountOnly = "true",
        f               = "json"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) {
      cli::cli_alert_danger("Failed to retrieve feature count: {conditionMessage(e)}")
      stop(e)
    }
  )

  total <- count_resp$count

  if (is.null(total) || total == 0) {
    cli::cli_alert_warning("Query returned {.val 0} features. Check your {.arg where} clause.")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }

  n_pages <- ceiling(total / chunk_size)
  cli::cli_alert_success(
    "Found {.val {total}} feature{?s} across {.val {n_pages}} page{?s} \\
    (chunk size: {.val {chunk_size}})"
  )

  # Paginate and download
  all_features <- vector("list", n_pages)
  offset       <- 0
  page         <- 1

  pb <- cli::cli_progress_bar(
    name   = "Downloading",
    total  = n_pages,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} pages | ETA: {cli::pb_eta}"
  )

  while (offset < total) {

    chunk_resp <- tryCatch(
      httr2::request(query_url) |>
        httr2::req_url_query(
          where             = where,
          outFields         = out_fields,
          resultOffset      = offset,
          resultRecordCount = chunk_size,
          f                 = "geojson"
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_string(),
      error = function(e) {
        cli::cli_progress_done(result = "failed")
        cli::cli_alert_danger(
          "Download failed at offset {.val {offset}}: {conditionMessage(e)}"
        )
        stop(e)
      }
    )

    all_features[[page]] <- sf::st_read(chunk_resp, quiet = TRUE)

    offset <- offset + chunk_size
    page   <- page + 1
    cli::cli_progress_update(id = pb)
  }

  cli::cli_progress_done(id = pb)

  # Combine chunks
  cli::cli_alert_info("Combining {.val {n_pages}} chunk{?s} into a single {.cls sf} object ...")
  result <- dplyr::bind_rows(all_features)

  cli::cli_alert_success(
    "Done! Downloaded {.val {nrow(result)}} feature{?s} with \\
    {.val {ncol(result) - 1}} attribute column{?s}."
  )

  result
}


# Wrapper helpers (internal)

#' Shared parameter documentation for `bj_get_*` wrapper functions
#'
#' @param where `character(1)`. A SQL `WHERE` clause to filter features.
#'   Defaults to `"1=1"` (all features). Example: `"nom_dept = 'ATACORA'"`.
#' @param out_fields `character(1)`. Comma-separated field names to return.
#'   Defaults to `"*"` (all fields).
#' @param chunk_size `integer(1)`. Number of features per paginated request.
#'   Defaults to `1000`.
#'
#' @name bj_shared_params
#' @keywords internal
NULL


# Wrapper functions

#' Get Benin National Boundary
#'
#' @description
#' Downloads the national boundary of Benin as an [sf][sf::sf] polygon object..
#'
# @return An [sf][sf::sf] data frame containing all downloaded features, with
#   geometry and attribute columns as returned by the ArcGIS service.
#   The coordinate reference system (CRS) is as defined by the source layer
#   (typically EPSG:4326).
#'
#' @seealso [bj_get_arcgis_layer()], [bj_get_departements()],
#'   [bj_get_arrondissements()], [bj_get_villages()],
#'   [bj_get_zones_sanitaires()]
#'
#' @examples
#' \dontrun{
#' benin <- bj_get_country()
#' plot(sf::st_geometry(benin))
#'
#' # Save to GeoPackage
#' sf::st_write(benin, "benin_boundary.gpkg")
#' }
#'
#' @export
bj_get_country <- function() {
  cli::cli_alert_info("Dataset: {.strong Benin National Boundary}")
  bj_get_arcgis_layer(
    item_id    = "f1e94d03dd7d44229af3891ad6ce8dfe",
    where      = "1=1",
    out_fields = "*",
    chunk_size = 1000
  )
}


#' Get Benin Departments
#'
#' @description
#' Downloads the administrative departments of Benin as an [sf][sf::sf]
#' polygon object.
#'
#' @inheritParams bj_shared_params
#'
# @return An [sf][sf::sf] data frame containing all downloaded features, with
#   geometry and attribute columns as returned by the ArcGIS service.
#   The coordinate reference system (CRS) is as defined by the source layer
#   (typically EPSG:4326).
#'
#' @seealso [bj_get_arcgis_layer()], [bj_get_country()],
#'   [bj_get_arrondissements()], [bj_get_villages()],
#'   [bj_get_zones_sanitaires()]
#'
#' @examples
#' \dontrun{
#' depts <- bj_get_departements()
#' print(depts)
#'
#' # Filter a single department
#' atacora <- bj_get_departements(where = "ADM1_NAME = 'Atacora'")
#'
#' # Plot
#' plot(sf::st_geometry(depts), main = "Benin - Departments")
#' }
#'
#' @export
bj_get_departements <- function(where = "1=1",
                                out_fields = "*",
                                chunk_size = 1000) {
  cli::cli_alert_info("Dataset: {.strong Benin Departments}")
  bj_get_arcgis_layer(
    item_id    = "ed051fda472e43e08de35596c7809fcd",
    where      = where,
    out_fields = out_fields,
    chunk_size = chunk_size
  )
}

#' Get Benin Arrondissements
#'
#' @description
#' Downloads all arrondissements of Benin as an [sf][sf::sf] polygon object.
#' Arrondissements are the 4th level of Benin's administrative hierarchy
#' (Country > Department > Commune > Arrondissement).
#'
#' @inheritParams bj_shared_params
#'
# @return An [sf][sf::sf] data frame containing all downloaded features, with
#   geometry and attribute columns as returned by the ArcGIS service.
#   The coordinate reference system (CRS) is as defined by the source layer
#   (typically EPSG:4326).
#'
#' @seealso [bj_get_arcgis_layer()], [bj_get_country()],
#'   [bj_get_departements()], [bj_get_villages()],
#'   [bj_get_zones_sanitaires()]
#'
#' @examples
#' \dontrun{
#' arrond <- bj_get_arrondissements()
#' print(arrond)
#'
#' # Return only names and geometry
#' arrond_names <- bj_get_arrondissements(out_fields = "BN_NIV3,BN_NIV4")
#' }
#'
#' @export
bj_get_arrondissements <- function(where = "1=1",
                                   out_fields = "*",
                                   chunk_size = 1000) {
  cli::cli_alert_info("Dataset: {.strong Benin Arrondissements}")
  bj_get_arcgis_layer(
    item_id    = "e695dd99818d4e088217c26c0a6b2683",
    where      = where,
    out_fields = out_fields,
    chunk_size = chunk_size
  )
}


#' Get Benin Villages and Localities
#'
#' @description
#' Downloads village and locality point data for Benin as an [sf][sf::sf]
#' object.
#'
#' @inheritParams bj_shared_params
#'
# @return An [sf][sf::sf] data frame containing all downloaded features, with
#   geometry and attribute columns as returned by the ArcGIS service.
#   The coordinate reference system (CRS) is as defined by the source layer
#   (typically EPSG:4326).
#'
#' @note
#' This dataset is large. Consider using the `where` argument to filter by
#' department or commune to reduce download time, or lower `chunk_size` if
#' you encounter server timeout errors.
#'
#' @seealso [bj_get_arcgis_layer()], [bj_get_country()],
#'   [bj_get_departements()], [bj_get_arrondissements()],
#'   [bj_get_zones_sanitaires()]
#'
#' @examples
#' \dontrun{
#' # Download all villages (may take a moment)
#' villages <- bj_get_villages()
#'
#' # Filter villages in DANGBO area
#' dangbo_villages <- bj_get_villages(where = "Commune = 'DANGBO'")
#'
#' # Get only name and coordinates
#' village_names <- bj_get_villages(out_fields = "Village")
#' }
#'
#' @export
bj_get_villages <- function(where = "1=1",
                            out_fields = "*",
                            chunk_size = 1000) {
  cli::cli_alert_info("Dataset: {.strong Benin Villages & Localities}")
  bj_get_arcgis_layer(
    item_id    = "d77eeec0d1ec402d8f384556cc277e64",
    where      = where,
    out_fields = out_fields,
    chunk_size = chunk_size
  )
}


#' Get Benin Health Zones (Zones Sanitaires)
#'
#' @description
#' Downloads the health zone (*zone sanitaire*) boundaries of Benin as an
#' [sf][sf::sf] polygon object. Health zones are the primary unit for public
#' health planning and surveillance in Benin.
#'
#' @inheritParams bj_shared_params
#'
# @return An [sf][sf::sf] data frame containing all downloaded features, with
#   geometry and attribute columns as returned by the ArcGIS service.
#   The coordinate reference system (CRS) is as defined by the source layer
#   (typically EPSG:4326).
#'
#' @seealso [bj_get_arcgis_layer()], [bj_get_country()],
#'   [bj_get_departements()], [bj_get_arrondissements()],
#'   [bj_get_villages()]
#'
#' @examples
#' \dontrun{
#' zones <- bj_get_zones_sanitaires()
#' print(zones)
#'
#' # Filter zones in a specific department
#' zones_donga <- bj_get_zones_sanitaires(where = "ADM1_NAME = 'Donga'")
#'
#' head(zones_donga)
#' }
#'
#' @export
bj_get_zones_sanitaires <- function(where = "1=1",
                                    out_fields = "*",
                                    chunk_size = 1000) {
  cli::cli_alert_info("Dataset: {.strong Benin Health Zones (Zones Sanitaires)}")
  bj_get_arcgis_layer(
    item_id    = "02755022147343c1b1f9f277d3d752ed",
    where      = where,
    out_fields = out_fields,
    chunk_size = chunk_size
  )
}
