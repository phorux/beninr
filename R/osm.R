#' Download Benin OpenStreetMap Data from Geofabrik
#'
#' @description
#' Downloads the latest OpenStreetMap data for Benin from the
#' [Geofabrik download server](https://download.geofabrik.de/africa/benin.html).
#'
#' @param format `character`. File format to download. One of:
#'   * `"shp"` - ESRI Shapefile (`.shp.zip`). Extracts to multiple
#'     theme-based shapefiles (roads, buildings, waterways, etc.).
#'   * `"gpkg"` - GeoPackage (`.gpkg.zip`). Same content as
#'     Shapefile but in modern OGC GeoPackage format.
#'   * `"pbf"` - Protocolbuffer Binary Format (`.osm.pbf`). Compact
#'     format suitable for Osmium, Osmosis, osm2pgsql.
#' @param dest_dir `character`. Directory where downloaded files will be
#'   saved. Defaults to `"."` (current working directory). Created
#'   automatically if it does not exist.
#' @param layer `character`. For `"shp"` and `"gpkg"` formats, the name
#'   of the layer to read after extraction (e.g. `"gis_osm_roads_free_1"`).
#'   If `NULL` (default), the archive is extracted but no [sf][sf::sf] object
#'   is returned. Use `bj_get_osm(format, dest_dir, list_layers = TRUE)` to
#'   see all available layers.
#' @param list_layers `logical`. If `TRUE`, lists all available layers in
#'   the extracted archive and returns them as a character vector without
#'   reading any layer. Defaults to `FALSE`.
#' @param overwrite `logical`. If `TRUE`, re-downloads the file even if it
#'   already exists locally. Defaults to `FALSE`.
#'
#' @return
#' * If `list_layers = TRUE`: a `character` vector of available layer names.
#' * If `layer = NULL`: invisibly returns the path to the downloaded or
#'   extracted file(s).
#' * If `layer` is specified: an [sf][sf::sf] data frame of the requested
#'   layer.
#'
#' @seealso
#' * [bj_get_arcgis_layer()] for ArcGIS Online data
#'
#' @examples
#' \dontrun{
#' # Download shapefile archive (extract only)
#' bj_get_osm(format = "shp", dest_dir = "data/osm")
#'
#' # List available layers after download
#' bj_get_osm(format = "shp", dest_dir = "data/osm", list_layers = TRUE)
#'
#' # Download and read a specific layer
#' roads <- bj_get_osm(
#'   format   = "shp",
#'   dest_dir = "data/osm",
#'   layer    = "gis_osm_roads_free_1"
#' )
#'
#' # GeoPackage format
#' poi <- bj_get_osm(
#'   format   = "gpkg",
#'   dest_dir = "data/osm",
#'   layer    = "gis_osm_pois_free_1"
#' )
#'
#' # PBF format (use with osmextract)
#' pbf_path <- bj_get_osm(format = "pbf", dest_dir = "data/osm")
#' roads <- osmextract::oe_read(pbf_path, layer = "lines")
#'
#' # Force re-download
#' bj_get_osm(format = "shp", dest_dir = "data/osm", overwrite = TRUE)
#' }
#'
#' @importFrom httr2 request req_perform resp_body_raw
#' @importFrom rlang arg_match
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_alert_danger cli_progress_step cli_progress_done cli_ul
#'
#' @export
bj_get_osm <- function(format = c("shp", "gpkg", "pbf"),
                       dest_dir = ".",
                       layer = NULL,
                       list_layers = FALSE,
                       overwrite = FALSE) {

  format <- arg_match(format)

  cli::cli_h1("OpenStreetMap Data Download - Geofabrik")

  # Build URL
  base_url <- "https://download.geofabrik.de/africa"

  url <- switch(format,
                pbf = file.path(base_url, "benin-latest.osm.pbf"),
                shp = file.path(base_url, "benin-latest-free.shp.zip"),
                gpkg = file.path(base_url, "benin-latest-free.gpkg.zip")
  )


  # Prepare destination
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    cli::cli_alert_info("Created directory: {.path {dest_dir}}")
  }

  filename  <- basename(url)
  dest_file <- file.path(dest_dir, filename)

  # Check cache
  if (file.exists(dest_file) && !overwrite) {
    cli::cli_alert_warning(c(
      "File already exists: {.path {dest_file}} ",
      "i" = "Use {.code overwrite = TRUE} to re-download."
    ))
  } else {
    # Download
    cli::cli_progress_step("Downloading {.file {filename}} ...")

    tryCatch({
      httr2::request(url) %>%
        httr2::req_progress() %>%
        httr2::req_perform() %>%
        httr2::resp_body_raw() %>%
        writeBin(dest_file)

      size_mb <- round(file.size(dest_file) / 1024^2, 1)
      cli::cli_progress_done()
      cli::cli_alert_success(
        "Downloaded {.file {filename}} ({.val {size_mb}} MB) -> {.path {dest_dir}}"
      )
    },
    error = function(e) {
      cli::cli_progress_done(result = "failed")
      cli::cli_alert_danger("Download failed: {conditionMessage(e)}")
      stop(e)
    })
  }

  # Extract zip
  extract_dir <- NULL

  if (format %in% c("shp", "gpkg")) {
    extract_dir <- file.path(dest_dir, sub("\\.zip$", "", filename))

    if (!dir.exists(extract_dir)) {
      cli::cli_progress_step("Extracting archive ...")
      tryCatch({
        unzip(dest_file, exdir = extract_dir)
        cli::cli_progress_done()
        cli::cli_alert_success("Extracted to: {.path {extract_dir}}")
      },
      error = function(e) {
        cli::cli_progress_done(result = "failed")
        cli::cli_alert_danger("Extraction failed: {conditionMessage(e)}")
        stop(e)
      })
    } else {
      cli::cli_alert_warning("Already extracted: {.path {extract_dir}}")
    }
  }

  # List layers
  if (list_layers) {

    if (format == "pbf") {
      cli::cli_alert_warning(c(
        "{.val pbf} format does not support layer listing"
      ))
      return(invisible(dest_file))
    }

    ext <- if (format == "shp") "\\.shp$" else "\\.gpkg$"
    files <- list.files(extract_dir, pattern = ext,
                        recursive = TRUE, full.names = FALSE)

    remove_ext <- function(x, format){
      x <- gsub(sprintf(".%s$", format), "", x)
      return(x)
    }
    layers <- remove_ext(basename(files), format)

    cli::cli_alert_success("Found {.val {length(layers)}} layer{?s}:")
    cli::cli_ul(layers)

    return(invisible(layers))
  }

  # Read layer (optional)
  if (!is.null(layer)) {

    if (format == "pbf") {
      cli::cli_alert_warning(c(
        "Layer reading is not supported for {.val pbf} format. ",
        "i" = "Try: {.code osmextract::oe_read('{dest_file}', layer = '{layer}')}"
      ))
      return(invisible(dest_file))
    }

    ext <- if (format == "shp") ".shp" else ".gpkg"
    layer_file <- list.files(extract_dir,
                             pattern    = paste0(layer, ext, "$"),
                             full.names = TRUE,
                             recursive  = TRUE)

    if (length(layer_file) == 0) {
      cli::cli_alert_danger(c(
        "Layer {.val {layer}} not found. ",
        "i" = "Use {.code bj_get_osm('{format}', '{dest_dir}', list_layers = TRUE)} \\
               to see available layers."
      ))
      stop("Layer not found.")
    }

    cli::cli_progress_step("Reading layer {.val {layer}} ...")
    result <- tryCatch(
      sf::st_read(layer_file, quiet = TRUE),
      error = function(e) {
        cli::cli_progress_done(result = "failed")
        cli::cli_alert_danger("Failed to read layer: {conditionMessage(e)}")
        stop(e)
      }
    )
    cli::cli_progress_done()
    cli::cli_alert_success(
      "Loaded {.val {nrow(result)}} feature{?s} with \\
      {.val {ncol(result) - 1}} attribute column{?s}."
    )

    return(result)
  }

  invisible(if (!is.null(extract_dir)) extract_dir else dest_file)
}
