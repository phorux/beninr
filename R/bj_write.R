#' Write data to file
#'
#' Writes tabular or spatial data to disk.
#'
#' @param data An R object to export. Supported types:
#'   - **Spatial**: `sf`
#'   - **Tabular**: `data.frame`, `tibble`, or any object coercible to one
#' @param file_path A character string giving the output file path including
#'   extension (e.g. `"output/results.csv"`). If no extension is provided, a
#'   default is inferred: `.gpkg` for `sf` objects, `.csv` otherwise.
#' @param quiet Logical. If `TRUE`, suppresses all console messages.
#'   Defaults to `FALSE`.
#' @param ... Additional arguments passed to the underlying write function
#'   (e.g. `readr::write_csv()`, `sf::write_sf()`).
#'
#' @return The input `data`, invisibly.
#'
#' @examples
#' # CSV
#' bj_write(RGPH4[1:5, ], "output/RGPH4.csv", quiet = TRUE)
#' unlink("output/RGPH4.csv")
#'
#' # Spatial
#' bj_write(agroeco_zones, "output/agroeco_zones.gpkg", quiet = TRUE)
#' unlink("output/agroeco_zones.gpkg")
#'
#'@importFrom rlang check_installed
#'@import cli
#'
#' @export
bj_write <- function(data, file_path, quiet = FALSE, ...) {

  fe <- tolower(file_ext(file_path))
  if (!dir.exists(dirname(file_path))) dir.create(dirname(file_path))

  # -- Infer extension when missing
  if (nchar(fe) == 0) {
    if (inherits(data, "sf")) {
      file_path <- paste0(file_path, ".gpkg")
      fe <- "gpkg"
    } else {
      file_path <- paste0(file_path, ".csv")
      fe <- "csv"
    }
    if (!quiet)
      cli_alert_warning("No extension provided - defaulting to {.val {fe}}")
  }

  # --- Spatial (sf)
  sf_ext <- sf_drivers$extension

  if (inherits(data, "sf")) {

    if (!fe %in% sf_ext)
      cli_abort("Extension {.val {fe}} is not supported.")

    check_installed("sf", reason = "to write spatial files")

    if (!quiet) cli_progress_step("Writing to file")
    call_pkg_fun("sf", "write_sf", data, file_path,  quiet = TRUE,...)
    if (!quiet) cli_alert_success("File written to {.file {file_path}}")
    return(invisible(data))
  }

  # --- Tabular
  if (!quiet) cli_progress_step("Writing data to {.file {file_path}}")

  switch(fe,
         # --- Delimited
         csv = call_pkg_fun("readr", "write_csv", data, file_path, ...),
         tsv = call_pkg_fun("readr", "write_tsv", data, file_path, ...),
         # --- Excel
         xlsx = call_pkg_fun("openxlsx2", "write_xlsx", data, file_path, ...),
         # --- Native R
         rds = saveRDS(data, file_path, ...),
         rdata = ,
         rda = save(data, file = file_path, ...),
         # --- Parquet (columnar, best for large data)
         parquet = call_pkg_fun("arrow", "write_parquet", data, file_path, ...),
         # --- Feather (fast IPC format, Arrow-based)
         feather = call_pkg_fun("arrow", "write_feather", data, file_path, ...),
         # --- JSON
         json = call_pkg_fun("jsonlite", "write_json", data, file_path, ...),
         # --- YAML
         yaml = ,
         yml  = call_pkg_fun("yaml", "write_yaml", data, file_path, ...),

         # --- Fallback
         cli_abort(c(
           "Extension {.val {fe}} is not supported.",
           "i" = "Supported tabular formats: {.val {c('csv','tsv','xlsx','rds','rda','parquet','feather','json','yaml')}}"
         ))
  )

  if (!quiet) cli_alert_success("File written to {.file {file_path}} ({.val {format(file.size(file_path), big.mark = ',')}} bytes)")
  return(invisible(data))
}
