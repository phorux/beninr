## code to prepare `sf_drivers` dataset goes here
## sf_drivers is used internally to validate driver/format names supplied
## by the user. Not exported.

sf_drivers <- sf::st_drivers() %>%
  dplyr::filter(write == TRUE)
sf_drivers$extension <- c("pix", "nc", "xml", "vic/img", "pdf", "mbtiles", "bag",
                    "shp", "tab/mif/mid", "000", "dgn", "", "csv", "gml",
                    "gpx", "kml", "geojson/json", "geojsons/jsonl", "gmt",
                    "gpkg", "sqlite/db", "map", "", "", "", "gdb", "dxf",
                    "fgb", "gxt/txt", "xml", "sql", "", "ods", "xlsx", "",
                    "", "", "slf", "jml", "xml", "mvt/pbf", "", "mapml",
                    "pmtiles", "json")

sf_drivers <- sf_drivers %>%
  filter(extension != "") %>%
  tidyr::separate_longer_delim(cols = extension, delim = "/") %>%
  select(name, long_name, extension)
usethis::use_data(sf_drivers, overwrite = TRUE, internal = TRUE)
