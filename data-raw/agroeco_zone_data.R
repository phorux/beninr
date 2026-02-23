library(sf)
library(dplyr)

agroeco_zones <- read_sf("C:/Users/ganda/Downloads/Benin ZA/benin_za.shp") %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = "EPSG:32631") %>% # FOr accurate mesure calculation
  mutate(`AREA KM2` = round(as.numeric(st_area(.))/1e6, 2),
         `PERIMETER KM` = as.numeric(
           st_length(st_cast(x = ., to = "MULTILINESTRING")))/1e3,
         `PERIMETER KM` = round(`PERIMETER KM`, 2)
         ) %>%
  relocate(geometry, .after = `PERIMETER KM`)

# I realized EPSG:32631 cause ASCII warning during the package check.
# So transform back to geographic system.
# agroeco_zones <- agroeco_zones %>%
#   st_transform(crs = 4326)

#Encoding(sf::st_crs(agroeco_zones)$wkt) <- "UTF-8"
usethis::use_data(agroeco_zones, overwrite = TRUE)
