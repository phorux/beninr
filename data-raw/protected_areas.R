original_path <- "D:/QGIS Projects/Shp/Protected_Area_Benin/ADM_AIRE_PROTEGEE_SURFACIQUE.shp"
ranchs <- sf::read_sf("D:/QGIS Projects/Projects/EA/ranch_savalou_djija_zogbodomey.shp") %>%
  mutate(type = "Ranch", nom = layer)

library(dplyr)
benin_pa <- sf::read_sf(original_path) %>%
  rename(type = 1, nom = 2) %>%
  sf::st_transform(crs = sf::st_crs(ranchs)) %>%
  sf::st_zm(what = "ZM") %>%
  bind_rows(ranchs) %>%
  mutate(type = stringr::str_to_title(sub("_", " ", type)),
         type = sub("Classee", "Classée", type),
         type = sub("Cynegetique", "Cynégétique", type)) %>%
  dplyr::select(-c(3:9), -layer) %>%
  mutate(superficie_km2 = round(as.numeric(sf::st_area(.))*1e-6, 3),
         superficie_ha = round(as.numeric(sf::st_area(.))*1e-4, 3),
         perimetre_km = round(as.numeric(sf::st_length(sf::st_boundary(.)))*1e-3, 3))

glimpse(pa)
usethis::use_data(benin_pa, overwrite = TRUE)
