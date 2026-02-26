library(dplyr)
library(sf)

communes <- read_sf("E:\\benindata\\boundary\\adm_commune.shp") %>%
  st_simplify(dTolerance = 500, preserveTopology = TRUE) %>%
  rename(nom = Nom_COM, code = Code_COM, population = Population) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON") %>%
  mutate(population = as.numeric(population),
         perimetre_km = st_length(st_cast(., to = "MULTILINESTRING")),
         perimetre_km = as.numeric(perimetre_km)/1e3,
         superficie_km2 = as.numeric(st_area(.))/1e6) %>%
  select(nom, code, population, perimetre_km, superficie_km2)

usethis::use_data(communes, overwrite = TRUE)
