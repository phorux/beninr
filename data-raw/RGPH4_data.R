## code to prepare `DATASET` dataset goes here

library(dplyr)

file_path <- "C:\\Users\\ganda\\Downloads\\Naf\\Cahier Village RGPH4 2013.xlsx"
sheet <- readxl::excel_sheets(file_path)

RGPH4 <- tibble::tibble()
for (sh in sheet) {
  try_read <- readxl::read_excel(file_path, sheet = sh)
  depin <- any(grepl("DEPARTEMENT", colnames(try_read)))
  if(depin){
    df <- readxl::read_excel(file_path, sheet = sh, skip = 3, col_names = FALSE)
  }else{
    df <- readxl::read_excel(file_path, sheet = sh, skip = 2, col_names = FALSE)
  }

  colnames(df) <- c("Departement", "Nombre ménages",	"Total",	"Masculin",	"Féminin",	"Taille ménage")

  RGPH4 <- dplyr::bind_rows(RGPH4, df)
}

## Perpare Departement, Municipality, District, and Village name

fill_na_with_previous <- function(x) {
  for(i in 1:length(x)){
    if(is.na(x[i])){
      if(i-1 > 0){
        x[i] <- x[i-1]
      }
    }
  }
  return(x)
}

parse_char <- function(x) {
  as.numeric(gsub("\\s+", "", x))
}

RGPH4 <- RGPH4 %>%
  mutate(
    Departements = if_else(grepl("DEP:", Departement), trimws(gsub("DEP:", "", Departement)), NA_character_),
    Departements = fill_na_with_previous(Departements),
    Communes = if_else(grepl("COM:", Departement), trimws(gsub("COM:", "", Departement)), NA_character_),
    Communes = fill_na_with_previous(Communes),
    Arrondissements = if_else(grepl("ARROND:", Departement), trimws(gsub("ARROND:", "", Departement)), NA_character_),
    Arrondissements = fill_na_with_previous(Arrondissements),
    Villages = trimws(gsub("DEP:|COM:|ARROND:", "", Departement))
  ) %>%
  filter(!grepl("DEP:|COM:|ARROND:", Departement)) %>%
  tidyr::drop_na() %>%
  mutate(across(.cols = `Nombre ménages`:`Féminin`, .fns = parse_char),
         `Taille ménage` = as.numeric(gsub("\\s+|,", ".", `Taille ménage`))) %>%
  select(-Departement) %>%
  relocate(Departements, Communes, Arrondissements, Villages, .before = 1)


usethis::use_data(RGPH4, overwrite = TRUE)
