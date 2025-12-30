library(tidyverse)
library(eurostat)
library(stringi)

#' Leer y transformar una variable de Eurostat para regiones italianas
#'
#' Esta función descarga una variable desde Eurostat, filtra los datos
#' para las regiones NUTS de Italia, une la información geográfica
#' correspondiente y devuelve un data frame estandarizado con valores
#' anuales por región.
#'
#' @param id Character. Identificador del dataset de Eurostat.
#' @param var_name Character. Nombre lógico para la columna de valores.
#' @param more_filters List. Filtros adicionales para get_eurostat().
#' @param period Character vector. Años a consultar.
#'
read_eurostat_variable <- function(
    id,
    var_name,
    more_filters = list(),
    period = format(seq.Date(as.Date("2008-01-01"), as.Date("2023-01-01"), by = "1 year"), "%Y")
) {
  return(
    get_eurostat(
      id,
      filters = c(
        list(
          geo = c(
            "ITC1","ITC2","ITC3","ITC4",
            "ITH1","ITH2","ITH3","ITH4","ITH5",
            "ITI1","ITI2","ITI3","ITI4",
            "ITF1","ITF2","ITF3","ITF4","ITF5","ITF6",
            "ITG1","ITG2"
          ),
          time_period = period
        ),
        more_filters
      )
    ) |>
      inner_join(
        eurostat_geodata_60_2016 |>
          select(NUTS_ID, region = NUTS_NAME, CNTR_CODE) |>
          filter(CNTR_CODE == "IT"),
        by = join_by(geo == NUTS_ID)
      ) |>
      select(
        anio = time,
        id_region = geo,
        region,
        {{ var_name }} := values,
        "{var_name}_unidad" := unit
      ) |>
      distinct()
  )
}

main <- function() {
  # La intención del siguiente proceso es la de extraer variables de control desde diversas fuentes (eurostat) 
  # para Italia a nivel regional (regioni)
  
  # Leemos y unimos cada variable de control conseguida a través de eurostat
  eurostat_control_df <- list(
  list(id="nama_10r_2hhinc", var_name="renta_media_hogar", more_filters=list(unit="EUR_HAB", direct="BAL", na_item="B5N")),
  list(id="nama_10r_3gdp", var_name="pbi_per_capita", more_filters=list(unit="EUR_HAB")),
  list(id="demo_r_d3dens", var_name="densidad_pobl"),
  list(id="demo_r_pjangrp3", var_name="poblacion", more_filters=list(unit="NR", sex="T", age="TOTAL")),
  # Tasa de riesgo de pobreza regional (EU-SILC, sin alquiler imputado) - esto tmb esta para es - mas comparable
  #no encontre equivalente en istat a la tasa con alquiler imputado de ine españa
  list(id="ilc_li41", var_name="tasa_pobreza", more_filters=list(unit="PC"))
  ) |>
    map(do.call, what = read_eurostat_variable) |>
    reduce(left_join, by = join_by(anio, id_region, region))
  
  # agrego Criminalidad de eurostat (Denuncias policiales)
  # estaba en nuts3, paso a nuts2 - se podria hacer lo mismo para españa
  denuncias <- get_eurostat(
    "crim_gen_reg",
    filters = list(
      geo = c(
        "ITC1","ITC2","ITC3","ITC4",
        "ITH1","ITH2","ITH3","ITH4","ITH5",
        "ITI1","ITI2","ITI3","ITI4",
        "ITF1","ITF2","ITF3","ITF4","ITF5","ITF6",
        "ITG1","ITG2"
      ),
      time_period = format(
        seq.Date(as.Date("2008-01-01"), as.Date("2023-01-01"), by = "1 year"),
        "%Y"
      )
    )
  ) |>
    mutate(
      id_region = geo,
      anio = as.Date(paste(time, "01", "01", sep = "-"))
    ) |>
    group_by(id_region, anio) |>
    summarise(
      denuncias = sum(values, na.rm = TRUE),
      denuncias_unidad = first(unit),
      .groups = "drop"
    ) |>
    inner_join(
      eurostat_geodata_60_2016 |>
        select(NUTS_ID, region = NUTS_NAME, CNTR_CODE) |>
        filter(CNTR_CODE == "IT"),
      by = join_by(id_region == NUTS_ID)
    ) |>
    select(anio, id_region, region, denuncias, denuncias_unidad)
  
  # hago tasa de criminalidad a mano
  eurostat_control_df <- eurostat_control_df |>
    left_join(denuncias, by = join_by(anio, id_region, region)) |>
    mutate(
      tasa_criminalidad = round(denuncias / poblacion * 100000, 1),
      tasa_criminalidad_unidad = "por 100.000 hab."
    )

  
  # Leemos el IDH por separado debido a que es un csv que contiene el HDI nacional y subnacional para todos los paises del mundo practicamente
  idh_df <- read_csv(
    file.path("data", "raw", "control", "GDL-Subnational-HDI-data.csv"),
    col_select = c("Country", "Level", "Region", "Year", "shdi")
  ) |>
    rename_with(tolower) |>
    filter(country == "Italy" & level == "Subnat" & year > 2007) |>
    mutate(
      anio = as.Date(paste(year, "01", "01", sep = "-")),
      hdi_unidad = "Indice"
    ) |>
    select(anio, region, hdi = shdi, hdi_unidad)
  
  # Armamos el df final que es la unión de los anteriores. 
  # con normalización usando stringi (ni idea esto me lo sugirio la ia, no se si es necesario )
  it_control_df <- eurostat_control_df |>
    mutate(region_cleaned = stri_trans_general(region, id = "Latin-ASCII") |> tolower()) |>
    left_join(
      idh_df |>
        mutate(region_cleaned = tolower(region)) |>
        select(!region),
      by = join_by(region_cleaned, anio)
    ) |>
    select(!region_cleaned)
  
  # Escribimos los dataframes intermedios (por si necesitamos corroborar y/o reconstruir)
  interim_dir <- file.path("data", "interim", "variables", "it", "control")
  dir.create(interim_dir, showWarnings = FALSE, recursive = TRUE)
  
  eurostat_control_df |>
    write_csv(file.path(interim_dir, "eurostat.csv"))
  
  # Escribimos el dataframe final procesado
  it_control_df |>
    write_csv(file.path("data", "processed", "variables", "it", "control.csv"))
}

main()
