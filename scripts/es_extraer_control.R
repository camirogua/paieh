library(tidyverse)
library(eurostat)

#' Leer y transformar una variable del Instituto Nacional de España desde un archivo CSV descargado
#'
#' Esta función lee un archivo CSV proveniente del INE, normaliza los nombres
#' de las columnas, separa la columna de comunidades y ciudades autónomas
#' en código y región, construye una fecha anual a partir del período
#' y devuelve un data frame con una estructura homogénea para su posterior análisis.
#'
#' El resultado contiene una columna de fecha (`anio`), la región,
#' la variable solicitada y su unidad asociada.
#'
#' @param file_path Character. Ruta al archivo CSV que contiene los datos del INE.
#'
#' @param var_name Character. Nombre lógico de la variable que se desea asignar
#'   a la columna de valores (por ejemplo: `"poblacion"`, `"tasa_homicidios"`, etc.).
#'
#' @param var_unit_name Character. Nombre lógico que se asignará a la columna
#'   que representa la unidad de medida de la variable.
#'
#' @return A tibble con las siguientes columnas:
#' \describe{
#'   \item{anio}{Date. Fecha anual construida a partir del período.}
#'   \item{region}{Character. Nombre de la comunidad o ciudad autónoma.}
#'   \item{<var_name>}{Numeric. Valores de la variable seleccionada.}
#'   \item{<var_name>_unidad}{Character. Unidad asociada a la variable.}
#' }
#'
#' @details
#' La función asume que:
#' \itemize{
#'   \item El archivo CSV utiliza codificación ISO-8859-3.
#'   \item Existe una columna denominada "Comunidades y ciudades autónomas"
#'     que puede separarse por `" - "`.
#'   \item El período corresponde a un año.
#' }
#'
read_ine_variable <- function(file_path, var_name, var_unit_name) {
  return(
    read_csv2(
      file_path,
      locale = locale(encoding = "ISO-8859-3")
    ) |>
    rename_with(tolower) |>
    separate(
      `comunidades y ciudades autónomas`, into = c("codigo", "region"), sep = " ", extra = "merge", fill = "left"
    ) |>
    mutate(anio = as.Date(paste(periodo, "01", "01", sep="-"))) |>
    select(anio, region, {{var_name}} := total, "{var_name}_unidad" := {var_unit_name})
  )
}

#' Leer y transformar una variable de Eurostat para regiones españolas
#'
#' Esta función descarga una variable desde Eurostat, filtra los datos
#' para las regiones NUTS de España, une la información geográfica
#' correspondiente y devuelve un data frame estandarizado con valores
#' anuales por región.
#'
#' Permite aplicar filtros adicionales dinámicos y define un período
#' temporal configurable.
#'
#' @param id Character. Identificador del dataset de Eurostat
#'   (por ejemplo `"nama_10r_3gdp"`).
#'
#' @param var_name Character. Nombre lógico que se asignará a la variable
#'   contenida en la columna `values`.
#'
#' @param more_filters List. Lista opcional de filtros adicionales
#'   que se pasarán al argumento `filters` de `get_eurostat()`.
#'   Cada elemento debe ser un vector nombrado.
#'
#' @param period Character vector. Vector de años en formato `"YYYY-MM-DD"`
#'   que define el período temporal a consultar.
#'   Por defecto va de 2008 a 2023.
#'
#' @return A tibble con las siguientes columnas:
#' \describe{
#'   \item{anio}{Character. Año de referencia.}
#'   \item{id_region}{Character. Código NUTS de la región.}
#'   \item{region}{Character. Nombre de la región.}
#'   \item{<var_name>}{Numeric. Valor de la variable de Eurostat.}
#'   \item{<var_name>_unidad}{Character. Unidad de medida de la variable.}
#' }
#'
#' @details
#' La función:
#' \itemize{
#'   \item Filtra automáticamente por regiones NUTS de España.
#'   \item Realiza un `inner_join` con la tabla geográfica de Eurostat.
#'   \item Renombra dinámicamente columnas usando tidy evaluation.
#'   \item Elimina registros duplicados mediante `distinct()`.
#' }
#'
read_eurostat_variable <- function(id, var_name, more_filters = list(), period=format(seq.Date("2008-01-01", "2023-01-01", by="1 year"), "%Y")) {
  return(
    get_eurostat(
      id,
      filters=c(list(
        geo=c("ES11","ES12","ES13","ES21","ES22","ES23","ES24","ES30","ES41","ES42","ES43","ES51","ES52","ES53","ES61","ES62","ES63","ES64","ES70"),
        time_period=format(seq.Date("2008-01-01", "2023-01-01", by="1 year"), "%Y")
      ),
      more_filters
      )
    ) |>
      inner_join(
        eurostat_geodata_60_2016 |> select(NUTS_ID, region = NUTS_NAME, CNTR_CODE) |> filter(tolower(CNTR_CODE) == "es"),
        by = join_by(geo == NUTS_ID)
      ) |>
      select(anio = time, id_region = geo, region, {{var_name}} := values, "{var_name}_unidad" := unit) |>
      distinct()
  )
}

main <- function() {
  # La intención del siguiente proceso es la de extraer variables de control desde diversas fuentes (eurostat e INE) para el país de España a nivel regional
  # (comunidades autonomas)
  
  # Leemos y unimos cada variable de control conseguida a través de eurostat
  eurostat_control_df <- list(
    list(id="nama_10r_2hhinc", var_name="renta_media_hogar", more_filters=list(unit="EUR_HAB", direct="BAL", na_item="B5N")),
    list(id="nama_10r_3gdp", var_name="pbi_per_capita", more_filters=list(unit="EUR_HAB")),
    list(id="demo_r_d3dens", var_name="densidad_pobl")
  ) |>
    map(do.call, what=read_eurostat_variable) |>
    reduce(left_join, by=join_by(anio, id_region, region))
  
  # Idem pero para las descargadas desde el Instituto de Estadistica de España (INE)
  ine_control_df <- list(
    list(file_path=file.path("data", "raw", "es", "control", "tasa_homicidios.csv"), var_name="tasa_homicidios", var_unit_name="tipo de tasa"),
    list(file_path=file.path("data", "raw", "es", "control", "tasa_criminalidad.csv"), var_name="tasa_criminalidad", var_unit_name="tipo de tasa"),
    list(file_path=file.path("data", "raw", "es", "control", "tasa_pobreza.csv"), var_name="tasa_pobreza", var_unit_name="tasa de riesgo de pobreza"),
    list(file_path=file.path("data", "raw", "es", "control", "poblacion_total.csv"), var_name="pobl_total", var_unit_name="sexo")
  ) |>
    map(do.call, what=read_ine_variable) |>
    reduce(left_join, by=join_by(anio, region))
  
  # Leemos el IDH por separado debido a que es un csv que contiene el HDI nacional y subnacional para todos los paises del mundo practicamente
  idh_df <- read_csv(file.path("data", "raw", "control", "GDL-Subnational-HDI-data.csv"), col_select = c("Country", "Level", "Region", "Year", "shdi")) |>
    rename_with(tolower) |>
    filter(country == "Spain" & level == "Subnat" & year > 2007) |>
    mutate(anio = as.Date(paste(year, "01", "01", sep="-")), hdi_unidad = "Indice") |>
    select(anio, region, hdi = shdi, hdi_unidad)
  
  # Armamos el df final que es la unión de los anteriores.
  es_control_df <- eurostat_control_df |>
    mutate(region_cleaned = stringi::stri_trans_general(region, id = "Latin-ASCII") |> tolower()) |>
    left_join(
      idh_df |> mutate(region_cleaned = tolower(region)) |> select(!region),
      by=join_by(region_cleaned, anio)
    ) |>
    left_join(
      ine_control_df |>
        mutate(region_cleaned = stringi::stri_trans_general(region, id = "Latin-ASCII")) |>
        mutate(
          region_cleaned = case_when(
            str_detect(region, "Asturias") ~ "Principado de Asturias",
            str_detect(region, "Balears") ~ "Illes Balears ",
            str_detect(region, "Castilla - La Mancha") ~ "Castilla-La Mancha",
            str_detect(region, "Valenciana") ~ "Comunidad Valenciana",
            str_detect(region, "Madrid") ~ "Comunidad de Madrid",
            str_detect(region, "Murcia") ~ "Región de Murcia",
            str_detect(region, "Navarra") ~ "Comunidad Foral de Navarra",
            str_detect(region, "Rioja") ~ "La Rioja",
            str_detect(region, "Ceuta") ~ "Ciudad Autónoma de Ceuta",
            str_detect(region, "Melilla") ~ "Ciudad Autónoma de Melilla",
            .default = as.character(region_cleaned)
          ) |> tolower()
        ) |>
        select(!region),
      by=join_by(region_cleaned, anio)
    ) |>
    select(!region_cleaned)
  
  # Escribimos los dataframes intermedios (por si necesitamos corroborar y/o reconstruir)
  interim_dir <- file.path("data", "interim", "variables", "es", "control")
  dir.create(interim_dir, showWarnings = F, recursive = T)
  
  eurostat_control_df |>
    write_csv(file.path(interim_dir, "eurostat.csv"))
  
  ine_control_df |>
    write_csv(file.path(interim_dir, "ine.csv"))
  
  # Escribimos el dataframe final procesado
  es_control_df |>
    write_csv(file.path("data", "processed", "variables", "es", "control.csv"))
}

main()