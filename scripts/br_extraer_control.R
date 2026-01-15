library(ipeadatar)
library(tidyverse)

main <- function() {
  # La intención del siguiente proceso es la de extraer variables de control desde la API de ipeadata para el país de Brasil a nivel regional
  # (estados federativos)
  
  regiones <- available_territories("en")
  
  br_control_df <- c("PNADCA_GINIUF", "PIBPCE", "PNADCA_TXPNUF", "ADH12_POPTOT","PNADCT_TXINFORMUF","IDHMRE","SIS_RDPCMEDIO","PNADCA_NMAE25UF", "THOMIC") |>
    map(\(code) {
      ipeadata(code, "en") |>
        mutate(tcode=as.character(tcode)) |>
        filter(uname == "States") |>
        # Agregamos 'area' al select para que la información pase al dataframe
        # ya estaban los datos aca mismo
        inner_join(regiones |> select(tcode, tname, area), by = "tcode")
    }) |>
    bind_rows() |>
    # Agregamos 'area' a id_cols para que no desaparezca al transformar la tabla a formato ancho
    pivot_wider(id_cols=c("date", "tname", "area"), names_from="code", values_from="value") |>
    rename(
      anio = "date",
      region = "tname",
      area_km2 = "area",
      gini = "PNADCA_GINIUF",
      pib_per_capita = "PIBPCE",
      tasa_pobr = "PNADCA_TXPNUF",
      pobl_total = "ADH12_POPTOT",
      tasa_informalidad = "PNADCT_TXINFORMUF",
      idh = "IDHMRE",
      ingreso_medio = "SIS_RDPCMEDIO",
      prom_anio_estudio = "PNADCA_NMAE25UF",
      tasa_homicidios = "THOMIC"
    ) |>
    # Calculamos la densidad poblacional
    mutate(densidad_pobl = pobl_total / area_km2)
  
  br_control_df |>
    write_csv(file.path("data", "processed", "variables", "br", "control.csv"))
}

main()