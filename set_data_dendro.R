# file for load df_dendro and setup

# load df_dendro

library(tidyverse)
library(magrittr)

path_df_dendro <- list.files("data", full.names =  TRUE)

readxl::excel_sheets(path_df_dendro)

df_dendro <- readxl::read_excel(path_df_dendro)

# homogenize fields
df_dendro$ID_Sitio...2 <- ifelse(
    grepl("B", df_dendro$ID_Sitio...2), "BellaVista", "Yataiti"
    )

## identify redundant fields
# removing fields:
# 1. ID_CS: BV appears as 0 and yataity as 1
# 2. ID_Sitio...10: the same as ID_Sitio...2
# 3. ID_Arena: bella vista appears as arena 0 and yataity as `amarilla`
# 4. ID_P: the same as ID_Parcela

df_dendro <- df_dendro[-c(1, 3, 9, 10)]


# rename fields
# * ID_Sitio...2 -> Localidad
# * ID_Parcela -> Parcela
# * TRAT/PAR -> Tratamiento
# * ID_Dato -> Clase_Dato
# * ID_Empresa -> Empresa

df_dendro %<>% rename(
    Localidad = ID_Sitio...2,
    Parcela = ID_Parcela,
    Tratamiento = `TRAT/PAR`,
    Clase_Dato = ID_Dato,
    Empresa = ID_Empresa
    )


# remove fields h, d and `DAP/HT`, Edad...17: the same as HT, DAP, Edad...4
# remove field REPE: innecesary
# rename Edad...4 to Edad
df_dendro %<>% select(
    -c(h, d, `DAP/HT`, Edad...17, REPE)
    ) %>%
    rename(Edad = Edad...4)


# rename fields DENSIDAD and `Sup Parcela` to Densidad and Sup_Parcela
# rename fields ARBOL, DAP and HT to Arbol, Dap y Ht
df_dendro %<>% rename(
    Densidad = DENSIDAD, Sup_Parcela = `Sup Parcela`,
    Arbol = ARBOL, Dap = DAP, Ht = HT
    )

# reorder df_dendro fields to:
# Localidad, Empresa, Clase_Dato, Tratamiento, Parcela, Sup_Parcela, Densidad,
# Edad, Arbol, Dap, Ht
df_dendro %<>%
    select(
        Localidad, Empresa, Clase_Dato, Tratamiento, Parcela, Sup_Parcela,
        Densidad, Edad, Arbol, Dap, Ht
    )

df_dendro %>% colnames()

# identificar campos con NA's en la base de datos
colSums(is.na(df_dendro))

# imprimir primeros y los últimos diez registros de la base de datos
df_dendro %>% head(10)
df_dendro %>% tail(10)
