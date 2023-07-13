# file for load data and setup

# load data

library(tidyverse)
library(magrittr)

path_data <- list.files("data", full.names =  TRUE)

readxl::excel_sheets(path_data)

data <- readxl::read_excel(path_data)

colnames(data)

data %>% head()

# homogenize fields
data$ID_Sitio...2 <- ifelse(
    grepl("B", data$ID_Sitio...2), "BellaVista", "Yataiti"
    )

## identify redundant fields
# removing field ID_CS: BV appears as 0 and yataity as 1
data <- data[-1]

# removing field ID_Sitio...10: the same as ID_Sitio...2
data <- data[-9]

# removing field ID_Arena:
# bella vista appears as arena 0 and yataity as `amarilla`
data <- data[-8]

# removing field ID_P: the same as ID_Parcela
data %>% count(ID_P, ID_Parcela)

plot(data$ID_P, data$ID_Parcela)

data <- data[-2]

# rename fields
# ID_Sitio...2 -> Localidad
# ID_Parcela -> Parcela
# TRAT/PAR -> Tratamiento
# ID_Dato -> Clase_Dato
# ID_Empresa -> Empresa

data <- data %>% rename(
    Localidad = ID_Sitio...2,
    Parcela = ID_Parcela,
    Tratamiento = `TRAT/PAR`,
    Clase_Dato = ID_Dato,
    Empresa = ID_Empresa
    )


# remove fields h, d and `DAP/HT`: the same as HT y DAP
data <- data %>% select(-c(h, d, `DAP/HT`))

# remove fields Edad...17 and rename Edad...4 to Edad
data <- data %>% select(-Edad...17) %>% rename(Edad = Edad...4)

# remove field REPE: innecesary
data <- data %>% select(-REPE)

# rename fields DENSIDAD and `Sup Parcela` to Densidad and Sup_Parcela
data <- data %>% rename(Densidad = DENSIDAD, Sup_Parcela = `Sup Parcela`)

# rename fields ARBOL, DAP and HT to Arbol, Dap y Ht
data <- data %>% rename(Arbol = ARBOL, Dap = DAP, Ht = HT)

# reorder data fields to Localidad, Empresa, Clase_Dato, Tratamiento, Parcela, Sup_Parcela, Densidad, Edad, Arbol, Dap, Ht
data %<>%
    select(
        Localidad, Empresa, Clase_Dato, Tratamiento, Parcela, Sup_Parcela,
        Densidad, Edad, Arbol, Dap, Ht
    )

data %>% colnames()

# identificar campos con NA's en la base de datos
colSums(is.na(data))

# imprimir primeros y los últimos diez registros de la base de datos
data %>% head(10)
data %>% tail(10)
