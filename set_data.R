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
data <- data[-1]
data <- data[-9]

# removing field ID_Arena:
# bella vista appears as arena 0 and yataity as `amarilla`
data <- data[-8]

data %>% count(ID_Sitio...2, ID_Arena)

data %>% str()
