# Script to generate a the summary statistics
# for the dendromtric data of the tapperitas sites

# load source code set_data_dendro.R
source("set_data_dendro.R")

# filter data by Localidad != "BellaVista"
df_dendro_yataity <- df_dendro %>%
    filter(Localidad != "BellaVista")

# redondear Densidad a 0 decimales
df_dendro_yataity %<>% mutate(Densidad = round(Densidad, 0))

# graficar dap vs edad
# discriminar con forma por tipo de datos y cn colores la densidad
ggplot(
    df_dendro_yataity,
    aes(x = Edad, y = Dap, shape = Clase_Dato, color = factor(Densidad))
    ) +
    geom_point() +
    labs(
        title = "Dap vs Edad",
        x = "Edad (años)",
        y = "Dap (cm)"
    ) +
    scale_color_discrete(name = "Densidad (plantas/ha)") +
    theme_bw()

# identificar parcelas en las que la densidad se modifica en una misma edad
# y generar lista de parcelas a eliminar
df_dendro_yataity %>%
    group_by(Edad, Parcela) %>%
    summarise(
        l = sd(Densidad, na.rm = TRUE)
    ) %>%
    filter(l != 0) %>%
    select(Parcela)

df_dendro_yataity %>%
filter(Parcela == 45 & Edad == 6.25) %>%
tail()

# drop df_dendro
rm(df_dendro)

# si Empresa no varia, elminar campos Localidad y Empresa
df_dendro_yataity %<>% select(-c(Localidad, Empresa))

# Generar resumen de datos dendrometricos
# Agrupar Por Clase_Dato, Tratamiento, Parcela, Edad
# calcular media de Sup_Parcela en Ha, Densidad, Dap y Ht,
# contar cantidad de Arboles, area basal
# y sumatoria de volumen
df_dendro_yataity_summary <- df_dendro_yataity %>%
    group_by(Clase_Dato, Tratamiento, Parcela, Edad) %>%
    summarise(
        Sup_Parcela = mean(Sup_Parcela, na.rm = TRUE) / 10000,
        Densidad = mean(Densidad, na.rm = TRUE),
        Arboles = n(),
        Dap_m = mean(Dap, na.rm = TRUE),
        Ht_m = mean(Ht, na.rm = TRUE),
        Area_Basal = sum(pi * (Dap / 200) ^ 2, na.rm = TRUE),
        Volumen = sum(pi * (Dap / 200) ^ 2 * Ht, na.rm = TRUE)
    )

# summary df_dendro_yataity_summary
summary(df_dendro_yataity_summary)

# drop df_dendro_yataity
# * rm(df_dendro_yataity)

# Modify Area_Basal: reference area is Sup_Parcela
df_dendro_yataity_summary %<>% mutate(
    Area_Basal = Area_Basal / Sup_Parcela
    )

# Modify Volumen: reference area is Sup_Parcela and factor 0.45 is applied
df_dendro_yataity_summary %<>% mutate(
    Volumen = Volumen / Sup_Parcela * 0.45
    )

# realizar histograma de la edad
ggplot(df_dendro_yataity_summary, aes(x = Edad)) +
    geom_histogram(binwidth = 1) +
    labs(
        title = "Histograma de Edad",
        x = "Edad (años)",
        y = "Frecuencia"
    ) +
    theme_bw()


# plot Area_Basal vs Edad by Tratamiento (Factor) and Clase_Dato (Facet)
# lines group by Parcela
ggplot(df_dendro_yataity_summary, aes(x = Edad, y = Area_Basal)) +
    geom_line(aes(group = Parcela)) +
    geom_point() +
    facet_grid(Clase_Dato ~ Tratamiento) +
    labs(
        title = "Area Basal vs Edad",
        x = "Edad (años)",
        y = "Area Basal (m2/ha)"
    ) +
    theme_bw()

# plot Volumen vs Edad by Tratamiento (Factor) and Clase_Dato (Facet)
# lines group by Parcela
ggplot(df_dendro_yataity_summary, aes(x = Edad, y = Volumen)) +
    geom_line(aes(group = Parcela)) +
    geom_point() +
    facet_grid(Clase_Dato ~ Tratamiento) +
    labs(
        title = "Volumen vs Edad",
        x = "Edad (años)",
        y = "Volumen (m3/ha)"
    ) +
    theme_bw()

# plot Dap_m vs Edad by Tratamiento (Factor) and Clase_Dato (Facet)
# lines group by Parcela
ggplot(df_dendro_yataity_summary, aes(x = Edad, y = Dap_m)) +
    geom_line(aes(group = Parcela)) +
    geom_point() +
    facet_grid(Clase_Dato ~ Tratamiento) +
    labs(
        title = "Dap vs Edad",
        x = "Edad (años)",
        y = "Dap (cm)"
    ) +
    theme_bw()
