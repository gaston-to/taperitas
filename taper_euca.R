# Import, explorer and resume taper data from Eucalyptus

# Import useful packages R
library(tidyverse)
library(lubridate)
# *library(here)   # Relative paths (no lo tengo)
library(hms)    # Time
library(glue)   # Strings
library(magrittr) # Pipes

# Import data
df_taper_euca <- read.csv(
    list.files(
        "../../crecimiento-euca/data", pattern = "Perf", full.names = TRUE
        ),
    dec = ","
    )

df_taper_euca %>% glimpse()

# Explore data
# 1. How many Sites are there?
df_taper_euca %>%
    distinct(ID.Sitio) 

# 2. How many trees (ID.Arbol) are there per Site?
df_taper_euca %>%
    group_by(ID.Sitio) %>%
    summarise(n_trees = n_distinct(ID.Arbol)) %>%
    arrange(desc(n_trees))

# Eliminate Site == "BV"
df_taper_euca %<>% filter(ID.Sitio != "BV")

# 3. How many trees (ID.Arbol) are there per ID.Clase and per Trat?
df_taper_euca %>%
    group_by(ID.Clase, ID.Trat) %>%
    summarise(n_trees = n_distinct(ID.Arbol)) %>%
    arrange(desc(n_trees))

# 4. How many trees (ID.Arbol) are there per ID.Clase and Edad?
df_taper_euca %>%
    group_by(ID.Clase, Edad) %>%
    summarise(
        n_trat = n_distinct(ID.Trat),
        Dap_prom = mean(Dhb.cm, na.rm = TRUE),
        Ht_prom = mean(Ht.m, na.rm = TRUE),
        n_trees = n_distinct(ID.Arbol)
        ) %>%
    arrange(Edad)

# 5. Plot De vs Hi, facets = ID.Clase
df_taper_euca %>%
    ggplot(aes(x = Hi.m, y = De.cm, color = factor(Edad))) +
    geom_line(aes(group = ID.Arbol), color = "darkgrey") +
    geom_point() +
    scale_color_manual(
        "Edad (años)",
        values = c("darkgreen",  "#191988", "#960a0a")
        ) +
    facet_wrap(~ID.Clase, ncol = 1) +
    theme_bw() +
    labs(
        x = "Altura de corte (m)",
        y = "Diámetro con corteza (cm)",
        title = "Ahusamiento de Eucalyptus"
    )

# load FuncionesInutiles package
library(FuncionesInutiles)

# list of functions from FuncionesInutiles package
lsf.str("package:FuncionesInutiles")

# compute smallian vol for each log in tree
df_taper_euca %<>% smallian_vol(
        arbol = "ID.Arbol", di = De.cm, long = Hi.m, lt = FALSE
    ) %>%
    select(-ID.Sitio, -ID.Trat, -ID.Metodo, -Hi.m, -lt, -Es.c, -st, -nc) 

# resume data
# 1. Means per ID.Clase and ID.Arbol (Dhb.cm and Ht.m)
# 2. Sum per ID.Clase and ID.Arbol (Vol.m3)

resume <- df_taper_euca %>%
    group_by(ID.Clase, ID.Arbol) %>%
    summarise(
        Edad = mean(Edad),
        Dap = mean(Dhb.cm, na.rm = TRUE),
        Ht = mean(Ht.m, na.rm = TRUE),
        Vol_m3 = sum(Vol, na.rm = TRUE)
        )

resume %>% glimpse()

# calculate volume of cilinder whit Dap and Ht in resume
resume %<>% mutate(
    Vol_cil = pi * (Dap / 200)^2 * Ht
    )

resume %<>% mutate(
    ff = Vol_m3 / Vol_cil
    ) %>%
    select(-Vol_cil)

resume %>% group_by(Edad) %>% summarise(ff = mean(ff, na.rm = TRUE))

# Explore graphically the relationship between Vol and Dap conditioned by Edad
resume %>%
    ggplot(aes(x = Dap, y = Vol_m3, color = factor(Edad))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(
        "Edad (años)",
        values = c("darkgreen",  "#191988", "#960a0a")
        ) +
    theme_bw() +
    labs(
        x = "Diámetro con corteza (cm)",
        y = "Volumen (m3)",
        title = "Volumen de Eucalyptus"
    )

# Explore graphically the relationship between Vol and Ht conditioned by Edad
resume %>%
    ggplot(aes(x = Ht, y = Vol_m3, color = factor(Edad))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(
        "Edad (años)",
        values = c("darkgreen",  "#191988", "#960a0a")
        ) +
    theme_bw() +
    labs(
        x = "Altura de corte (m)",
        y = "Volumen (m3)",
        title = "Volumen de Eucalyptus"
    )

# calculate the corelation between Vol, Dap and Ht conditioned by Edad
resume %>%
    group_by(Edad) %>%
    summarise(
        r_Dap = cor(Dap, Vol_m3, use = "complete.obs"),
        r_Ht = cor(Ht, Vol_m3, use = "complete.obs")
        )

lm(Vol_m3 ~ Dap * factor(Edad), data = resume) %>% summary()

resume %>%
    ggplot(aes(x = Dap, y = Vol_m3, color = factor(Edad))) +
    geom_point() +
    geom_abline(
        aes(slope = 0.051, intercept = -0.63, colour = "Edad 10.6"), lty = 2
        ) +
    geom_abline(
        aes(slope = 0.051 + 0.011, intercept = -0.63 - 0.28, colour = "Edad 14"), lty = 3
        ) +
    geom_abline(
        aes(
            slope = 0.051 + 0.003, intercept = -0.63 - 0.23,
            colour = "Edad 18"
            ), lty = 4
        )