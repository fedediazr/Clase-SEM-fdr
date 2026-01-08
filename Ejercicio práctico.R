
##################################
# Ejercicio práctico
##################################

rm(list = ls())
load("Base_polarizacion_escalas.Rdata")
library(dplyr)
library(lavaan)


# Vamos a probar si el contacto intergrupal genera percepción de identidad común
# y eso a su vez disminuye la polarización

# Descriptivos

df_subset<-dplyr::select(df, Q60, Q61, Q62, Q68_1, Q68_2, Q68_3, id_pol)

library(Hmisc)
library(vtable)
library(labelled)

label(df_subset) <- list(
  Q60   = "Calidad de contacto (hostiles o amistosos)",
  Q61   = "Calidad de contacto (positivos o negativos)",
  Q62   = "Calidad de contacto (malas experiencias)",
  Q68_1 = "Identidad comun (destino comun)",
  Q68_2 = "Identidad comun (mismo barco)",
  Q68_3 = "Identidad comun (mismo grupo)",
  id_pol = "Polarización ideológica"
)

labels <- as.data.frame(var_label(df_subset))

df_subset <- data.frame(lapply(df_subset, function(x) as.numeric(as.character(x))))

sumtable(df_subset, labels = labels, summ = c("notNA(x)", "mean(x)", "sd(x)", "min(x)", "median(x)", "max(x)"))


## Modelo de medición

cfa <- '

## -----------------------------------------------
# Variables latentes
# -----------------------------------------------
calidad =~ Q60 + Q61 + Q62
id_comun =~ Q68_1 + Q68_2 + Q68_3 '

fit_cfa <- cfa(cfa, data = df_subset)
fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", "rmsea", "tli"))
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# Item Q62 tiene carga factorial negativa, volvemos a estimar invirtiendo item

df_subset$Q62<-6-df_subset$Q62
cfa <- '

## -----------------------------------------------
# Variables latentes
# -----------------------------------------------
calidad =~ Q60 + Q61 + Q62
id_comun =~ Q68_1 + Q68_2 + Q68_3 '

fit_cfa <- cfa(cfa, data = df_subset)
fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", "rmsea", "tli"))
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)


# Modelo completo

m2 <- '
  # -------------------------------
  # Modelos de medición
  # -------------------------------
  calidad =~ Q60 + Q61 + Q62
  id_comun =~ Q68_1 + Q68_2 + Q68_3

  # -------------------------------
  # Modelo estructural
  # -------------------------------
  id_comun ~ a*calidad
  id_pol  ~ b*id_comun + c*calidad

  # -------------------------------
  # Efectos indirectos y totales
  # -------------------------------
  efecto_indirecto := a*b
  efecto_total := c + (a*b)
'

fit_m2 <- sem(m2, data = df_subset, missing="FIML")
summary(fit_m2, fit.measures = TRUE, standardized = TRUE)