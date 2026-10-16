library(tidyverse)
library(readxl)
library(tsibble)
library(lubridate)
library(here) # Agregar here

pm25 <- read_excel(
  here("data", "PM2.5.xlsx"),
  sheet = "LIMPIO"
)
glimpse(pm25)
head(pm25)
pm25 <- pm25 %>%
  select(FECHA:TUMBACO)
head(pm25)
# vamos a cambiar formato ancho a largo osea las estaciones paran de columnas a ilas nada mas 
pm25_largo <- pm25 %>%
  pivot_longer(
    cols = -FECHA,
    names_to = "estacion",
    values_to = "pm25"
  )
head(pm25_largo)
# indexamos en fecha 
class(pm25_largo$FECHA)
# tsibble con key en estacion e index en FECHA
pm25_ts <- pm25_largo %>%
  as_tsibble(
    key = estacion,
    index = FECHA
  )
# Se corrigio el punto
pm25_ts
is_tsibble(pm25_ts)



