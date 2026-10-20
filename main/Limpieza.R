library(fpp3)
library(readxl)
library(here)

library(readr)
library(readxl)
library(readxl)

pm25 <- read_excel(
  "C:/Users/jazzd/OneDrive/Documentos/PM25/Data/PM2.5.xlsx",
  sheet = "LIMPIO",
  range = cell_cols("A:J"),
  col_types = c("date", rep("numeric", 9))
)

glimpse(pm25)

pm25_largo <- pm25 %>%
  pivot_longer(cols = -FECHA, names_to = "estacion", values_to = "pm25")

pm25_largo_corregido <- pm25_largo %>%
  mutate(FECHA = force_tz(round_date(FECHA, "hour"), "America/Guayaquil"))

duplicados <- pm25_largo_corregido %>%
  count(estacion, FECHA) %>%
  filter(n > 1)
stopifnot(nrow(duplicados) == 0)

pm25_ts <- pm25_largo_corregido %>%
  filter(!is.na(pm25)) %>%
  as_tsibble(key = estacion, index = FECHA) %>%
  fill_gaps()          # huecos internos -> NA explícitos

has_gaps(pm25_ts)      # TRUE esperado (hay huecos reales)
pm25_ts %>% count_gaps() %>% arrange(desc(.n))   # los huecos más largos




# Exportar a formato CSV
write_csv(pm25_ts, "C:/Users/jazzd/OneDrive/Documentos/PM25/Data/PM25_procesado.csv")
write_csv(pm25_ts, here("data", "PM25_limpio.csv"))
