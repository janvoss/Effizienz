#library(jsonlite)

library(dplyr)
library(tidyr)
library(purrr)
library(forecast)
library(ggplot2)


countries_of_interest <- c("Germany", "Italy", "Ireland",  "Greece", "France", "Poland", "China")

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/gdp-per-capita-worldbank.csv?v=1&csvType=full&useColumnShortNames=true") %>%
  select(!c(code, owid_region)) %>%
  filter(entity %in% countries_of_interest)  %>%
  mutate(ny_gdp_pcap_pp_kd= log(ny_gdp_pcap_pp_kd))

# Fetch the metadata
# metadata <- fromJSON("https://ourworldindata.org/grapher/gdp-per-capita-worldbank.metadata.json?v=1&csvType=full&useColumnShortNames=true")

head(df)

# Prognosen erstellen

# Prognosezeitraum

h <- 10

dat_forecast <- df %>%
  group_by(entity) %>%
  nest() %>%
  mutate(
    # Zeitreihen-Objekte erstellen
    ts_obj = map(data, ~ ts(.x$ny_gdp_pcap_pp_kd, start = min(.x$year), frequency = 1)),
    # ARIMA-Modelle anpassen
    modell = map(ts_obj, ~ auto.arima(.x)),
    # Prognosen erstellen (h Jahre)
    prognose = map(modell, ~ forecast(.x, h = h)),
    # Letztes Jahr der historischen Daten
    last_year = map_int(data, ~ max(.x$year)),
    # Prognose-Tibbles erstellen
    prognose_tbl = map2(prognose, last_year, ~ {
      tibble(
        year = (.y + 1):(.y + h),
        Prognose = as.numeric(.x$mean),
        Unteres95 = as.numeric(.x$lower[, 2]),
        Oberes95 = as.numeric(.x$upper[, 2])
      )
    })
  ) %>%
  select(entity, prognose_tbl) %>%
  unnest(prognose_tbl)

# Originaldaten für Plot vorbereiten
df_plot <- df %>%
  mutate(Typ = "Original") %>%
  select(entity, year, Wert = ny_gdp_pcap_pp_kd, Typ)

# Prognosedaten für Plot vorbereiten
prognose_plot <- dat_forecast %>%
  mutate(Typ = "Prognose") %>%
  rename(Wert = Prognose) %>%
  select(entity, year, Wert, Typ, Unteres95, Oberes95)

# Kombinierte Daten für ersten Plot
plot_df <- bind_rows(df_plot, prognose_plot)

head(plot_df)

# 2. Plot: Mit Unsicherheitsintervallen
ggplot() +
  geom_line(data = df_plot, aes(x = year, y = Wert), color = "black") +
  geom_line(data = prognose_plot, aes(x = year, y = Wert), color = "blue", linetype = "dashed") +
  geom_ribbon(
    data = prognose_plot,
    aes(x = year, ymin = Unteres95, ymax = Oberes95),
    alpha = 0.2,
    fill = "blue"
  ) +
 #  scale_y_log10() +
  facet_wrap(~ entity, scales = "free_y") +
  theme_minimal() +
  labs(title = "BIP Zeitreihen und ARIMA-Prognose mit Unsicherheit",
       subtitle = "log BIP pro Kopf, kaufkraftbereinigte Dollar",
       x= "Jahr",
       caption = "Eigene Darstellung. Daten: ourworldindata.org")

# Zusätzlich: Modellzusammenfassungen anzeigen
modelle <- df %>%
  group_by(entity) %>%
  nest() %>%
  mutate(
    ts_obj = map(data, ~ ts(.x$ny_gdp_pcap_pp_kd, start = min(.x$year), frequency = 1)),
    modell = map(ts_obj, ~ auto.arima(.x))
  )

# Modellzusammenfassungen anzeigen
# for(entity in modelle$entity) {
#  cat(paste("\nModell für", entity, ":\n"))
#  print(summary(modelle$modell[[which(modelle$entity == entity)]]))
#  cat("----------------------------------------\n")
#}