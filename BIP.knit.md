# Das Bruttoinlandsprodukt

## Begriff

**Definition**: Das **Bruttoinlandsprodukt (BIP)** ist der **Marktwert** aller für den **Endverbrauch** bestimmten **Waren und Dienstleistunge**n, die **in einem Land** **in einem bestimmten Zeitabschnitt** hergestellt werden.

- Marktwert (staatliche Leistungen ohne Preis: Kosten)

- Nur Endverbrauch. ZWischenprodukte werden nicht mitgezählt, um Doppelzählung zu vermeiden


- Keine gebrauchten Güter

- Lagerbestandsänderungen $\rightarrow$ Investitionen

- Geografische Abgrenzung (Inland, nicht Inländer)

- zeitliche Abgrenzung. Saison- und Feiertagsbereinigung


## Entwicklung


::: {.cell}

```{.r .cell-code}
library(tidyverse)

df <- read.csv("gdp-per-capita-worldbank.csv")

#head(df)

selected_countries <- c("Germany", "France", "United States", "Turkey", 
                       "China", "Greece", "Italy", "Ireland", "India")

options(scipen=999)

df %>% filter(Entity %in% selected_countries) %>% 
  ggplot(aes(Year, GDP.per.capita..PPP..constant.2017.international...))+
  geom_point()+
  geom_line()+
  geom_smooth(method="glm")+
  scale_y_log10() +
  theme_light()+
  labs(title="Die Entwicklung des BIP pro Kopf",
       x="Jahr",
       y="BIP pro Kopf, konstante 2017 international Dollar, PPP",
       caption= "Eigene Darstellung, Daten: Ourworldindata.org")+
  facet_wrap(~Entity , scales = "free_y")
```

::: {.cell-output-display}
![](BIP_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::

```{.r .cell-code}
# Tabelle Wachstumsraten

library(broom)

growth <- df %>% filter(Entity %in% selected_countries) %>% 
  group_by(Entity) %>% 
  do(tidy(lm(log(GDP.per.capita..PPP..constant.2017.international...) ~ Year,.))) %>% 
  filter(term=="Year") %>% 
  select(Entity, estimate) %>% 
  rename(Staat=Entity,
         Wachstumsraten=estimate) %>% 
  mutate(Wachstumsraten=Wachstumsraten*100)

#head(growth)

library(knitr)

kable(growth, digits=2, format.args = list(decimal.mark = ','), col.names=c("Staat", "Wachstumsrate (%)"),
      caption="geschätzte Durchschnittliche jährliche Wachstumsrate (%) 1990-2021, Daten: Ourworldindata.org")
```

::: {.cell-output-display}


Table: geschätzte Durchschnittliche jährliche Wachstumsrate (%) 1990-2021, Daten: Ourworldindata.org

|Staat         | Wachstumsrate (%)|
|:-------------|-----------------:|
|China         |              8,32|
|France        |              0,97|
|Germany       |              1,25|
|Greece        |              0,57|
|India         |              4,59|
|Ireland       |              3,71|
|Italy         |              0,28|
|Turkey        |              3,05|
|United States |              1,44|


:::
:::



## Effizienzfragen

- Kosten der Schwankung des BIP

- Kosten es Wachstums des BIP

- Misst das BIP Wohlstand?

## Zusammenhänge

Verwendungsgleichung: $Y=C+I+G+(Ex-Im)$

oder: $Y+Im=C+I+Ex$

Gespart wird, was nicht konsumiert wird. 

- Geschlossene Volkswirtschaft $S=Y-C-G \Leftrightarrow I=S$

- Offene Volkswirtschaft: $Y=C+I+G+NX\Leftrightarrow Y-C-G=I+NX\Leftrightarrow I=Y-C-G-NX$

- Nominales vs. reales BIP



