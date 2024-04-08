library(tidyverse)

df <- read.csv("gdp-per-capita-worldbank.csv")

head(df)

selected_counries <- c("Germany", "France", "United States", "Turkey", 
                       "China", "Greece", "Italy", "Ireland", "India")

df %>% filter(Entity %in% selected_counries) %>% 
  ggplot(aes(Year, GDP.per.capita..PPP..constant.2017.international...))+
  geom_point()+
  geom_line()+
  geom_smooth(method="glm")+
  scale_y_log10() +
  theme_light()+
  labs(title="Die Entwicklung des BIP pro Kopf",
       x="Jahr",
       y="BIP pro Kopf, konstante 2017 international Dollar, PPP")+
  facet_wrap(~Entity , scales = "free_y"
             )

# Tabelle Wachstumsraten

library(broom)

growth <- df %>% filter(Entity %in% selected_counries) %>% 
  group_by(Entity) %>% 
  do(tidy(lm(log(GDP.per.capita..PPP..constant.2017.international...) ~ Year,.))) %>% 
  filter(term=="Year") %>% 
  select(Entity, estimate) %>% 
  rename(Staat=Entity,
         Wachstumsraten=estimate) %>% 
  mutate(Wachstumsraten=Wachstumsraten*100)

head(growth)

library(knitr)

kable(growth)
