library(tidyverse)
# library(owidR)
library(RColorBrewer)

# Daten von OWID laden

gdppc <- read.csv("gdp-per-capita-worldbank.csv")
lifeex <- read.csv("life-expectancy.csv")
gini <- read.csv("economic-inequality-gini-index.csv")
pop <- read.csv("population-long-run-with-projections.csv")

# Daten zusammenfügen

df <- gdppc %>% 
  left_join(lifeex) %>%
  left_join(gini) %>%
  left_join(pop) %>%
  
  #Dummy for EU Member States
  mutate(dummy_EU = as.numeric(Entity %in% c("Austria", "Belgium", "Denmark",
                                             "Finland", "France", "Germany", 
                                             "Greece", "Ireland", "Italy",
                                             "Luxembourg", "Netherlands", "Portugal",
                                             "Spain", "Sweden", "United Kingdom"))) %>%
  #Dummy for USA
  mutate(dummy_USA = as.numeric(Entity %in% c("United States")))

## Lücken füllen

df %<>%  group_by(Entity) %>% 
  fill(Gini.coefficient, .direction = "downup") %>% 
  ungroup()


p <- df %>% 
  #Gruppen herauswerfen
  filter(is.na(Code)==F,
         Entity !="World" #,
        # Entity=="United States"
         )%>%
  ggplot(aes(Year, GDP.per.capita..PPP..constant.2017.international..., label=Entity))+
 # geom_boxplot()+
#  geom_smooth()+
 # geom_quantile(size=1)+
  geom_point(aes(color = Gini.coefficient),alpha=.7)+
#  geom_point()+
  scale_color_distiller(palette='Spectral')+
  scale_y_continuous(labels= function(x) format(x,scientific = F) , 
                     trans = "log10"
  )+
  theme_light()


p


library(plotly)

p1 <- ggplotly(p) %>%
  layout(margin = list(l = 50, r = 50, b = 100, t = 50),
         annotations = list(x = 1, y = -0.3, text =  paste('Abbildung: Jan S. Voßwinkel; Daten: Our World in Data, Datenabruf:', Sys.Date(), sep = " "),
                            xref='paper', yref='paper', showarrow = F, 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font = list(size = 11)))

p1


library(canvasXpress)

p2 <- canvasXpress(p)
p2

#----------------------

df1 <- df %>% 
  filter(Year==1990) %>% 
  select(Entity, Code, Gini.coefficient) %>% 
  rename(Gini1990 = Gini.coefficient)

df2 <- df %>% 
  filter(Year==2019) %>% 
  select(Entity, Code, Gini.coefficient , GDP.per.capita..PPP..constant.2017.international...) %>% 
  rename(Gini2019 = Gini.coefficient)

df3 <- df1 %>% 
  left_join(df2)

p <- df3 %>% 
  filter(is.na(Code)==F,
         Entity !="World")%>%
  ggplot(aes(x=Gini1990,
             y=Gini2019,
             group=Entity))+
  scale_x_continuous(labels= function(x) format(x,scientific = F) #, 
                     # trans = "log10"
  )+
  scale_y_continuous(labels= function(x) format(x,scientific = F) #,
                     # trans="log10"
  )+
  scale_color_distiller(palette='Spectral')+
  geom_point(
#    aes(color=`GDP per capita`, size=Population #, frame=year
    #)
  )+
  geom_smooth(aes(group=NULL #, weight=Population
                  ))+
  geom_abline(slope = 1,
              intercept = 0,
              color="red")+
  labs(title="Gini-Index 1990 vs. 2019",
       #subtitle= expression('2020=100'),
       x="Gini-Index 1990",
       y='Gini-Index 2019 ',
       caption = paste('Abbildung: Jan S. Voßwinkel; Daten: Our World in Data, Datenabruf:', Sys.Date(), sep = " ")
  )

# p

p1 <- ggplotly(p)%>%
  layout(margin = list(l = 50, r = 50, b = 100, t = 50),
         annotations = list(x = 1, y = -0.3, text =  paste('Abbildung: Jan S. Voßwinkel; Daten: Our World in Data, Datenabruf:', Sys.Date(), sep = " "),
                            xref='paper', yref='paper', showarrow = F, 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font = list(size = 11)))

p1
