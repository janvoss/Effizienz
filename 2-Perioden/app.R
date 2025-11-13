#| file: app.R
#| title: Intertemporale Konsumoptimierung. Zwei-Perioden-Modell

# Benötigte Pakete laden
library(shiny)
library(ggplot2)

# Definition der Benutzeroberfläche (UI)
ui <- fluidPage(
  # CSS für ein besseres Design hinzufügen
  tags$head(
    tags$style(HTML("
      .shiny-input-container { margin-bottom: 20px; }
      .well { background-color: #f8f9fa; border-color: #dee2e6;}
      h2, h4 { color: #343a40; }
    "))
  ),
  
  # Titel der Anwendung
  titlePanel(h2("Intertemporale Konsumoptimierung (Zwei-Perioden-Modell)", align = "center")),
  
  # Seitenleisten-Layout
  sidebarLayout(
    # Panel für die Seitenleiste mit den Eingabeelementen
    sidebarPanel(
      width = 3,
      h4("Parameter anpassen"),
      # Schieberegler für das Einkommen (I)
      sliderInput("X", "Grundausstattung (X)", min = 50, max = 200, value = 100, step = 10, pre = "€"),
      # Schieberegler für den Preis von Gut 1 (p1)
      sliderInput("g", "Wachstumsrate (g)", min = 0, max = .5, value = .01, step = 0.01),
      # Schieberegler für den Preis von Gut 2 (p2)
     # sliderInput("p2", "Preis von Gut 2 (p₂)", min = 0.1, max = 5, value = 0.5, step = 0.1, pre = "€"),
      # Schieberegler für Zeitpräferenz (i)
      sliderInput("i", "Präferenz (i)", min = 0, max = 0.95, value = .01, step = 0.01)
    ),
    
    # Hauptpanel zur Anzeige der Grafik
    mainPanel(
      width = 9,
      plotOutput("cobbDouglasPlot", height = "600px")
    )
  )
)

# Definition der Server-Logik
server <- function(input, output) {
  
  # Reaktiv die Berechnungen durchführen, wenn sich ein Input ändert
  reactive_calcs <- reactive({
    # Eingabewerte von den Schiebereglern holen
    X <- input$X
    g <- input$g
#    p2 <- input$p2
    i <- input$i
    
    # Berechnung des optimalen Konsumpunkts (Gleichgewicht)
    x0_eq <- X * ((1+i)/(2+i))
    x1_eq <- (X-x0_eq) * (1+g)
    
    # Berechnung des Nutzenniveaus im Gleichgewicht
    # Fehlerbehandlung für den Fall, dass x1_eq oder x2_eq null sind
    U_eq <- if (x0_eq > 0 && x1_eq > 0) {
      log(x0_eq) + log(x1_eq)/(1+i)
    } else {
      0
    }
    
    list(X = X, g = g, i = i, x0_eq = x0_eq, x1_eq = x1_eq, U_eq = U_eq)
  })
  
  # Die Grafik rendern
  output$cobbDouglasPlot <- renderPlot({
    
    # Hole die berechneten Werte
    calcs <- reactive_calcs()
    X <- calcs$X
    g <- calcs$g
    i <- calcs$i
    x0_eq <- calcs$x0_eq
    x1_eq <- calcs$x1_eq
    U_eq <- calcs$U_eq
    
    # Maximaler Konsum von Gut 1 (Schnittpunkt mit der x-Achse)
    x0_max <- X
    
    # Erstellen der Daten für die Kurven
    # Wir starten bei einem kleinen Wert, um Division durch Null zu vermeiden
    x0_vals <- seq(0.01, x0_max * 1.1, length.out = 200)
    
    # Daten für die Budgetgerade
    df_budget <- data.frame(
      x0 = seq(0, x0_max, length.out = 200),
      Kurve = "Budgetgerade"
    )
    df_budget$x1 <- X*(1+g) - (1+g) * df_budget$x0
    
    # Daten für die Indifferenzkurve
    df_indifferenz <- data.frame(
      x0 = x0_vals,
      Kurve = "Indifferenzkurve"
    )
    # Nur berechnen, wenn U_eq > 0 ist
    
    #######
    #% Gegebene Gleichung
    #U = \ln(x_1) + \ln(x_2) \cdot \frac{1}{1+i}
    #
    #% 1. \ln(x_1) subtrahieren
    #U - \ln(x_1) = \ln(x_2) \cdot \frac{1}{1+i}
  #  
  #  % 2. Mit (1+i) multiplizieren
  #  (U - \ln(x_1)) \cdot (1+i) = \ln(x_2)
  #  
  #  % 3. Exponentiieren (e^{\dots})
  #  e^{(U - \ln(x_1)) \cdot (1+i)} = e^{\ln(x_2)}
  #  
  #  % Endergebnis
  #  x_2 = e^{(U - \ln(x_1)) \cdot (1+i)}
  #  
  
  #  #######
    
    if (U_eq > 0) {
      df_indifferenz$x1 <- exp((U_eq - log(df_indifferenz$x0)) * (1 + calcs$i))
    } else {
      df_indifferenz$x1 <- NA
    }
    
    # Kombinieren der Daten
    df <- rbind(df_budget, df_indifferenz)
    # Entferne unmögliche (negative) Werte und Ausreißer
    df <- df[df$x1 >= 0 & df$x1 < X*2, ]
    
    # Erstellen der Grafik mit ggplot2
    ggplot(df, aes(x = x0, y = x1, color = Kurve)) +
      geom_line(na.rm = TRUE, linewidth = 1.2) +
      
      # Gleichgewichtspunkt und gestrichelte Linien hinzufügen
      annotate("point", x = x0_eq, y = x1_eq, color = "#212529", size = 4, shape = 8, stroke = 1.5) +
      annotate("segment", x = x0_eq, xend = x0_eq, y = 0, yend = x1_eq, linetype = "dashed", color = "black") +
      annotate("segment", x = 0, xend = x0_eq, y = x1_eq, yend = x1_eq, linetype = "dashed", color = "black") +
      
      # Skalen und Achsen anpassen
      scale_x_continuous(
        limits = c(0, 300), 
        expand = c(0, 0),
        breaks = unique(round(c(0, x0_eq, X), 1))
      ) +
      scale_y_continuous(
        limits = c(0, 300), 
        expand = c(0, 0),
        breaks = unique(round(c(0, x1_eq, X*1.5), 1))
      ) +
      scale_color_manual(values = c("Budgetgerade" = "red", "Indifferenzkurve" = "green")) +
      
      # Titel, Untertitel und Achsenbeschriftungen
      labs(
        title = "Haushaltsoptimum: Budgetgerade und Indifferenzkurve",
        #subtitle = paste0("Optimum bei x0 = ", round(x0_eq, 2), ", x1 = ", round(x1_eq, 2), " und U = ", round(U_eq, 2)),
        subtitle = bquote(
          "Optimum bei" ~ x[0] ~ "=" ~ .(round(x0_eq, 2)) ~ "," ~ x[1] ~ "=" ~ .(round(x1_eq, 2)) ~ "und U =" ~ .(round(U_eq, 2))
        ),
        x = expression(x[0]),
        y = expression(x[1]),
        color = ""
      ) +
      
      # Theme und Design anpassen
      theme_light(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray80")
      )
  }, res = 100) # Auflösung für eine schärfere Grafik erhöhen
}

# Die Shiny App erstellen und starten
shinyApp(ui = ui, server = server)
