library(shiny)
library(dplyr)
library(ggplot2)

# UI-Definition
ui <- fluidPage(
  titlePanel("Interaktive Risikoanalyse: CRRA Nutzenfunktion"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("sigma", "Risikoaversion (sigma):", 
                  min = -1.5, max = 1.5, value = 1, step = 0.1),
      sliderInput("p", "Wahrscheinlichkeit p (für x1=2):", 
                  min = 0, max = 1, value = 0.5, step = 0.05),
      hr(),
      helpText("x1 ist auf 2 fixiert, x2 auf 18."),
      tags$div(
        style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px;",
        htmlOutput("stats")
      )
    ),
    
    mainPanel(
      plotOutput("utilityPlot", height = "600px")
    )
  )
)

# Server-Logik
server <- function(input, output) {
  
  # Hilfsfunktion für den Nutzen
  u_fun <- function(x, s) {
    if (s == 1) return(log(x))
    else return((x^(1 - s) - 1) / (1 - s))
  }
  
  output$utilityPlot <- renderPlot({
    # Parameter aus UI
    s <- input$sigma
    p <- input$p
    x1 <- 2
    x2 <- 18
    
    # 1. Berechnungen
    ew <- p * x1 + (1 - p) * x2
    en <- p * u_fun(x1, s) + (1 - p) * u_fun(x2, s)
    sae <- if(s == 1) exp(en) else ((en * (1 - s)) + 1)^(1 / (1 - s))
    rp <- ew - sae
    
    # 2. Daten für Kurve
    df <- data.frame(x = seq(0.1, 25, length.out = 500)) %>%
      mutate(u = case_when(
        s != 1 ~ (x^(1 - s) - 1) / (1 - s),
        s == 1 ~ log(x)
      ))
    
    # 3. Plot erstellen
    ggplot(df, aes(x = x, y = u)) +
      geom_line(color = "firebrick", lwd = 1.2) +
      # Sekante
      annotate("segment", x = x1, xend = x2, 
               y = u_fun(x1, s), yend = u_fun(x2, s), 
               color = "blue", linetype = "dashed", lwd = 1) +
      # Erwartungsnutzen Punkt
      annotate("point", x = ew, y = en, color = "blue", size = 4) +
      # Pfeil zum SÄ
      annotate("segment", x = sae, xend = ew, y = en, yend = en, 
               color = "darkgreen", arrow = arrow(length = unit(0.3, "cm"), ends = "first"), lwd = 1) +
      # Hilfslinien
      annotate("segment", x = sae, xend = sae, y = min(df$u), yend = en, 
               color = "darkgreen", linetype = "dotted") +
      annotate("segment", x = ew, xend = ew, y = min(df$u), yend = en, 
               color = "blue", linetype = "dotted") +
      # Risikoprämie
      annotate("segment", x = sae, xend = ew, y = min(df$u) + 0.1, yend = min(df$u) + 0.1, 
               color = "purple", arrow = arrow(length = unit(0.2, "cm"), ends = "both"), lwd = 1) +
      # Labels
      annotate("text", x = sae, y = min(df$u), label = paste("SÄ:", round(sae, 2)), color = "darkgreen") +
      annotate("text", x = ew, y = min(df$u), label = paste("E[x]:", round(ew, 2)), color = "blue") +
      theme_light(base_size = 15) +
      labs(title = paste("Risikoanalyse (sigma =", s, ", p =", p, ")"),
           subtitle = paste("Risikoprämie (Lila):", round(rp, 3)),
           x = "Auszahlung (x)", y = "Nutzen u(x)")
  })
  
  output$stats <- renderUI({
    # Erneute Berechnung für die Textanzeige
    s <- input$sigma
    p <- input$p
    ew <- p * 2 + (1 - p) * 18
    en <- p * u_fun(2, s) + (1 - p) * u_fun(18, s)
    sae <- if(s == 1) exp(en) else ((en * (1 - s)) + 1)^(1 / (1 - s))
    
    HTML(paste0(
      "<b>Ergebnisse:</b><br>",
      "Erwartungswert E[x]: ", round(ew, 2), "<br>",
      "Sicherheitsäquivalent: ", round(sae, 2), "<br>",
      "Risikoprämie: <b>", round(ew - sae, 3), "</b>"
    ))
  })
}

# App starten
shinyApp(ui = ui, server = server)