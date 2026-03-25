library(shiny)
library(ggplot2)

# --- 1. UI (Benutzeroberfläche) definieren ---
ui <- fluidPage(
  titlePanel("Interaktive Edgeworth-Box"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Präferenzen (Cobb-Douglas)"),
      sliderInput("alpha_A", expression(bold(paste("Alpha A (Gewichtung Gut ", x[1], "):"))), 
                  min = 0.05, max = 0.95, value = 0.3, step = 0.05),
      sliderInput("alpha_B", expression(bold(paste("Alpha B (Gewichtung Gut ", x[1], "):"))), 
                  min = 0.05, max = 0.95, value = 0.8, step = 0.05),
      hr(),
      h4("Anfangsausstattung (Person A)"),
      sliderInput("w1_A", expression(bold(paste("Ausstattung Gut ", x[1], " (A):"))), 
                  min = 0.5, max = 9.5, value = 8.0, step = 0.5),
      sliderInput("w2_A", expression(bold(paste("Ausstattung Gut ", x[2], " (A):"))), 
                  min = 0.5, max = 9.5, value = 2.0, step = 0.5),
      hr(),
      helpText("Hinweis: Die Gesamtmengen beider Güter im System betragen jeweils exakt 10 Einheiten. Die goldene Fläche markiert die Tauschlinse (Pareto-Verbesserungen).")
    ),
    
    mainPanel(
      # Quadratische Ausgabe, damit die Box proportional korrekt bleibt
      plotOutput("edgeworthPlot", height = "700px", width = "700px")
    )
  )
)

# --- 2. Server-Logik definieren ---
server <- function(input, output) {
  
  output$edgeworthPlot <- renderPlot({
    
    # 1. Parameter aus den Slidern einlesen
    X1_total <- 10  
    X2_total <- 10  
    
    alpha_A <- input$alpha_A  
    alpha_B <- input$alpha_B  
    w1_A <- input$w1_A
    w2_A <- input$w2_A
    
    # 2. Nutzenfunktionen reaktiv definieren
    U_A   <- function(x1, x2) { x1^alpha_A * x2^(1-alpha_A) }
    MRS_A <- function(x1, x2) { (alpha_A / (1-alpha_A)) * (x2 / x1) }
    
    U_B   <- function(x1, x2) { x1^alpha_B * x2^(1-alpha_B) }
    MRS_B <- function(x1, x2) { (alpha_B / (1-alpha_B)) * (x2 / x1) }
    
    # 3. Nutzen der Anfangsausstattung berechnen
    U_A_start <- U_A(w1_A, w2_A)
    U_B_start <- U_B(X1_total - w1_A, X2_total - w2_A)
    endow_df <- data.frame(x1_A = w1_A, x2_A = w2_A)
    
    # 4. Grid berechnen (200x200 für flüssige Shiny-Performance)
    grid <- expand.grid(x1_A = seq(0.01, X1_total - 0.01, length.out = 200),
                        x2_A = seq(0.01, X2_total - 0.01, length.out = 200))
    
    grid$Nutzen_A <- U_A(grid$x1_A, grid$x2_A)
    grid$Nutzen_B <- U_B(X1_total - grid$x1_A, X2_total - grid$x2_A)
    grid$Tauschlinse <- grid$Nutzen_A >= U_A_start & grid$Nutzen_B >= U_B_start
    
    # 5. Kontraktkurve berechnen
    x1_seq <- seq(0.01, X1_total - 0.01, length.out = 100)
    cc_x2 <- sapply(x1_seq, function(x1) {
      obj <- function(x2) { MRS_A(x1, x2) - MRS_B(X1_total - x1, X2_total - x2) }
      tryCatch(uniroot(obj, interval = c(1e-4, X2_total - 1e-4))$root, error = function(e) NA)
    })
    contract_df <- na.omit(data.frame(x1_A = c(0, x1_seq, X1_total), x2_A = c(0, cc_x2, X2_total)))
    
    # 6. Tangentialpunkte berechnen
    tangency_x1 <- 1:9
    tangency_x2 <- sapply(tangency_x1, function(x1) {
      obj <- function(x2) { MRS_A(x1, x2) - MRS_B(X1_total - x1, X2_total - x2) }
      tryCatch(uniroot(obj, interval = c(1e-4, X2_total - 1e-4))$root, error = function(e) NA)
    })
    tangency_df <- na.omit(data.frame(x1_A = tangency_x1, x2_A = tangency_x2))
    
    breaks_A <- U_A(tangency_df$x1_A, tangency_df$x2_A)
    breaks_B <- U_B(X1_total - tangency_df$x1_A, X2_total - tangency_df$x2_A)
    
    # 7. Plot generieren
    ggplot() +
      # Tauschlinse
      geom_raster(data = subset(grid, Tauschlinse == TRUE), 
                  aes(x = x1_A, y = x2_A), fill = "gold", alpha = 0.4) +
      
      # Hintergrund-Indifferenzkurven
      geom_contour(data = grid, aes(x = x1_A, y = x2_A, z = Nutzen_A), 
                   color = "steelblue", breaks = breaks_A, linewidth = 0.4, alpha = 0.4) +
      geom_contour(data = grid, aes(x = x1_A, y = x2_A, z = Nutzen_B), 
                   color = "firebrick", breaks = breaks_B, linewidth = 0.4, alpha = 0.4) +
      
      # Indifferenzkurven des Ausstattungspunktes (dick hervorgehoben)
      geom_contour(data = grid, aes(x = x1_A, y = x2_A, z = Nutzen_A), 
                   color = "darkblue", breaks = U_A_start, linewidth = 1.2) +
      geom_contour(data = grid, aes(x = x1_A, y = x2_A, z = Nutzen_B), 
                   color = "darkred", breaks = U_B_start, linewidth = 1.2) +
      
      # Kontraktkurve und Tangentialpunkte
      geom_line(data = contract_df, aes(x = x1_A, y = x2_A), color = "black", linewidth = 1) +
      geom_point(data = tangency_df, aes(x = x1_A, y = x2_A), color = "black", size = 2) +
      
      # Ausstattungspunkt (grüne Raute)
      geom_point(data = endow_df, aes(x = x1_A, y = x2_A), color = "forestgreen", size = 5, shape = 18) +
      
      # Skalierung und exakte quadratische Form erzwingen
      coord_fixed(ratio = 1, xlim = c(0, X1_total), ylim = c(0, X2_total), expand = FALSE) +
      scale_x_continuous(sec.axis = sec_axis(~ X1_total - ., name = expression(bold("Gut ") * bold(x[1]) * bold(" (Person B)")))) +
      scale_y_continuous(sec.axis = sec_axis(~ X2_total - ., name = expression(bold("Gut ") * bold(x[2]) * bold(" (Person B)")))) +
      
      # Beschriftung
      labs(x = expression(bold("Gut ") * bold(x[1]) * bold(" (Person A)")), 
           y = expression(bold("Gut ") * bold(x[2]) * bold(" (Person A)"))) +
      theme_minimal() +
      theme(
        axis.title.x.bottom = element_text(color = "steelblue", size = 12),
        axis.title.y.left = element_text(color = "steelblue", size = 12),
        axis.title.x.top = element_text(color = "firebrick", size = 12),
        axis.title.y.right = element_text(color = "firebrick", size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
      )
  })
}

# --- 3. App starten ---
shinyApp(ui = ui, server = server)