# Lade benötigte Pakete
library(shiny)
library(deSolve)
library(ggplot2)
library(pracma) # Für numerische Integration

# Die Benutzeroberfläche (UI) definiert das Layout und die Steuerelemente
ui <- fluidPage(
  
  # Responsive Design für Mobilgeräte
  tags$head(
    tags$style(HTML("
      /* Allgemeine Schriftart und Hintergrund */
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f8f9fa;
      }
      /* Stil für die Hauptüberschrift */
      .title-panel {
        background-color: #007bff;
        color: white;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      /* Stil für Slider-Container */
      .well {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
      }
      /* Plot-Container Stil */
      .plot-container {
        padding: 15px;
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      /* Ergebnisbox Stil */
      #result_output {
        font-size: 1.1em;
        font-weight: bold;
        color: #333;
        padding: 15px;
        border: 2px solid #28a745;
        border-radius: 8px;
        background-color: #e6ffed;
        text-align: center;
      }
    "))
  ),
  
  # Titel der Anwendung
  div(class = "title-panel", 
      h1("Dynamische Ressourcen-Ökonomie Simulation")
  ),
  
  # Sidebar mit Eingabesteuerungen
  sidebarLayout(
    sidebarPanel(
      # Slider für den initialen Ressourcenbestand X_0
      sliderInput("X0", 
                  "Initialer Ressourcenbestand X₀:",
                  min = 50, max = 200, value = 100, step = 1),
      
      # Slider für die Abzinsungsrate i
      # i=0 macht das Nutzenmodell U(t) und Uz(t) ungültig (wegen Division durch Null / log(0))
      sliderInput("i", 
                  "Abzinsungsrate i:",
                  min = 0, max = 1, value = 0.03, step = 0.005),
      
      # Slider für die Wachstumsrate r
      # r=0 macht das Steady-State Nutzenmodell Uz(t) ungültig (wegen log(0))
      sliderInput("r", 
                  "Wachstumsrate r:",
                  min = 0, max = 1, value = 0.06, step = 0.005),
      
      # Anmerkung zur Funktion
      p(HTML("<b>Modellannahme:</b> Optimaler Entnahmepfad x(t) = X₀ ⋅ i ⋅ e⁽ʳ⁻ⁱ⁾ᵗ"))
    ),
    
    # Hauptbereich für Plots und Ergebnisse
    mainPanel(
      # Ergebnis-Output für den Schnittpunkt
      div(id = "result_output", textOutput("root_time")),
      
      # Plot für Entnahmepfade
      div(class = "plot-container", plotOutput("plot_entnahme")),
      
      # Plot für Ressourcenentwicklung
      div(class = "plot-container", plotOutput("plot_ressource")),
      
      # Plot für Nutzenfunktionen
      div(class = "plot-container", plotOutput("plot_nutzen"))
    )
  )
)

# Der Server-Teil definiert die Logik
server <- function(input, output) {
  
  # Definiere reaktive Zeitachse
  time_axis <- reactive({
    seq(0, 100, length.out = 500)
  })
  
  # --- Reaktive Funktionen und Datenrahmen ---
  
  # Berechne alle Modelldaten reaktiv basierend auf den Inputs
  model_data <- reactive({
    
    # Lese Inputs
    X_0 <- input$X0
    i <- input$i
    r <- input$r
    time <- time_axis()
    
    # Toleranz für den Nullvergleich (numerische Stabilität)
    tol <- 1e-6
    
    # 1. Optimaler Entnahmepfad: x(t) = X₀ * i * exp((r - i) * t)
    # Gilt auch, wenn i=0 oder r=0.
    x_t <- function(t) {
      X_0 * i * exp((r - i) * t)
    }
    
    # 6. Differentialgleichung für Ressourcenbestand
    # dX/dt = r * X - x(t) (Gilt auch, wenn i=0 oder r=0)
    dXdt <- function(t, X, params) {
      dX_dt <- r * X[1] - x_t(t)
      return(list(dX_dt))
    }
    
    # 7. Lösen der ODE (Numerische Integration des Ressourcenbestands)
    X_values_ode <- ode(y = c(X = X_0), times = time, func = dXdt, parms = NULL)
    X_values <- X_values_ode[, "X"]
    
    # --- Nutzenfunktionen (Konditionelle Berechnung) ---
    
    nutzen_U <- NULL
    nutzen_Uz <- NULL
    U_t_func <- NULL
    Uz_t_func <- NULL
    
    # U(t) ist nur definiert, wenn i > 0 (da sonst x(t)=0 und log(0))
    if (i > tol) { 
      # 3. Integrand für U(t): log(x(tau)) * exp(-i * tau)
      integrand_U <- function(tau) {
        log(x_t(tau)) * exp(-i * tau)
      }
      
      # 4. Nutzenfunktion U(t) (Numerische Integration)
      U_t_func <- function(t) {
        sapply(t, function(t_val) {
          if (t_val == 0) return(0)
          # quad ist stabil für die numerische Integration
          result <- pracma::quad(integrand_U, 0, t_val) 
          return(as.numeric(result))
        })
      }
      nutzen_U <- U_t_func(time)
    }
    
    # Uz(t) ist nur definiert, wenn i > 0 (Div. durch Null) UND r > 0 (log(0))
    if (i > tol && r > tol) { 
      # 5. Steady-State-Nutzen Uz(t) (Analytisch)
      Uz_t_func <- function(t) {
        log(X_0 * r) * (1 / i) * (1 - exp(-i * t))
      }
      nutzen_Uz <- Uz_t_func(time)
    }
    
    # --- DataFrames für Plots erstellen ---
    
    # 1. DataFrame für Entnahmepfade (x(t) ist immer drin)
    df_entnahme <- data.frame(
      Zeit = time,
      Wert = x_t(time),
      Kurve = factor(rep("x(t) - Optimal", length(time)))
    )
    
    # Füge z(t) nur hinzu, wenn r > 0 (Steady-State ist ökonomisch nur dann relevant)
    if (r > tol) {
      z_t_values <- rep(X_0 * r, length(time))
      df_entnahme <- rbind(df_entnahme, data.frame(
        Zeit = time,
        Wert = z_t_values,
        Kurve = factor(rep("z(t) - Steady-State", length(time)))
      ))
    }
    
    # 2. DataFrame für Nutzenfunktionen (Nur wenn U(t) definiert ist)
    df_nutzen <- NULL
    if (!is.null(nutzen_U)) {
      df_nutzen <- data.frame(
        Zeit = time,
        Nutzen = nutzen_U,
        Art = factor(rep("U(t) - Optimaler Pfad", length(time)))
      )
      
      # Füge Uz(t) nur hinzu, wenn es definiert ist
      if (!is.null(nutzen_Uz)) {
        df_nutzen <- rbind(df_nutzen, data.frame(
          Zeit = time,
          Nutzen = nutzen_Uz,
          Art = factor(rep("Uz(t) - Steady-State Pfad", length(time)))
        ))
      }
    }
    
    # 3. DataFrame für Ressourcenentwicklung
    df_ressource <- data.frame(
      Zeit = time,
      Bestand = X_values
    )
    
    # Rückgabe aller berechneten DataFrames und Funktionen
    return(list(
      df_entnahme = df_entnahme,
      df_nutzen = df_nutzen,
      df_ressource = df_ressource,
      U_t = U_t_func,
      Uz_t = Uz_t_func,
      i = i,
      r = r,
      tol = tol
    ))
  })
  
  # --- Nullstellensuche und Ergebnis-Output ---
  
  output$root_time <- renderText({
    data <- model_data()
    
    # 1. Fall: Nutzenfunktionen sind nicht definiert
    if (data$i < data$tol || data$r < data$tol) {
      msg <- "Nutzenvergleich inaktiv: "
      if (data$i < data$tol) msg <- paste0(msg, "i ≈ 0 (Optimal- und Steady-State-Nutzen U(t) nicht definiert). ")
      if (data$r < data$tol && data$i > data$tol) msg <- paste0(msg, "r ≈ 0 (Steady-State-Nutzen Uz(t) nicht definiert). ")
      return(msg)
    }
    
    # 2. Fall: r ≈ i (Nutzenfunktionen sind identisch)
    if (abs(data$r - data$i) < data$tol) {
      return("Spezialfall (r ≈ i): Optimaler und Steady-State-Nutzen sind identisch (U(t) = Uz(t)).")
    }
    
    # 3. Fall: uniroot-Suche (i > 0 und r > 0)
    U_t_func <- data$U_t
    Uz_t_func <- data$Uz_t
    
    f_diff <- function(t) {
      U_t_func(t) - Uz_t_func(t)
    }
    
    tryCatch({
      t_root_result <- uniroot(f_diff, interval = c(0.01, 100))
      
      if (t_root_result$root > 0.01 && t_root_result$root < 100) {
        sprintf("Schnittpunkt (U(t) = Uz(t)) bei t = %.2f", t_root_result$root)
      } else {
        "Kein Schnittpunkt im Intervall [0.01, 100] gefunden."
      }
    }, error = function(e) {
      if (data$r < data$i) {
        return("Spezialfall (r < i): Der Optimalpfad x(t) führt immer zu einem höheren Nutzen.")
      }
      return("Fehler bei der Nullstellensuche (Wahrscheinlich kein Schnittpunkt im Intervall).")
    })
  })
  
  # --- PLOT 1: Entnahmepfade ---
  output$plot_entnahme <- renderPlot({
    data <- model_data()
    
    # Farben werden dynamisch zugewiesen
    colors <- c("x(t) - Optimal" = "#007bff", "z(t) - Steady-State" = "#dc3545")
    
    ggplot(data$df_entnahme, aes(x = Zeit, y = Wert, color = Kurve)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors[names(colors) %in% levels(data$df_entnahme$Kurve)]) +
      labs(
        title = 'Vergleich der Entnahmepfade x(t) und z(t)',
        x = 'Zeit t',
        y = 'Entnahmerate',
        color = 'Pfad'
      ) +
      theme_light() +
      theme(legend.position = 'bottom',
            plot.title = element_text(face = "bold"))
  })
  
  # --- PLOT 2: Ressourcenentwicklung ---
  output$plot_ressource <- renderPlot({
    data <- model_data()
    
    # Ermittle Maximum für Y-Achsenbegrenzung
    max_X <- max(data$df_ressource$Bestand, input$X0)
    min_X <- min(0, min(data$df_ressource$Bestand))
    
    ggplot(data$df_ressource, aes(x = Zeit, y = Bestand)) +
      geom_line(color = '#28a745', linewidth = 1.2) +
      labs(
        title = 'Entwicklung des Ressourcenbestands X(t)',
        x = 'Zeit t',
        y = 'Ressourcenbestand X(t)'
      ) +
      # Setze die y-Achse dynamisch (wichtig für negative Bestände bei r < 0)
      coord_cartesian(ylim = c(min_X * 1.1, max_X * 1.1)) + 
      theme_light() +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # --- PLOT 3: Nutzenfunktionen ---
  output$plot_nutzen <- renderPlot({
    data <- model_data()
    
    if (is.null(data$df_nutzen)) {
      # Zeige Platzhalter, wenn Nutzenfunktionen nicht definiert sind
      p <- ggplot() + 
        labs(title = "Abgezinster Gesamtnutzen U(t) vs. Uz(t)",
             x = "Zeit t", y = "Gesamtnutzen") +
        geom_text(aes(x = 50, y = 0, label = "Nutzenfunktionen sind für i=0 oder r=0 nicht definiert."), 
                  size = 5, color = "darkred") +
        theme_light() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank())
      return(p)
    }
    
    colors <- c("U(t) - Optimaler Pfad" = "#007bff", "Uz(t) - Steady-State Pfad" = "#dc3545")
    
    ggplot(data$df_nutzen, aes(x = Zeit, y = Nutzen, color = Art)) +
      geom_line(linewidth = 1.2) +
      # Skaliere nur die vorhandenen Farben
      scale_color_manual(values = colors[names(colors) %in% levels(data$df_nutzen$Art)]) +
      labs(
        title = 'Abgezinster Gesamtnutzen U(t) vs. Uz(t)',
        x = 'Zeit t',
        y = 'Gesamtnutzen',
        color = 'Nutzenpfad'
      ) +
      theme_light() +
      theme(legend.position = 'bottom',
            plot.title = element_text(face = "bold"))
  })
}

# Starte die App
shinyApp(ui = ui, server = server)