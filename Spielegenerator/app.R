library(shiny)
library(ggplot2)

# --- UI (Benutzeroberfläche) ---
ui <- fluidPage(
  # Laden des Bootstrap-Themas für die "alert" Meldungen
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")
  ),
  
  titlePanel("Spielegenerator (2x2 Matrix)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Spiel generieren"),
      # Button zum Generieren der Zufallszahlen (Setzt NE-Anzeige zurück)
      actionButton("randomize_button", "Zufällige Werte zuweisen"),
      
      hr(), # Trennlinie
      
      h4("2. Lösung anzeigen"),
      # NEUER BUTTON: Zeigt das Nash-Gleichgewicht an
      actionButton("show_nash_button", "Nash-Gleichgewicht anzeigen"),
      
      br(),
      br(),
      p(em("Zuerst Werte generieren. Die Hervorhebung und die Textausgabe erscheinen erst nach Klick auf 'Anzeigen'."))
    ),
    
    mainPanel(
      h3("Auszahlungsmatrix (Hervorhebung = Nash-GG)"),
      # Hinzufügen des Plot-Outputs für die Matrix
      plotOutput("payoff_matrix_plot"),
      
      h3("Nash-Gleichgewicht (Reine Strategien)"),
      # Ausgabe des berechneten Nash-Gleichgewichts (Verwendet Bootstrap-Stile)
      htmlOutput("nash_output") 
    )
  )
)

# --- Server-Logik ---
server <- function(input, output, session) {
  
  # Ein reaktiver Wert, um die Parameter zu speichern
  # A, B, C, D = Spieler 1 (Zeile), a, b, c, d = Spieler 2 (Spalte)
  parameter_values <- reactiveVal(
    list(A = 0, B = 0, C = 0, D = 0, a = 0, b = 0, c = 0, d = 0)
  )
  
  # REAKTIVE VARIABLE: Steuert, ob das NE angezeigt wird (initial FALSE)
  show_nash_state <- reactiveVal(FALSE)
  
  # Beobachtet Klicks auf den "randomize_button" und setzt neue Zufallswerte
  observeEvent(input$randomize_button, {
    
    # Generiert 8 zufällige Ganzzahlen zwischen 0 und 9
    new_values <- sample(0:9, size = 8, replace = TRUE)
    
    # Aktualisiert die Parameterliste mit den neuen Werten
    new_parameter_list <- list(
      A = new_values[1], # P1 (O, L)
      B = new_values[2], # P1 (U, L)
      C = new_values[3], # P1 (O, R)
      D = new_values[4], # P1 (U, R)
      a = new_values[5], # P2 (O, L)
      b = new_values[6], # P2 (U, L)
      c = new_values[7], # P2 (O, R)
      d = new_values[8]  # P2 (U, R)
    )
    
    # Setzt den reaktiven Wert
    parameter_values(new_parameter_list)
    
    # ZURÜCKSETZEN: Blendet die Lösung wieder aus
    show_nash_state(FALSE) 
  })
  
  # OBSERVER: Beobachtet den "Anzeigen"-Button und setzt den Status auf TRUE
  observeEvent(input$show_nash_button, {
    show_nash_state(TRUE)
  })
  
  # -----------------------------------------------------
  # REAKTIVE BERECHNUNG DES NASH-GLEICHGEWICHTS (NE)
  # P1: A, B, C, D (Zeile)
  # P2: a, b, c, d (Spalte)
  # -----------------------------------------------------
  nash_results <- reactive({
    p <- parameter_values()
    
    # 1. Prüfen, ob die Lösung angezeigt werden soll
    if (!show_nash_state()) {
      return(list(
        is_visible = FALSE,
        text_output = "Drücken Sie 'Nash-Gleichgewicht anzeigen' für die Lösung.",
        ne_cells = character(0) # Leerer Vektor für keine Hervorhebung
      ))
    }
    
    # 2. Berechnung durchführen (wenn sichtbar)
    NE_cells <- character(0)
    NE_text_list <- character(0)
    
    # NE-Check 1: (Oben, Links) -> Auszahlungen (A, a)
    # P1 (Row) BR zu L: A >= B? UND P2 (Col) BR zu O: a >= c?
    if (p$A >= p$B && p$a >= p$c) {
      NE_cells <- c(NE_cells, "OL")
      NE_text_list <- c(NE_text_list, paste0("O, L (", p$A, ", ", p$a, ")"))
    }
    
    # NE-Check 2: (Unten, Links) -> Auszahlungen (B, b)
    # P1 (Row) BR zu L: B >= A? UND P2 (Col) BR zu U: b >= d?
    if (p$B >= p$A && p$b >= p$d) {
      NE_cells <- c(NE_cells, "UL")
      NE_text_list <- c(NE_text_list, paste0("U, L (", p$B, ", ", p$b, ")"))
    }
    
    # NE-Check 3: (Oben, Rechts) -> Auszahlungen (C, c)
    # P1 (Row) BR zu R: C >= D? UND P2 (Col) BR zu O: c >= a?
    if (p$C >= p$D && p$c >= p$a) {
      NE_cells <- c(NE_cells, "OR")
      NE_text_list <- c(NE_text_list, paste0("O, R (", p$C, ", ", p$c, ")"))
    }
    
    # NE-Check 4: (Unten, Rechts) -> Auszahlungen (D, d)
    # P1 (Row) BR zu R: D >= C? UND P2 (Col) BR zu U: d >= b?
    if (p$D >= p$C && p$d >= p$b) {
      NE_cells <- c(NE_cells, "UR")
      NE_text_list <- c(NE_text_list, paste0("U, R (", p$D, ", ", p$d, ")"))
    }
    
    # 3. Ergebnis formatieren
    if (length(NE_cells) == 0) {
      text_output <- "Kein reines Nash-Gleichgewicht gefunden."
    } else {
      text_output <- paste(NE_text_list, collapse = "<br>")
    }
    
    return(list(
      is_visible = TRUE,
      text_output = text_output,
      ne_cells = NE_cells
    ))
  })
  
  # -----------------------------------------------------
  # 3. Rendert die NE-Ausgabe als HTML/Bootstrap-Meldung
  # -----------------------------------------------------
  output$nash_output <- renderUI({
    results <- nash_results()
    result_text <- results$text_output
    
    if (!results$is_visible) {
      # Zeigt den Hinweis, dass der Button gedrückt werden muss
      div(class = "alert alert-info", HTML(paste0("<strong>Lösung:</strong> ", result_text)))
    } else if (grepl("Kein", result_text)) {
      # Bootstrap-Klasse: alert-warning (Gelb)
      div(class = "alert alert-warning", HTML(paste0("<strong>Ergebnis:</strong> ", result_text)))
    } else {
      # Bootstrap-Klasse: alert-success (Grün)
      div(class = "alert alert-success", HTML(paste0("<strong>Reines Nash-Gleichgewicht(e) gefunden (P1, P2):</strong><br>", result_text)))
    }
  })
  
  # -----------------------------------------------------
  # 2. Rendert den ggplot für die Auszahlungsmatrix (mit bedingter NE-Hervorhebung)
  # -----------------------------------------------------
  output$payoff_matrix_plot <- renderPlot({
    
    p <- parameter_values()
    results <- nash_results()
    
    # Datenrahmen für die Hervorhebung (alle 4 Zellen)
    ne_coords <- data.frame(
      # xmin, xmax, ymin, ymax definiert die Begrenzung jeder Zelle
      xmin = c(-2, -2, 0, 0),
      xmax = c(0, 0, 2, 2),
      ymin = c(0, -3, 0, -3),
      ymax = c(3, 0, 3, 0),
      # ID für die einfache Zuordnung zu den NE-Ergebnissen
      cell_id = c("OL", "UL", "OR", "UR") 
    )
    
    # Filtert nur die Zellen, die ein NE sind UND sichtbar sein sollen
    ne_highlights <- if (results$is_visible) {
      ne_coords[ne_coords$cell_id %in% results$ne_cells, ]
    } else {
      ne_coords[FALSE, ] # Leerer DataFrame, wenn nicht sichtbar
    }
    
    # Definiert die Hervorhebung als Layer oder NULL, wenn keine Hervorhebung nötig ist.
    highlight_layer <- if (nrow(ne_highlights) > 0) {
      geom_rect(
        data = ne_highlights,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#d4edda", # Leuchtgrüner Hintergrund (light green)
        alpha = 0.6
      )
    } else {
      NULL
    }
    
    # Erstellt den Plot und fügt den optionalen Layer hinzu
    gg <- ggplot() +
      highlight_layer + # Fügt NULL oder den geom_rect Layer hinzu
      
      # Zelle (oben, links) - Auszahlungen (P1: A, P2: a)
      # P1 (A, Zeile, Blau): UNTEN LINKS
      geom_text(aes(x = -1.5, y = 0.75, label = p$A), size = 6, fontface = "bold", color = "#1a5276") + 
      # P2 (a, Spalte, Rot): OBEN RECHTS
      geom_text(aes(x = -0.5, y = 2, label = p$a), size = 6, fontface = "bold", color = "#b03a2e") + 
      
      # Zelle (unten, links) - Auszahlungen (P1: B, P2: b)
      # P1 (B): UNTEN LINKS
      geom_text(aes(x = -1.5, y = -2.25, label = p$B), size = 6, fontface = "bold", color = "#1a5276") +
      # P2 (b): OBEN RECHTS
      geom_text(aes(x = -0.5, y = -1, label = p$b), size = 6, fontface = "bold", color = "#b03a2e") +
      
      # Zelle (oben, rechts) - Auszahlungen (P1: C, P2: c)
      # P1 (C): UNTEN LINKS
      geom_text(aes(x = 0.5, y = 0.75, label = p$C), size = 6, fontface = "bold", color = "#1a5276") +
      # P2 (c): OBEN RECHTS
      geom_text(aes(x = 1.5, y = 2, label = p$c), size = 6, fontface = "bold", color = "#b03a2e") +
      
      # Zelle (unten, rechts) - Auszahlungen (P1: D, P2: d)
      # P1 (D): UNTEN LINKS
      geom_text(aes(x = 0.5, y = -2.25, label = p$D), size = 6, fontface = "bold", color = "#1a5276") +
      # P2 (d): OBEN RECHTS
      geom_text(aes(x = 1.5, y = -1, label = p$d), size = 6, fontface = "bold", color = "#b03a2e") +
      
      # Beschriftungen der Strategien
      geom_text(aes(x = -1, y = 3.3, label = 'links (L)'), size = 5, color = "#b03a2e") +
      geom_text(aes(x = 1, y = 3.3, label = 'rechts (R)'), size = 5, color = "#b03a2e") +
      geom_text(aes(x = -2.2, y = 1.375, label = 'oben (O)'), angle = 90, size = 5, color = "#1a5276") +
      geom_text(aes(x = -2.2, y = -1.625, label = 'unten (U)'), angle = 90, size = 5, color = "#1a5276") +
      
      # Spieler-Titel (Habe die Buchstaben in den Klammern angepasst)
      geom_text(aes(x = 0, y = 4, label = 'Spieler 2 (Spalten, {a,b,c,d})'), fontface = "bold", size = 6, color = "#b03a2e") + 
      geom_text(aes(x = -2.7, y = 0, label = 'Spieler 1 (Zeilen, {A,B,C,D})'), angle = 90, fontface = "bold", size = 6, color = "#1a5276") + 
      
      # Matrix-Linien 
      geom_segment(aes(x = -2, xend = 2, y = 0, yend = 0), linewidth = 1) + 
      geom_segment(aes(x = -2, xend = 2, y = 3, yend = 3), linewidth = 1) + 
      geom_segment(aes(x = -2, xend = 2, y = -3, yend = -3), linewidth = 1) + 
      geom_segment(aes(x = -2, xend = -2, y = -3, yend = 3), linewidth = 1) + 
      geom_segment(aes(x = 0, xend = 0, y = -3, yend = 3), linewidth = 1) + 
      geom_segment(aes(x = 2, xend = 2, y = -3, yend = 3), linewidth = 1) + 
      
      # Styling
      theme_void() +
      coord_fixed(xlim = c(-3, 2.5), ylim = c(-3.5, 4.5)) +
      labs(title = "Auszahlungen in einem simultanen 2x2 Spiel") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.margin = margin(20, 20, 20, 20, "pt")
      )
    
    return(gg)
  })
  
  # Die Textausgabe der Parameterwerte wird hier nur für den Fall einer späteren Nutzung beibehalten.
  # WICHTIG: Die Ausgabe des Textes wurde angepasst, um die neue Zuordnung zu reflektieren.
  output$parameter_output <- renderPrint({
    p <- parameter_values()
    cat("Großbuchstaben (Spieler 1, Zeile):\n")
    cat(sprintf("A: %d, B: %d, C: %d, D: %d\n", p$A, p$B, p$C, p$D))
    cat("\nKleinbuchstaben (Spieler 2, Spalte):\n")
    cat(sprintf("a: %d, b: %d, c: %d, d: %d\n", p$a, p$b, p$c, p$d))
  })
  
}

# --- App starten ---
shinyApp(ui = ui, server = server)