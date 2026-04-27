library(shiny)
library(igraph)
library(ggplot2)

# ── Preferential Attachment: agrega un nodo con m enlaces ──────────────────
pa_step <- function(g, m) {
  n <- vcount(g)
  if (n == 0) return(add_vertices(g, 1))

  degs  <- degree(g)
  total <- sum(degs)
  probs <- if (total == 0) rep(1/n, n) else degs / total

  m_real  <- min(m, n)
  targets <- sample(seq_len(n), size = m_real, prob = probs, replace = FALSE)

  g <- add_vertices(g, 1)
  new_id <- vcount(g)
  edge_list <- as.vector(rbind(rep(new_id, m_real), targets))
  add_edges(g, edge_list)
}

# ── Paleta grado (azul → rojo) ─────────────────────────────────────────────
deg_color <- function(degs) {
  pal <- colorRampPalette(c("#4fc3f7", "#ab47bc", "#ef5350"))(max(degs) + 1)
  pal[degs + 1]
}

# ── UI ────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#0d1117; color:#e6edf3; font-family: 'Segoe UI', sans-serif; }
    .well { background:#161b22; border:1px solid #30363d; }
    h2 { color:#58a6ff; font-weight:700; margin-bottom:6px; }
    .sub { color:#8b949e; font-size:13px; margin-bottom:18px; }
    .stat-box { background:#21262d; border-radius:8px; padding:10px 16px;
                margin-bottom:8px; font-size:13px; color:#c9d1d9; }
    .stat-box span { color:#58a6ff; font-weight:bold; font-size:16px; float:right; }
    #toggle { width:100%; font-size:15px; font-weight:600; }
    #reset  { width:100%; font-size:13px; margin-top:6px; }
    .progress-bar-label { color:#8b949e; font-size:11px; margin-top:12px; }
  "))),

  titlePanel(
    div(
      h2("Red de Preferential Attachment"),
      div("Modelo Barabási–Albert — ley de potencia emergente", class = "sub")
    )
  ),

  sidebarLayout(
    sidebarPanel(width = 3,

      # m selector
      tags$label("Nuevos enlaces por nodo (m)", style="color:#8b949e;font-size:12px;"),
      radioButtons("m", label = NULL,
                   choices  = c("1" = 1, "2" = 2, "3" = 3),
                   selected = 2, inline = TRUE),
      hr(style="border-color:#30363d;"),

      # Controles
      actionButton("toggle", "▶  Play",
                   class = "btn btn-success btn-lg"),
      actionButton("reset",  "↺  Reiniciar",
                   class = "btn btn-warning btn-sm"),

      hr(style="border-color:#30363d;"),

      sliderInput("speed", "Velocidad (ms / paso)",
                  min = 80, max = 1500, value = 400, step = 20,
                  ticks = FALSE),

      sliderInput("max_n", "Máx. nodos",
                  min = 20, max = 300, value = 120, step = 10,
                  ticks = FALSE),

      hr(style="border-color:#30363d;"),

      # Estadísticas
      uiOutput("stats_ui"),

      div(class="progress-bar-label", "Progreso de la red"),
      uiOutput("progress_ui")
    ),

    mainPanel(width = 9,
      fluidRow(
        column(12,
          div(style="background:#161b22;border-radius:10px;padding:6px;",
            plotOutput("netPlot", height = "360px"))
        )
      ),
      br(),
      fluidRow(
        column(6,
          div(style="background:#161b22;border-radius:10px;padding:6px;",
            plotOutput("histPlot", height = "240px"))
        ),
        column(6,
          div(style="background:#161b22;border-radius:10px;padding:6px;",
            plotOutput("loglogPlot", height = "240px"))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    g       = {g <- make_empty_graph(2, directed=FALSE); add_edges(g, c(1,2))},
    layout  = NULL,
    running = FALSE
  )

  # ── Play / Pause ────────────────────────────────────────────────────────
  observeEvent(input$toggle, {
    rv$running <- !rv$running
    label <- if (rv$running) "⏸  Pause" else "▶  Play"
    updateActionButton(session, "toggle", label = label)
  })

  # ── Reset ───────────────────────────────────────────────────────────────
  observeEvent(input$reset, {
    rv$running <- FALSE
    g <- make_empty_graph(2, directed = FALSE)
    rv$g      <- add_edges(g, c(1, 2))
    rv$layout <- NULL
    updateActionButton(session, "toggle", label = "▶  Play")
  })

  # ── Animación principal ─────────────────────────────────────────────────
  observe({
    invalidateLater(input$speed)
    isolate({
      if (!rv$running) return()
      if (vcount(rv$g) >= input$max_n) {
        rv$running <- FALSE
        updateActionButton(session, "toggle", label = "▶  Play")
        return()
      }
      m_val <- as.integer(input$m)
      rv$g  <- pa_step(rv$g, m_val)

      # Layout incremental: fijar nodos viejos, posicionar nuevo
      n   <- vcount(rv$g)
      old_n <- n - 1
      if (is.null(rv$layout) || nrow(rv$layout) < 2) {
        rv$layout <- layout_with_fr(rv$g)
      } else {
        lay <- matrix(0, n, 2)
        lay[1:old_n, ] <- rv$layout
        nbrs <- as.integer(neighbors(rv$g, n))
        if (length(nbrs) > 0) {
          center <- colMeans(lay[nbrs, , drop = FALSE])
        } else {
          center <- c(0, 0)
        }
        lay[n, ] <- center + rnorm(2, 0, 0.15)
        rv$layout <- lay
      }
    })
  })

  # ── Red ─────────────────────────────────────────────────────────────────
  output$netPlot <- renderPlot({
    g    <- rv$g
    degs <- degree(g)
    lay  <- if (is.null(rv$layout)) layout_with_fr(g) else rv$layout

    vcols <- deg_color(degs)
    vsizes <- pmin(3 + degs * 1.8, 22)

    par(mar = c(0, 0, 0, 0), bg = "#161b22")
    plot(g,
         layout            = lay,
         vertex.color      = vcols,
         vertex.size       = vsizes,
         vertex.label      = NA,
         vertex.frame.color= NA,
         edge.color        = "#ffffff18",
         edge.width        = 0.6,
         rescale           = TRUE)
  }, bg = "#161b22")

  # ── Histograma ──────────────────────────────────────────────────────────
  output$histPlot <- renderPlot({
    g    <- rv$g
    degs <- degree(g)
    df   <- data.frame(k = degs)

    ggplot(df, aes(x = k)) +
      geom_histogram(binwidth = 1, fill = "#4fc3f7", color = "#0d1117",
                     alpha = 0.85) +
      labs(title = "Distribución de grado  P(k)",
           x = "Grado k", y = "Frecuencia") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#21262d"),
        panel.grid.minor = element_blank(),
        text             = element_text(color = "#c9d1d9"),
        axis.text        = element_text(color = "#8b949e"),
        plot.title       = element_text(color = "#58a6ff", size = 13,
                                        face = "bold")
      )
  }, bg = "#161b22")

  # ── Log-log ─────────────────────────────────────────────────────────────
  output$loglogPlot <- renderPlot({
    g <- rv$g
    if (vcount(g) < 8) {
      par(bg = "#161b22"); plot.new()
      text(0.5, 0.5, "Acumulando datos...", col = "#8b949e", cex = 1.2)
      return()
    }

    degs     <- degree(g)
    deg_tab  <- table(degs)
    df <- data.frame(
      k  = as.numeric(names(deg_tab)),
      pk = as.numeric(deg_tab) / sum(deg_tab)
    )
    df <- df[df$k > 0, ]

    ggplot(df, aes(x = k, y = pk)) +
      geom_point(color = "#ef5350", size = 2.8, alpha = 0.85) +
      geom_smooth(method = "lm", se = TRUE, color = "#ffa726",
                  fill = "#ffa72630", linewidth = 0.9, linetype = "dashed") +
      scale_x_log10() + scale_y_log10() +
      labs(title = "Log-log  —  Ley de potencia",
           x = "log k", y = "log P(k)") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#21262d"),
        panel.grid.minor = element_blank(),
        text             = element_text(color = "#c9d1d9"),
        axis.text        = element_text(color = "#8b949e"),
        plot.title       = element_text(color = "#58a6ff", size = 13,
                                        face = "bold")
      )
  }, bg = "#161b22")

  # ── Stats UI ─────────────────────────────────────────────────────────────
  output$stats_ui <- renderUI({
    g    <- rv$g
    degs <- degree(g)
    tagList(
      div(class="stat-box", "Nodos",   tags$span(vcount(g))),
      div(class="stat-box", "Aristas", tags$span(ecount(g))),
      div(class="stat-box", "Grado máximo",  tags$span(max(degs))),
      div(class="stat-box", "Grado promedio",
          tags$span(round(mean(degs), 1)))
    )
  })

  # ── Barra de progreso ────────────────────────────────────────────────────
  output$progress_ui <- renderUI({
    pct <- round(100 * vcount(rv$g) / input$max_n)
    div(
      style = "background:#21262d;border-radius:6px;height:8px;margin-top:4px;",
      div(style = sprintf(
        "width:%d%%;background:#238636;height:8px;border-radius:6px;transition:width .3s;", pct))
    )
  })
}

shinyApp(ui, server)
