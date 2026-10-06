library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

# ── Data loading ───────────────────────────────────────────────────────────────

il_counties <- st_read("illinois_counties.geojson", quiet = TRUE)

fungi_data <- read_csv("fungi_by_county1.csv", show_col_types = FALSE)

# Fix county name typos so they match the GeoJSON
county_fixes <- c(
  "Adans"            = "Adams",
  "Carrol"           = "Carroll",
  "Johson"           = "Johnson",
  "La Sale"          = "Lasalle",
  "Mc lean"          = "Mclean",
  "mc lean"          = "Mclean",
  "mclean"           = "Mclean",
  "St. Clair County" = "St. Clair",
  "Willianson"       = "Williamson",
  "kane"             = "Kane"
)

fungi_data <- fungi_data %>%
  filter(!is.na(county), county != "") %>%
  mutate(county = recode(county, !!!county_fixes))

# ── Constants ──────────────────────────────────────────────────────────────────

disease_cols <- c(
  "Diaporthe_sp", "Alternaria_Alternata", "COLLETOTRICHUM",
  "FUSARIUM", "CLONOSTACHYS", "TRICHODERMA", "EPICOCCUM",
  "MACROPHOMINA", "MUCOR", "RED_CROWN_ROT"
)

disease_labels <- c(
  "All fungi"            = "ALL_FUNGI",
  "Diaporthe sp."        = "Diaporthe_sp",
  "Alternaria alternata" = "Alternaria_Alternata",
  "Colletotrichum"       = "COLLETOTRICHUM",
  "Fusarium"             = "FUSARIUM",
  "Clonostachys"         = "CLONOSTACHYS",
  "Trichoderma"          = "TRICHODERMA",
  "Epicoccum"            = "EPICOCCUM",
  "Macrophomina"         = "MACROPHOMINA",
  "Mucor"                = "MUCOR",
  "Red crown rot"        = "RED_CROWN_ROT"
)

# Map color options: disease/fungi + climate variables
map_color_labels <- c(
  disease_labels,
  "── Climate ──"           = "SEPARATOR",
  "Mean temperature (°F)"   = "CLIMATE_TEMP",
  "Total precipitation (mm)" = "CLIMATE_PRECIP"
)
# Remove the separator from actual choices (just a visual hint via optgroup workaround)
map_color_labels <- map_color_labels[map_color_labels != "SEPARATOR"]

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; }
    .well { background: #f8f9fa; border: none; box-shadow: none; }
    h2 { font-size: 1.4rem; font-weight: 600; margin-bottom: 4px; }
    .subtitle { color: #666; font-size: 0.85rem; margin-bottom: 16px; }
    .stat-box { background: white; border: 1px solid #e0e0e0; border-radius: 6px;
                padding: 10px 14px; margin-bottom: 8px; }
    .stat-label { font-size: 0.75rem; color: #888; text-transform: uppercase;
                  letter-spacing: 0.05em; }
    .stat-value { font-size: 1.4rem; font-weight: 600; color: #2c2c2c; }
    .section-title { font-size: 0.8rem; font-weight: 600; color: #555;
                     text-transform: uppercase; letter-spacing: 0.07em;
                     margin: 14px 0 6px; }
    select option[disabled] { color: #aaa; font-style: italic; }
  "))),
  
  titlePanel(
    div(
      h2("Illinois Soybean Stem Disease Map"),
      div("Fungal pathogen prevalence by county", class = "subtitle")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      div(class = "section-title", "Filters"),
      
      selectInput(
        "selected_disease",
        "Color map by",
        choices  = map_color_labels,
        selected = "ALL_FUNGI"
      ),
      
      selectInput(
        "selected_year",
        "Year",
        choices  = c("All years" = "All", sort(unique(fungi_data$year))),
        selected = "All"
      ),
      
      hr(),
      
      div(class = "section-title", "Summary"),
      
      div(class = "stat-box",
          div("Counties with data", class = "stat-label"),
          div(textOutput("stat_counties"),  class = "stat-value")
      ),
      div(class = "stat-box",
          div("Total samples", class = "stat-label"),
          div(textOutput("stat_samples"),   class = "stat-value")
      ),
      div(class = "stat-box",
          div("Mean prevalence (counties w/ data)", class = "stat-label"),
          div(textOutput("stat_prevalence"), class = "stat-value")
      ),
      
      hr(),
      
      div(class = "section-title", "Climate correlation"),
      
      radioButtons(
        "climate_var",
        NULL,
        choices  = c("Temperature (°F)" = "mean_temp",
                     "Precipitation (mm)" = "mean_precip"),
        selected = "mean_temp",
        inline   = TRUE
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = "560px"),
      br(),
      plotOutput("scatter", height = "280px")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # 1. Filter rows by year
  filtered_data <- reactive({
    data <- fungi_data
    if (input$selected_year != "All") {
      data <- data %>% filter(year == as.numeric(input$selected_year))
    }
    data
  })
  
  # 2. Summarise to county level
  county_summary <- reactive({
    filtered_data() %>%
      mutate(
        any_fungi = as.integer(
          rowSums(across(all_of(disease_cols), ~as.numeric(.x)), na.rm = TRUE) > 0
        )
      ) %>%
      group_by(county) %>%
      summarise(
        samples              = n(),
        mean_temp            = mean(season_temperature_f,         na.rm = TRUE),
        mean_precip          = mean(season_preciptation_total_mm,  na.rm = TRUE),
        all_fungi_prevalence = mean(any_fungi, na.rm = TRUE) * 100,
        across(
          all_of(disease_cols),
          list(
            positive   = ~sum(as.numeric(.x),  na.rm = TRUE),
            prevalence = ~mean(as.numeric(.x), na.rm = TRUE) * 100
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  })
  
  # 3. Join to spatial data
  map_data <- reactive({
    il_counties %>%
      left_join(county_summary(), by = c("name" = "county"))
  })
  
  # 4. Resolve which column + palette to use for map fill
  map_color_spec <- reactive({
    sel <- input$selected_disease
    if (sel == "CLIMATE_TEMP") {
      list(col = "mean_temp",   palette = "RdYlBu", reverse = TRUE,
           title = "Mean temp (°F)", is_climate = TRUE)
    } else if (sel == "CLIMATE_PRECIP") {
      list(col = "mean_precip", palette = "Blues",  reverse = FALSE,
           title = "Total precip (mm)", is_climate = TRUE)
    } else {
      col <- if (sel == "ALL_FUNGI") "all_fungi_prevalence" else paste0(sel, "_prevalence")
      label <- names(disease_labels)[disease_labels == sel]
      list(col = col, palette = "YlOrRd", reverse = FALSE,
           title = paste0(label, " prevalence (%)"), is_climate = FALSE)
    }
  })
  
  # 5. Sidebar stats (only meaningful for disease selection)
  output$stat_counties <- renderText({
    sum(!is.na(county_summary()$samples))
  })
  
  output$stat_samples <- renderText({
    formatC(sum(county_summary()$samples, na.rm = TRUE), format = "d", big.mark = ",")
  })
  
  output$stat_prevalence <- renderText({
    spec <- map_color_spec()
    if (spec$is_climate) return("—")
    vals <- county_summary()[[spec$col]]
    paste0(round(mean(vals, na.rm = TRUE), 1), "%")
  })
  
  # 6. Base map (rendered once)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -89.4, lat = 40.0, zoom = 7)
  })
  
  # 7. Update polygons + legend reactively
  observe({
    md   <- map_data()
    spec <- map_color_spec()
    vals <- md[[spec$col]]
    
    pal <- colorNumeric(
      palette  = spec$palette,
      domain   = vals,
      reverse  = spec$reverse,
      na.color = "#d0d0d0"
    )
    
    md_df <- st_drop_geometry(md)
    
    popups <- vapply(seq_len(nrow(md_df)), function(i) {
      row <- md_df[i, ]
      
      disease_lines <- character(0)
      for (d in disease_cols) {
        pos_val  <- as.numeric(row[[paste0(d, "_positive")]])
        prev_val <- as.numeric(row[[paste0(d, "_prevalence")]])
        if (!is.na(pos_val) && pos_val > 0) {
          disease_lines <- c(
            disease_lines,
            sprintf("<b>%s</b>: %d positives / %.1f%%", d, as.integer(pos_val), prev_val)
          )
        }
      }
      
      disease_text <- if (length(disease_lines) == 0) "No fungi detected" else
        paste(disease_lines, collapse = "<br>")
      
      fmt <- function(x, d = 1, sfx = "") {
        if (is.na(x)) "No data" else paste0(round(as.numeric(x), d), sfx)
      }
      
      paste0(
        "<b>", row[["name"]], " County</b><br>",
        "<b>Samples:</b> ",
        ifelse(is.na(row[["samples"]]), "No data", as.integer(row[["samples"]])), "<br>",
        "<b>Mean temperature:</b> ", fmt(row[["mean_temp"]], 1, " °F"), "<br>",
        "<b>Total precipitation:</b> ", fmt(row[["mean_precip"]], 1, " mm"), "<br><br>",
        "<b>Detected fungi / diseases:</b><br>", disease_text
      )
    }, character(1))
    
    leafletProxy("map", data = md) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor        = ~pal(vals),
        color            = "white",
        weight           = 1,
        fillOpacity      = 0.75,
        popup            = popups,
        highlightOptions = highlightOptions(
          weight = 3, color = "#333", bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = vals,
        title    = spec$title,
        position = "bottomright",
        na.label = "No data"
      )
  })
  
  # 8. Scatter plot: climate variable vs. disease prevalence
  output$scatter <- renderPlot({
    df   <- county_summary()
    spec <- map_color_spec()
    
    # Y-axis: selected disease prevalence (or all fungi if a climate var is shown on map)
    prev_col   <- if (spec$is_climate) "all_fungi_prevalence" else spec$col
    prev_label <- if (spec$is_climate) "All fungi prevalence (%)" else spec$title
    
    # X-axis: radio button choice
    clim_col   <- input$climate_var
    clim_label <- if (clim_col == "mean_temp") "Mean temperature (°F)" else "Total precipitation (mm)"
    
    plot_df <- df %>%
      select(county, x = all_of(clim_col), y = all_of(prev_col), samples) %>%
      filter(!is.na(x), !is.na(y))
    
    if (nrow(plot_df) < 3) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Not enough data", size = 5, color = "gray50") +
               theme_void())
    }
    
    # Pearson r for subtitle
    r_val <- cor(plot_df$x, plot_df$y, use = "complete.obs")
    r_lab <- sprintf("Pearson r = %.2f", r_val)
    
    # Highlight top-5 counties by sample count
    top5 <- plot_df %>% slice_max(samples, n = 5)
    
    ggplot(plot_df, aes(x = x, y = y)) +
      geom_smooth(method = "lm", se = TRUE, color = "#c0392b", fill = "#f1948a", alpha = 0.2,
                  linewidth = 0.8) +
      geom_point(aes(size = samples), color = "#2c7bb6", alpha = 0.7, shape = 16) +
      geom_label_repel(
        data          = top5,
        aes(label     = county),
        size          = 3,
        color         = "#333",
        box.padding   = 0.4,
        point.padding = 0.3,
        max.overlaps  = 10,
        label.size    = 0.2
      ) +
      scale_size_continuous(name = "Samples", range = c(2, 8), breaks = c(1, 5, 10)) +
      labs(
        title    = paste(clim_label, "vs.", prev_label),
        subtitle = r_lab,
        x        = clim_label,
        y        = prev_label
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "#666", size = 10),
        legend.position = "right"
      )
  })
}

shinyApp(ui, server)









