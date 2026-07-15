# ─────────────────────────────────────────────────────────────────────────────
# KWRC Water Temperature Explorer
# Keystone Water Resources Center — Bucknell University
#
# Phase 1: Temperature data only.
# Dependencies: shiny, bslib, dplyr, readr, lubridate, plotly, DT
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(plotly)
library(DT)

# ── 1. Load & prepare data ───────────────────────────────────────────────────

site_meta  <- readRDS("site_meta.rds")
temp_data  <- readRDS("temp_data.rds")

site_meta  <- readRDS("site_meta.rds") |> filter(site_id != "GGU")
temp_data  <- readRDS("temp_data.rds") |> filter(site_id != "GGU")

n_sites <- site_meta |> filter(site_type == "Surface") |> nrow()

# Load watershed boundary coordinates safely — app still works if files missing
HAS_WATERSHEDS <- tryCatch({
  surface_ws_coords     <<- read.csv("www/surface_ws_coords.csv",
                                     na.strings = c("NA", ""))
  groundwater_ws_coords <<- read.csv("www/groundwater_ws_coords.csv",
                                     na.strings = c("NA", ""))
  TRUE
}, error = function(e) {
  warning("Watershed boundary files not found; maps will run without boundaries.")
  FALSE
})

# Helper: add watershed boundary traces to an existing plotly object.
add_watershed_traces <- function(p) {
  if (!HAS_WATERSHEDS) return(p)
  p |>
    add_trace(
      data          = surface_ws_coords,
      lat           = ~lat,
      lon           = ~lon,
      type          = "scattermapbox",
      mode          = "lines",
      line          = list(color = "#1a3a5c", width = 2),
      name          = "Surface watershed",
      hoverinfo     = "none",
      showlegend    = FALSE
    ) |>
    add_trace(
      data          = groundwater_ws_coords,
      lat           = ~lat,
      lon           = ~lon,
      type          = "scattermapbox",
      mode          = "lines",
      line          = list(color = "#2b8a9b", width = 1.5, dash = "dot"),
      name          = "Groundwater basin",
      hoverinfo     = "none",
      showlegend    = FALSE
    )
}

# Spring site IDs — used to set a higher stuck-value threshold.
SPRING_SITES <- c("AXS", "BES", "BIS", "BLS", "COS", "LIS", "WAS", "WIS")

# Helper: replace runs of >= run_threshold identical consecutive values with NA.
mask_stuck_values <- function(x, run_threshold = 7L) {
  if (all(is.na(x))) return(x)
  r           <- rle(x)
  rep_lengths <- rep(r$lengths, r$lengths)
  stuck       <- !is.na(x) & rep_lengths >= run_threshold
  x[stuck]    <- NA_real_
  x
}

# Helper: render a small sidebar map highlighting one or more selected sites.
mini_map <- function(selected_ids, site_meta, colors = NULL) {
  all_sites <- site_meta |> filter(!is.na(latitude))
  sel       <- all_sites |> filter(site_id %in% selected_ids)
  if (nrow(sel) == 0) return(plotly_empty())
  if (is.null(colors)) colors <- rep("#e05c28", nrow(sel))
  sel$dot_color <- colors[seq_len(nrow(sel))]
  
  p <- plot_ly() |>
    add_watershed_traces() |>
    add_trace(
      data          = all_sites |> filter(!site_id %in% selected_ids),
      lat           = ~latitude,
      lon           = ~longitude,
      type          = "scattermapbox",
      mode          = "markers",
      marker        = list(size = 7, color = "#adb5bd", opacity = 0.5),
      hoverinfo     = "none",
      showlegend    = FALSE
    )
  for (i in seq_len(nrow(sel))) {
    p <- p |> add_trace(
      lat           = sel$latitude[i],
      lon           = sel$longitude[i],
      type          = "scattermapbox",
      mode          = "markers",
      marker        = list(size = 13, color = sel$dot_color[i]),
      text          = sel$site_name[i],
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = FALSE
    )
  }
  ctr_lat <- mean(sel$latitude)
  ctr_lon <- mean(sel$longitude)
  zoom    <- if (length(selected_ids) == 1) 9 else 8
  
  p |>
    layout(
      mapbox        = list(
        style  = "open-street-map",
        center = list(lat = ctr_lat, lon = ctr_lon),
        zoom   = zoom
      ),
      margin        = list(l = 0, r = 0, t = 0, b = 0),
      paper_bgcolor = "transparent"
    ) |>
    config(displayModeBar = FALSE)
}

# Summary stats for overview boxes
n_sites        <- n_distinct(temp_data$site_id)
year_min       <- min(temp_data$year, na.rm = TRUE)
year_max       <- max(temp_data$year, na.rm = TRUE)
n_since_1999   <- temp_data |>
  filter(!is.na(daily_mean_c)) |>
  group_by(site_id) |>
  summarise(first_yr = min(year), .groups = "drop") |>
  filter(first_yr <= 1999) |>
  nrow()

# Global y-axis range — fixed across all plots so sites are visually comparable.
# Padded by 1°C on each end so data doesn't kiss the axis edges.
TEMP_YMIN <- floor(min(temp_data$daily_mean_c, temp_data$daily_max_c,
                       na.rm = TRUE)) - 1
TEMP_YMAX <- ceiling(max(temp_data$daily_mean_c, temp_data$daily_max_c,
                         na.rm = TRUE)) + 1

# Site choice lists grouped by type
make_site_choices <- function() {
  surface <- site_meta |>
    filter(site_id %in% temp_data$site_id, site_type == "Surface") |>
    arrange(site_name) |>
    pull(site_id) |>
    setNames(
      site_meta |>
        filter(site_id %in% temp_data$site_id, site_type == "Surface") |>
        arrange(site_name) |>
        pull(site_name)
    )
  springs <- site_meta |>
    filter(site_id %in% temp_data$site_id, site_type == "Spring") |>
    arrange(site_name) |>
    pull(site_id) |>
    setNames(
      site_meta |>
        filter(site_id %in% temp_data$site_id, site_type == "Spring") |>
        arrange(site_name) |>
        pull(site_name)
    )
  list(`Surface Sites` = surface, `Spring Sites` = springs)
}

SITE_CHOICES <- make_site_choices()
ALL_SITE_IDS <- unlist(SITE_CHOICES, use.names = FALSE)

CMP_CHOICES        <- unlist(SITE_CHOICES)
names(CMP_CHOICES) <- sub("^.*\\.", "", names(CMP_CHOICES))

LINE_COLORS <- c(
  "#1d7fb3", "#2a9d8f", "#e07040", "#c98b2d",
  "#d64e3c", "#6a4c93", "#52b788", "#8d99ae"
)

# ── 2. Theme ─────────────────────────────────────────────────────────────────

kwrc_theme <- bs_theme(
  version      = 5,
  primary      = "#1a3a5c",
  secondary    = "#2b8a9b",
  success      = "#52b788",
  danger       = "#e07040",
  bg           = "#f4f8fb",
  fg           = "#1a2633",
  base_font    = font_collection("system-ui", "-apple-system", "sans-serif"),
  heading_font = font_collection("Georgia", "serif"),
  "navbar-bg"  = "#1a3a5c",
  "card-border-radius" = ".5rem",
  "card-box-shadow"    = "0 1px 4px rgba(0,0,0,.08)"
)

# ── 3. Shared sidebar style ───────────────────────────────────────────────────

SB_BG <- "#e4f0f7"
SB_W  <- 270

sidebar_head <- function(label) {
  p(label, style = "font-size:.72rem; font-weight:700; letter-spacing:.06em;
     text-transform:uppercase; color:#5f7a90; margin:0 0 4px;")
}

# ── 4. UI ────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  theme        = kwrc_theme,
  window_title = "KWRC Temperature Explorer",
  
  title = tags$span(
    style = "font-family:Georgia,serif; font-size:1.05rem; letter-spacing:.01em;",
    "KWRC Water Temperature Explorer"
  ),
  
  # ── (a) Overview ───────────────────────────────────────────────────────────
  nav_panel(
    "Overview",
    icon = icon("gauge"),
    
    div(
      style = "max-width:1200px; margin:0 auto; padding:1.5rem 1rem;",
      
      div(
        style = "margin-bottom:1.5rem;",
        h2(
          "Spring Creek Watershed Temperature Monitoring",
          style = "font-family:Georgia,serif; color:#1a3a5c; margin-bottom:.25rem;"
        ),
        p(
          paste0("Continuous water temperature records collected by KWRC across ",
                 n_sites, " stations in Centre County, PA."),
          style = "color:#5f7a90; font-size:1.05rem;"
        )
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Temperature Monitoring Stations",
          value    = n_sites,
          theme    = value_box_theme(bg = "#1a3a5c", fg = "white"),
          showcase = icon("location-dot", style = "font-size:2rem;")
        ),
        value_box(
          title    = "Years of Record",
          value    = paste0(year_min, "\u2013", year_max),
          theme    = value_box_theme(bg = "#2b8a9b", fg = "white"),
          showcase = icon("calendar", style = "font-size:2rem;")
        ),
        value_box(
          title    = "Stations Monitored Since 1999",
          value    = paste0(n_since_1999, " stations"),
          theme    = value_box_theme(bg = "#c98b2d", fg = "white"),
          showcase = icon("clock", style = "font-size:2rem;")
        )
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        style = "margin-top:1rem;",
        
        card(
          card_header("Monitoring Network"),
          plotlyOutput("overview_map", height = "420px")
        ),
        
        card(
          card_header("Station Summary"),
          style = "overflow-y:auto;",
          DTOutput("overview_table", height = "390px")
        )
      )
    )
  ),
  
  # ── (b) Time Series ────────────────────────────────────────────────────────
  nav_panel(
    "Time Series",
    icon = icon("chart-line"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = SB_W, bg = SB_BG,
        
        sidebar_head("Station"),
        selectInput("ts_site", NULL, choices = SITE_CHOICES, selected = "SPA"),
        plotlyOutput("ts_minimap", height = "185px"),
        
        hr(style = "margin:.6rem 0;"),
        sidebar_head("Year Range"),
        sliderInput("ts_years", NULL,
                    min = year_min, max = year_max,
                    value = c(2015, year_max), step = 1, sep = ""),
        
        hr(style = "margin:.6rem 0;"),
        sidebar_head("Display"),
        checkboxInput("ts_show_max",   "Show daily maximum",     value = TRUE),
        checkboxInput("ts_show_flags", "Highlight flagged rows", value = FALSE),
        checkboxInput("ts_smooth",     "Add smoothed trend",     value = FALSE),
        
        hr(style = "margin:.6rem 0;"),
        downloadButton("ts_dl", "Download filtered data",
                       class = "btn-outline-primary btn-sm w-100")
      ),
      
      card(
        full_screen = TRUE,
        card_header(textOutput("ts_title", inline = TRUE)),
        plotlyOutput("ts_plot", height = "calc(100vh - 210px)")
      )
    )
  ),
  
  # ── (c) Seasonal Patterns ──────────────────────────────────────────────────
  nav_panel(
    "Seasonal Patterns",
    icon = icon("snowflake"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = SB_W, bg = SB_BG,
        
        sidebar_head("Station"),
        selectInput("sp_site", NULL, choices = SITE_CHOICES, selected = "SPA"),
        plotlyOutput("sp_minimap", height = "185px"),
        
        hr(style = "margin:.6rem 0;"),
        sidebar_head("Year Range"),
        sliderInput("sp_years", NULL,
                    min = year_min, max = year_max,
                    value = c(year_min, year_max), step = 1, sep = ""),
        
        hr(style = "margin:.6rem 0;"),
        sidebar_head("Reference Lines"),
        checkboxInput("sp_groundwater",
                      tags$span("Groundwater temperature ",
                                tags$small("(~10.5°C)")),
                      value = FALSE),
        checkboxInput("sp_coldwater",
                      tags$span("Cold-water fishery threshold ",
                                tags$small("(17°C)")),
                      value = FALSE),
        checkboxInput("sp_trout",
                      tags$span("PA trout stress threshold ",
                                tags$small("(20°C / 68°F)")),
                      value = TRUE),
        
        hr(style = "margin:.6rem 0;"),
        p("Box = IQR · Line = median · Dots = outliers",
          style = "font-size:.75rem; color:#5f7a90;")
      ),
      
      card(
        full_screen = TRUE,
        card_header(textOutput("sp_title", inline = TRUE)),
        plotlyOutput("sp_plot", height = "calc(100vh - 210px)")
      )
    )
  ),
  
  # ── (d) Compare Sites ──────────────────────────────────────────────────────
  nav_panel(
    "Compare Sites",
    icon = icon("code-compare"),
    
    layout_columns(
      col_widths = c(3, 6, 3),
      style = "padding:.75rem; height:calc(100vh - 75px); align-items:stretch;",
      
      card(
        style = paste0("background:", SB_BG, "; overflow-y:auto;"),
        card_body(
          padding = "0.75rem",
          
          sidebar_head("Station Location"),
          plotlyOutput("cmp_minimap", height = "185px"),
          
          hr(style = "margin:.6rem 0;"),
          sidebar_head("Year Range"),
          sliderInput("cmp_years", NULL,
                      min = year_min, max = year_max,
                      value = c(2010, year_max), step = 1, sep = ""),
          
          hr(style = "margin:.6rem 0;"),
          sidebar_head("Metric"),
          radioButtons("cmp_metric", NULL,
                       choices  = c("Daily mean" = "daily_mean_c",
                                    "Daily maximum" = "daily_max_c"),
                       selected = "daily_mean_c")
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Monthly Median Temperature by Station"),
        card_body(
          padding = 0,
          plotlyOutput("cmp_plot", height = "calc(100vh - 175px)")
        )
      ),
      
      card(
        style = paste0("background:", SB_BG, "; overflow-y:auto;"),
        card_body(
          padding = "0.75rem",
          sidebar_head("Select Stations (up to 8)"),
          checkboxGroupInput(
            "cmp_sites", NULL,
            choices  = CMP_CHOICES,
            selected = c("SPA", "SPM", "SLU", "CEL")
          )
        )
      )
    )
  ),
  
  # ── (e) Download ───────────────────────────────────────────────────────────
  nav_panel(
    "Download",
    icon = icon("download"),
    
    div(
      style = "max-width:900px; margin:0 auto; padding:1.5rem 1rem;",
      
      div(
        style = "margin-bottom:1.25rem;",
        
        h2(
          "Download Temperature Data",
          style = paste(
            "font-family:Georgia,serif;",
            "color:#1a3a5c;",
            "margin-bottom:.25rem;"
          )
        ),
        
        p(
          paste(
            "Choose the stations, years, and quality-control option below.",
            "The downloaded CSV contains one row per station and date."
          ),
          style = "color:#5f7a90; font-size:1rem;"
        )
      ),
      
      card(
        card_header("Choose Data"),
        
        card_body(
          layout_columns(
            col_widths = c(7, 5),
            
            div(
              radioButtons(
                "dl_scope",
                "Stations",
                choices = c(
                  "All monitoring stations" = "all",
                  "Choose specific stations" = "selected"
                ),
                selected = "all"
              ),
              
              conditionalPanel(
                condition = "input.dl_scope == 'selected'",
                
                selectizeInput(
                  "dl_sites",
                  "Select stations",
                  choices = SITE_CHOICES,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    placeholder = "Type or select station names",
                    plugins = list("remove_button")
                  )
                ),
                
                helpText(
                  "You may select one or more surface-water or spring stations."
                )
              ),
              
              sliderInput(
                "dl_years",
                "Year range",
                min = year_min,
                max = year_max,
                value = c(year_min, year_max),
                step = 1,
                sep = ""
              ),
              
              checkboxInput(
                "dl_include_flagged",
                "Include rows flagged during quality control",
                value = FALSE
              ),
              
              helpText(
                paste(
                  "By default, quality-control flagged rows are excluded.",
                  "Select the option above when you need the complete unfiltered record."
                )
              )
            ),
            
            div(
              h5(
                "Your Download",
                style = "color:#1a3a5c; margin-top:.25rem;"
              ),
              
              div(
                style = paste(
                  "background:#f4f8fb;",
                  "border:1px solid #d5e3ec;",
                  "border-radius:.5rem;",
                  "padding:1rem;",
                  "margin-bottom:1rem;"
                ),
                uiOutput("dl_summary")
              ),
              
              uiOutput("dl_download_ui"),
              
              actionButton(
                "dl_reset",
                "Reset Filters",
                icon = icon("rotate-left"),
                class = "btn-outline-secondary btn-sm w-100",
                style = "margin-top:.6rem;"
              )
            )
          )
        )
      ),
      
      card(
        style = "margin-top:1rem;",
        card_header("Columns Included"),
        
        card_body(
          tags$p(tags$strong("Station information: "),
                 "site ID, station name, site type, and watershed"),
          tags$p(tags$strong("Date information: "),
                 "date, year, and month"),
          tags$p(tags$strong("Temperature information: "),
                 "daily mean and daily maximum temperature in degrees Celsius"),
          tags$p(tags$strong("Quality control: "),
                 "indicator showing whether the row was flagged during review")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a("kwrc.bucknell.edu", href = "https://www.bucknell.edu/kwrc",
           target = "_blank",
           class  = "nav-link",
           style  = "font-size:.85rem; opacity:.85;")
  )
)

# ── 5. Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Overview map ─────────────────────────────────────────────────────────
  output$overview_map <- renderPlotly({
    d <- site_meta |> filter(!is.na(latitude), site_id %in% ALL_SITE_IDS)
    
    d_surface <- d |> filter(site_type == "Surface")
    d_spring  <- d |> filter(site_type == "Spring")
    
    plot_ly() |>
      add_watershed_traces() |>
      add_trace(
        data          = d_surface,
        lat           = ~latitude,
        lon           = ~longitude,
        type          = "scattermapbox",
        mode          = "markers",
        name          = "Surface",
        marker        = list(size = 13, color = "#cc4e00", opacity = 0.9),
        text          = ~paste0(
          "<b>", site_name, "</b><br>",
          site_id, " \u00b7 Surface<br>",
          "Watershed: ", watershed, "<br>",
          "Record: ", record_start, "\u2013", record_end
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) |>
      add_trace(
        data          = d_spring,
        lat           = ~latitude,
        lon           = ~longitude,
        type          = "scattermapbox",
        mode          = "markers",
        name          = "Spring",
        marker        = list(size = 13, color = "#003f8a", opacity = 0.9),
        text          = ~paste0(
          "<b>", site_name, "</b><br>",
          site_id, " \u00b7 Spring<br>",
          "Watershed: ", watershed, "<br>",
          "Record: ", record_start, "\u2013", record_end
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) |>
      layout(
        mapbox  = list(
          style  = "open-street-map",
          center = list(lat = 40.855, lon = -77.82),
          zoom   = 10
        ),
        legend  = list(orientation = "h", x = 0, y = 0,
                       bgcolor = "rgba(255,255,255,0.8)"),
        margin  = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
  
  # ── Overview table ───────────────────────────────────────────────────────
  output$overview_table <- renderDT({
    summ <- temp_data |>
      filter(!is.na(daily_mean_c)) |>
      group_by(site_id, site_name, site_type, watershed) |>
      summarise(
        `First year`     = min(year),
        `Last year`      = max(year),
        `Days`           = n(),
        `Mean temp (°C)` = round(mean(daily_mean_c, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      select(-site_id) |>
      rename(Station = site_name, Type = site_type, Watershed = watershed)
    
    datatable(
      summ,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        scrollY    = "340px",
        scrollX    = TRUE,
        dom        = "t",
        ordering   = TRUE
      ),
      class = "compact stripe"
    )
  })
  
  # ── Time Series ──────────────────────────────────────────────────────────
  
  ts_filtered <- reactive({
    req(input$ts_site, input$ts_years)
    
    stuck_threshold <- if (input$ts_site %in% SPRING_SITES) 48L else 7L
    
    d <- temp_data |>
      filter(
        site_id == input$ts_site,
        year    >= input$ts_years[1],
        year    <= input$ts_years[2]
      ) |>
      arrange(date) |>
      mutate(
        daily_mean_c = mask_stuck_values(daily_mean_c, stuck_threshold),
        daily_max_c  = mask_stuck_values(daily_max_c,  stuck_threshold)
      )
    
    if (nrow(d) > 0) {
      d <- tidyr::complete(d, date = seq.Date(min(date, na.rm = TRUE),
                                              max(date, na.rm = TRUE),
                                              by = "day")) |>
        arrange(date)
    }
    d
  })
  
  output$ts_title <- renderText({
    m <- site_meta |> filter(site_id == req(input$ts_site))
    paste0(m$site_name, " — Daily Water Temperature")
  })
  
  output$ts_minimap  <- renderPlotly({ mini_map(req(input$ts_site), site_meta) })
  output$sp_minimap  <- renderPlotly({ mini_map(req(input$sp_site), site_meta) })
  output$cmp_minimap <- renderPlotly({
    sites <- head(req(input$cmp_sites), 8)
    mini_map(sites, site_meta, colors = LINE_COLORS[seq_along(sites)])
  })
  
  output$ts_plot <- renderPlotly({
    d <- ts_filtered()
    validate(need(sum(!is.na(d$daily_mean_c)) > 0,
                  "No data for the selected station and year range."))
    
    dc_smooth <- d |> filter(!is.na(daily_mean_c))
    
    p <- plot_ly() |>
      add_trace(
        data          = d,
        x             = ~date,
        y             = ~daily_mean_c,
        type          = "scatter",
        mode          = "lines",
        connectgaps   = FALSE,
        name          = "Daily mean",
        line          = list(color = "#1d7fb3", width = 1.4),
        hovertemplate = "%{x|%b %d, %Y}<br>Mean: %{y:.1f} \u00b0C<extra></extra>"
      )
    
    if (isTRUE(input$ts_show_max)) {
      p <- p |> add_trace(
        data          = d,
        x             = ~date,
        y             = ~daily_max_c,
        type          = "scatter",
        mode          = "lines",
        connectgaps   = FALSE,
        name          = "Daily max",
        line          = list(color = "#e07040", width = 1),
        hovertemplate = "%{x|%b %d, %Y}<br>Max: %{y:.1f} \u00b0C<extra></extra>"
      )
    }
    
    if (isTRUE(input$ts_show_flags)) {
      df <- d |> filter(qc_any_flag == TRUE, !is.na(daily_mean_c))
      if (nrow(df) > 0) {
        p <- p |> add_trace(
          data   = df,
          x      = ~date,
          y      = ~daily_mean_c,
          type   = "scatter",
          mode   = "markers",
          name   = "Flagged",
          marker = list(color = "#e9c46a", size = 7, symbol = "circle-open",
                        line = list(width = 2))
        )
      }
    }
    
    if (isTRUE(input$ts_smooth) && nrow(dc_smooth) > 60) {
      fit              <- loess(daily_mean_c ~ as.numeric(date), data = dc_smooth, span = 0.12)
      dc_smooth$smooth <- predict(fit)
      p <- p |> add_trace(
        data          = dc_smooth,
        x             = ~date,
        y             = ~smooth,
        type          = "scatter",
        mode          = "lines",
        name          = "Smoothed trend",
        line          = list(color = "#1a3a5c", width = 2.5),
        hovertemplate = "%{x|%b %d, %Y}<br>Trend: %{y:.1f} \u00b0C<extra></extra>"
      )
    }
    
    p |> layout(
      xaxis = list(
        title    = list(text = "", font = list(size = 19)),
        tickfont = list(size = 15),
        showgrid = FALSE
      ),
      yaxis = list(
        title     = list(text = "Temperature (°C)", font = list(size = 19)),
        tickfont  = list(size = 15),
        range     = c(TEMP_YMIN, TEMP_YMAX),
        gridcolor = "#e8ecf0",
        zeroline  = FALSE
      ),
      legend    = list(orientation = "h", x = 0, y = 1.06,
                       font = list(size = 15)),
      hovermode     = "x unified",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      font          = list(family = "system-ui, sans-serif", size = 16,
                           color = "#1a2633"),
      margin        = list(l = 55, r = 20, t = 15, b = 55)
    )
  })
  
  output$ts_dl <- downloadHandler(
    filename = function() {
      paste0("KWRC_temp_", input$ts_site, "_",
             input$ts_years[1], "-", input$ts_years[2], ".csv")
    },
    content = function(file) write_csv(ts_filtered(), file)
  )
  
  # ── Seasonal Patterns ─────────────────────────────────────────────────────
  
  sp_filtered <- reactive({
    req(input$sp_site, input$sp_years)
    temp_data |>
      filter(
        site_id == input$sp_site,
        year    >= input$sp_years[1],
        year    <= input$sp_years[2],
        !is.na(daily_mean_c)
      ) |>
      mutate(month_label = factor(month.abb[month], levels = month.abb))
  })
  
  output$sp_title <- renderText({
    m <- site_meta |> filter(site_id == req(input$sp_site))
    paste0(m$site_name, " — Monthly Temperature Distribution (",
           input$sp_years[1], "\u2013", input$sp_years[2], ")")
  })
  
  output$sp_plot <- renderPlotly({
    d <- sp_filtered()
    validate(need(nrow(d) > 0, "No data for the selected station and year range."))
    
    p <- plot_ly(
      data          = d,
      x             = ~month_label,
      y             = ~daily_mean_c,
      type          = "box",
      name          = "Daily mean temp",
      marker        = list(color = "#1d7fb3", size = 3, opacity = 0.35),
      line          = list(color = "#1a3a5c"),
      fillcolor     = "rgba(29, 127, 179, 0.22)",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Median: %{median:.1f} \u00b0C<br>",
        "IQR: %{q1:.1f}\u2013%{q3:.1f} \u00b0C<extra></extra>"
      )
    )
    
    ref_shapes      <- list()
    ref_annotations <- list()
    
    # Groundwater reference — band BELOW 10.5°C (anomalously cold zone)
    if (isTRUE(input$sp_groundwater)) {
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type      = "rect", xref = "paper", x0 = 0, x1 = 1,
        yref      = "y",    y0   = TEMP_YMIN, y1 = 10.5,
        fillcolor = "rgba(43, 138, 155, 0.10)",
        line      = list(width = 0)
      )
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type = "line", xref = "paper", x0 = 0, x1 = 1,
        yref = "y",    y0   = 10.5,    y1 = 10.5,
        line = list(color = "#2b8a9b", width = 3)
      )
      ref_annotations[[length(ref_annotations) + 1]] <- list(
        xref      = "paper", x = 0.01, y = 10.5,
        text      = "\u25bc Groundwater temperature (~10.5\u00b0C)",
        showarrow = FALSE, xanchor = "left",
        yanchor   = "top", yshift = -5,
        font      = list(color = "#2b8a9b", size = 14,
                         family = "system-ui, sans-serif")
      )
    }
    
    # Cold-water fishery threshold — band above 17°C
    if (isTRUE(input$sp_coldwater)) {
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type      = "rect", xref = "paper", x0 = 0, x1 = 1,
        yref      = "y",    y0   = 17,      y1 = TEMP_YMAX,
        fillcolor = "rgba(201, 139, 45, 0.10)",
        line      = list(width = 0)
      )
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type = "line", xref = "paper", x0 = 0, x1 = 1,
        yref = "y",    y0   = 17,      y1 = 17,
        line = list(color = "#c98b2d", width = 3)
      )
      ref_annotations[[length(ref_annotations) + 1]] <- list(
        xref      = "paper", x = 0.01, y = 17,
        text      = "\u25b2 Cold-water fishery threshold (17\u00b0C)",
        showarrow = FALSE, xanchor = "left",
        yanchor   = "bottom", yshift = 5,
        font      = list(color = "#c98b2d", size = 14,
                         family = "system-ui, sans-serif")
      )
    }
    
    # PA trout stress threshold — band above 20°C
    if (isTRUE(input$sp_trout)) {
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type      = "rect", xref = "paper", x0 = 0, x1 = 1,
        yref      = "y",    y0   = 20,      y1 = TEMP_YMAX,
        fillcolor = "rgba(214, 78, 60, 0.12)",
        line      = list(width = 0)
      )
      ref_shapes[[length(ref_shapes) + 1]] <- list(
        type = "line", xref = "paper", x0 = 0, x1 = 1,
        yref = "y",    y0   = 20,      y1 = 20,
        line = list(color = "#d64e3c", width = 3)
      )
      ref_annotations[[length(ref_annotations) + 1]] <- list(
        xref      = "paper", x = 0.01, y = 20,
        text      = "\u25b2 PA trout stress threshold (20\u00b0C)",
        showarrow = FALSE, xanchor = "left",
        yanchor   = "bottom", yshift = 5,
        font      = list(color = "#d64e3c", size = 14,
                         family = "system-ui, sans-serif")
      )
    }
    
    p |> layout(
      xaxis = list(
        title    = list(text = "Month", font = list(size = 19)),
        tickfont = list(size = 15),
        showgrid = FALSE
      ),
      yaxis = list(
        title     = list(text = "Daily Mean Temperature (\u00b0C)",
                         font = list(size = 19)),
        tickfont  = list(size = 15),
        range     = c(TEMP_YMIN, TEMP_YMAX),
        gridcolor = "#e8ecf0",
        zeroline  = FALSE
      ),
      legend = list(orientation = "h", x = 0, y = 1.06,
                    font = list(size = 15)),
      showlegend    = TRUE,
      shapes        = if (length(ref_shapes) > 0) ref_shapes else NULL,
      annotations   = if (length(ref_annotations) > 0) ref_annotations else NULL,
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      font          = list(family = "system-ui, sans-serif", size = 16,
                           color = "#1a2633"),
      margin        = list(l = 55, r = 20, t = 15, b = 55)
    )
  })
  
  # ── Compare Sites ─────────────────────────────────────────────────────────
  
  cmp_filtered <- reactive({
    req(input$cmp_sites, input$cmp_years, input$cmp_metric)
    sites_capped <- head(input$cmp_sites, 8)
    temp_data |>
      filter(
        site_id %in% sites_capped,
        year    >= input$cmp_years[1],
        year    <= input$cmp_years[2]
      )
  })
  
  output$cmp_plot <- renderPlotly({
    d <- cmp_filtered()
    validate(need(nrow(d) > 0, "No data for the selected stations and year range."))
    
    metric     <- input$cmp_metric
    metric_lbl <- if (metric == "daily_mean_c") "Daily mean" else "Daily maximum"
    
    summ <- d |>
      filter(!is.na(.data[[metric]])) |>
      group_by(site_id, site_name, month) |>
      summarise(med = median(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
      mutate(month_label = factor(month.abb[month], levels = month.abb))
    
    sites_ordered <- unique(summ$site_id)
    p <- plot_ly()
    
    for (i in seq_along(sites_ordered)) {
      s  <- sites_ordered[i]
      sd <- summ |> filter(site_id == s)
      p  <- p |> add_trace(
        data          = sd,
        x             = ~month_label,
        y             = ~med,
        type          = "scatter",
        mode          = "lines+markers",
        name          = sd$site_name[1],
        line          = list(color = LINE_COLORS[i], width = 2.2),
        marker        = list(color = LINE_COLORS[i], size = 7),
        hovertemplate = paste0(
          "<b>", sd$site_name[1], "</b><br>",
          "%{x}: %{y:.1f} \u00b0C<extra></extra>"
        )
      )
    }
    
    p |> layout(
      xaxis = list(
        title    = list(text = "Month", font = list(size = 19)),
        tickfont = list(size = 15),
        showgrid = FALSE
      ),
      yaxis = list(
        title = list(
          text = paste0(metric_lbl, " temperature (\u00b0C)"),
          font = list(size = 19)
        ),
        tickfont  = list(size = 15),
        range     = c(TEMP_YMIN, TEMP_YMAX),
        gridcolor = "#e8ecf0",
        zeroline  = FALSE
      ),
      legend        = list(orientation = "v", x = 1.01, y = 1, xanchor = "left",
                           font = list(size = 15)),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      font          = list(family = "system-ui, sans-serif", size = 16,
                           color = "#1a2633"),
      margin        = list(l = 55, r = 20, t = 15, b = 55)
    )
  })
  
  # ── Download ───────────────────────────────────────────────────────────────
  
  dl_filtered <- reactive({
    req(input$dl_years, input$dl_scope)
    
    d <- temp_data |>
      filter(
        year >= input$dl_years[1],
        year <= input$dl_years[2]
      )
    
    if (identical(input$dl_scope, "selected")) {
      selected_sites <- input$dl_sites
      
      if (is.null(selected_sites) || length(selected_sites) == 0) {
        return(
          d |>
            slice(0) |>
            select(
              site_id, site_name, site_type, watershed,
              date, year, month,
              daily_mean_c, daily_max_c, qc_any_flag
            )
        )
      }
      
      d <- d |> filter(site_id %in% selected_sites)
    }
    
    if (!isTRUE(input$dl_include_flagged)) {
      d <- d |> filter(qc_any_flag == FALSE | is.na(qc_any_flag))
    }
    
    d |>
      arrange(site_name, date) |>
      select(
        site_id, site_name, site_type, watershed,
        date, year, month,
        daily_mean_c, daily_max_c, qc_any_flag
      )
  })
  
  output$dl_summary <- renderUI({
    d <- dl_filtered()
    
    if (
      identical(input$dl_scope, "selected") &&
      (is.null(input$dl_sites) || length(input$dl_sites) == 0)
    ) {
      return(
        div(
          icon("triangle-exclamation"),
          tags$strong(" Select at least one station."),
          tags$p("Or choose 'All monitoring stations.'",
                 style = "margin:.35rem 0 0; color:#5f7a90;")
        )
      )
    }
    
    if (nrow(d) == 0) {
      return(
        div(
          icon("circle-exclamation"),
          tags$strong(" No matching data."),
          tags$p("Try changing the station, year, or quality-control filters.",
                 style = "margin:.35rem 0 0; color:#5f7a90;")
        )
      )
    }
    
    date_min <- min(d$date, na.rm = TRUE)
    date_max <- max(d$date, na.rm = TRUE)
    
    tagList(
      tags$p(style = "margin-bottom:.45rem;",
             icon("location-dot"), " ",
             tags$strong(n_distinct(d$site_id)),
             if (n_distinct(d$site_id) == 1) " station" else " stations"),
      tags$p(style = "margin-bottom:.45rem;",
             icon("calendar"), " ",
             format(date_min, "%B %d, %Y"), " through ",
             format(date_max, "%B %d, %Y")),
      tags$p(style = "margin-bottom:.45rem;",
             icon("table"), " ",
             tags$strong(formatC(nrow(d), format = "d", big.mark = ",")), " rows"),
      tags$p(style = "margin-bottom:0;",
             icon("shield-halved"), " ",
             if (isTRUE(input$dl_include_flagged)) {
               "Includes quality-control flagged rows"
             } else {
               "Quality-control flagged rows excluded"
             })
    )
  })
  
  output$dl_download_ui <- renderUI({
    d <- dl_filtered()
    
    if (nrow(d) == 0) {
      return(
        tags$button(
          icon("download"), " Download CSV",
          type = "button",
          class = "btn btn-primary w-100",
          disabled = "disabled"
        )
      )
    }
    
    downloadButton("dl_btn", "Download CSV",
                   icon = icon("download"),
                   class = "btn-primary w-100")
  })
  
  observeEvent(input$dl_reset, {
    updateRadioButtons(session, "dl_scope", selected = "all")
    updateSelectizeInput(session, "dl_sites", selected = character(0))
    updateSliderInput(session, "dl_years", value = c(year_min, year_max))
    updateCheckboxInput(session, "dl_include_flagged", value = FALSE)
  })
  
  output$dl_btn <- downloadHandler(
    filename = function() {
      station_text <- if (identical(input$dl_scope, "all")) {
        "all-stations"
      } else {
        paste0(length(input$dl_sites), "-stations")
      }
      qc_text <- if (isTRUE(input$dl_include_flagged)) "all-rows" else "qc-clean"
      paste0("KWRC_temperature_", input$dl_years[1], "-", input$dl_years[2],
             "_", station_text, "_", qc_text, ".csv")
    },
    content = function(file) {
      d <- dl_filtered()
      req(nrow(d) > 0)
      write_csv(d, file, na = "")
    }
  )
  
}

# ── 6. Run ───────────────────────────────────────────────────────────────────

shinyApp(ui, server)
