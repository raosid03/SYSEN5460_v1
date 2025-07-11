#' @name example.R
#' @description
#' Example script for how to use data in this folder.

# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(sf)
library(leaflet)

# Read & prep solar data
solar_df <- read_csv("data/jp_solar.csv") %>%
  clean_names() %>%
  filter(date != "2017-09-25") %>%
  mutate(
    date = as.Date(date),
    solar_rate = if_else(solar_rate == 0, 0.001, solar_rate),
    solar_rate_per_thousand = solar_rate * 1000
  ) %>%
  arrange(muni_code, date)

# Summarize solar adoption by muni
solar_df_summary <- solar_df %>%
  group_by(muni_code = as.character(muni_code)) %>%
  summarise(mean_solar = mean(solar_rate, na.rm = TRUE), .groups = "drop")

# Read municipality spatial data
muni_sf <- st_read("data/japan/municipalities.geojson", quiet = TRUE) %>%
  mutate(muni_code = as.character(muni_code)) %>%
  left_join(solar_df_summary, by = "muni_code")

# UI
ui <- fluidPage(
  titlePanel("Solar Adoption Dashboard – Group 8"),
  tabsetPanel(
    tabPanel("Solar Trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("muni", "Select Municipality:", choices = c("All", sort(unique(solar_df$muni_code))), selected = "All"),
                 sliderInput("population", "Max Population:", min = min(solar_df$pop, na.rm = TRUE), max = max(solar_df$pop, na.rm = TRUE), value = median(solar_df$pop, na.rm = TRUE)),
                 checkboxGroupInput("disaster", "Disaster status:", choices = c("No Disaster" = 0, "Disaster" = 1), selected = c(0, 1)),
                 sliderInput("yearRange", "Year Range:", min = min(solar_df$year), max = max(solar_df$year), value = c(min(solar_df$year), max(solar_df$year)), step = 1, sep = "")
               ),
               mainPanel(
                 h3("Plot 1: Solar Adoption Rates"),
                 verbatimTextOutput("summary1"),
                 plotOutput("plot1", height = "300px"),
                 hr(),
                 h3("Plot 2: Installations per 1,000 Residents"),
                 fluidRow(
                   column(2, wellPanel(h4("Years"), textOutput("nYears"))),
                   column(2, wellPanel(h4("Start"), textOutput("startRate"))),
                   column(2, wellPanel(h4("End"), textOutput("endRate"))),
                   column(2, wellPanel(h4("Δ Rate"), textOutput("absChange"))),
                   column(2, wellPanel(h4("% Change"), textOutput("pctChange")))
                 ),
                 plotOutput("plot2", height = "400px"),
                 hr(),
                 h3("Plot 3: Tokyo Municipalities by Cluster"),
                 plotOutput("plot3", height = "400px")
               )
             )
    ),
    tabPanel("Spatial Map",
             leafletOutput("solarMap", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_common <- reactive({
    df <- solar_df %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2],
        pop <= input$population,
        disaster %in% as.numeric(input$disaster)
      )
    if (input$muni != "All") {
      df <- df %>% filter(muni_code == input$muni)
    }
    df
  })
  
  summary_by_date <- reactive({
    data_common() %>%
      group_by(date) %>%
      summarise(
        mean_rate = mean(solar_rate, na.rm = TRUE),
        ci_lower = quantile(solar_rate, 0.025, na.rm = TRUE),
        ci_upper = quantile(solar_rate, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  summary_by_year <- reactive({
    data_common() %>%
      group_by(year) %>%
      summarise(
        mean_rate = mean(solar_rate_per_thousand, na.rm = TRUE),
        sd = sd(solar_rate_per_thousand, na.rm = TRUE),
        n = n(),
        se = sd / sqrt(n),
        ci_lower = mean_rate - 1.96 * se,
        ci_upper = mean_rate + 1.96 * se,
        .groups = "drop"
      )
  })
  
  output$summary1 <- renderPrint({ summary(data_common()) })
  
  # Load municipality polygons
  muni = read_sf("data/japan/municipalities.geojson")
  output$plot1 <- renderPlot({
    df    <- data_common()
    stats <- summary_by_date()
    
    ggplot() +
      geom_jitter(
        data = df, aes(x = date, y = solar_rate),
        width = 5, alpha = 0.2, color = "grey50", size = 1
      ) +
      geom_line(
        data = stats, aes(x = date, y = mean_rate),
        color = "blue", size = 1
      ) +
      geom_ribbon(
        data = stats,
        aes(x = date, ymin = ci_lower, ymax = ci_upper),
        fill = "blue", alpha = 0.2
      ) +
      labs(
        title    = "Solar Adoption Rates Over Time",
        subtitle = paste0(
          "Filtered between years ", input$yearRange[1], "–",
          input$yearRange[2]
        ),
        x = "Date", y = "Solar Adoption Rate"
      ) +
      theme_minimal()
  }, res = 96)
  
  # Filter to just municipalities in Hokkaido prefecture
  subset = muni %>%
    filter(pref == "Hokkaido")
  output$plot2 <- renderPlot({
    df <- summary_by_year()
    ggplot(df, aes(x = year, y = mean_rate)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.5) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(title = "Installations per 1,000 Residents Over Time", x = "Year", y = "Installs per 1,000 Residents") +
      theme_minimal()
  })
  
  #plot3
  jp_solar = read_csv("data/jp_solar.csv")
  mymunis = read_sf("data/japan/municipalities.geojson")
  mypref = read_sf("data/japan/prefectures.geojson")
  myregions = read_sf("data/japan/regions.geojson")
  tohoku_region_only <- myregions %>%
    filter(region == "Tohoku")
  tohoku_munis = mymunis %>%
    st_join(tohoku_region_only, join = st_within, left= FALSE) %>%
    left_join(by = "muni_code", y = jp_solar)
  tohoku_munis_centroids = tohoku_munis %>%
    mutate(
      points = st_centroid(geometry),
      coords = st_coordinates(points),
      x = coords[,1],
      y = coords[,2]
    ) %>%
    select(muni_code, solar_rate, x, y)
  m <- kmeans(x = tohoku_munis_centroids$x, centers = 6)
  # View cluster details
  broom::tidy(m)
  broom::glance(m)
  tohoku_munis_centroids = tohoku_munis_centroids %>%
    mutate(cluster = m$cluster)
  cluster_polygons = tohoku_munis_centroids %>%
    # For each cluster...
    group_by(cluster) %>%
    # union points into multipoints, then find the boundaries of the multipoints
    summarize(geometry = geometry %>% st_union() %>% st_convex_hull())
  # Visualize it!
  output$plot3 <- renderPlot({
    ggplot() +
      # Plot a simple background of precincts
      geom_sf(data = mypref, fill = "black", color = "#373737") +
      # Plot polls over top
      geom_sf(data = tohoku_munis_centroids,
              mapping = aes(fill = factor(cluster) ),
              size = 3, shape = 21, color = "white") +
      # Visualize the clusters
      geom_sf(data = cluster_polygons,
              mapping = aes(fill = factor(cluster) ),
              alpha = 0.5, color = "white") +
      coord_sf(xlim = c(139, 142.5),
               ylim = c(37, 41.5)) +
      labs(fill = "Tohoku Municipality by Cluster") +
      theme_bw()
  })
  
  ggplot()  +
    geom_sf(data = subset)
  output$nYears <- renderText({ nrow(summary_by_year()) })
  output$startRate <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[1], 1) else NA
  })
  output$endRate <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[nrow(df)], 1) else NA
  })
  output$absChange <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[nrow(df)] - df$mean_rate[1], 1) else NA
  })
  output$pctChange <- renderText({
    df <- summary_by_year()
    if (nrow(df)) {
      change <- df$mean_rate[nrow(df)] - df$mean_rate[1]
      paste0(round(change / df$mean_rate[1] * 100, 1), "%")
    } else NA
  })
  
  output$solarMap <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = muni_sf$mean_solar, na.color = "transparent")
    leaflet(muni_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(mean_solar),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
        label = ~paste0(muni, ": ", round(mean_solar, 2))
      ) %>%
      addLegend(pal = pal, values = ~mean_solar, opacity = 0.7, title = "Mean Solar Rate", position = "bottomright")
  })
}

# Same idea applies for:
pref = read_sf("data/japan/prefectures.geojson")
regions = read_sf("data/japan/regions.geojson")
country = read_sf("data/japan/country.geojson")
# Run the app
shinyApp(ui = ui, server = server)
