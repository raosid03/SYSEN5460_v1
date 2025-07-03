#group8 dashboard
#michaellaginestra,siddarthrao, madisonmccall, kevinjones

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(janitor)

#prep the data for the dashboard
raw_data <- read_csv("data/jp_solar.csv") %>%
  clean_names() %>%
  filter(date != "2017-09-25") %>%
  mutate(
    date                    = as.Date(date),
    solar_rate              = if_else(solar_rate == 0, 0.001, solar_rate),
    solar_rate_per_thousand = solar_rate * 1000
  ) %>%
  arrange(muni_code, date)

#global data points
muni_list <- c("All", sort(unique(raw_data$muni_code)))
min_pop   <- min(raw_data$pop, na.rm = TRUE)
max_pop   <- max(raw_data$pop, na.rm = TRUE)
year_min  <- min(raw_data$year, na.rm = TRUE)
year_max  <- max(raw_data$year, na.rm = TRUE)

#user interface creation
ui <- fluidPage(
  titlePanel("Solar Adoption Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "muni", 
        "Select Municipality:", 
        choices  = muni_list,
        selected = "All"
      ),
      
      sliderInput(
        "population", 
        "Max Population:", 
        min   = min_pop, 
        max   = max_pop,
        value = median(raw_data$pop, na.rm = TRUE)
      ),
      
      checkboxGroupInput(
        "disaster", 
        "Disaster status:",
        choices  = c("No Disaster" = 0, "Disaster" = 1),
        selected = c(0, 1)
      ),
      
      sliderInput(
        "yearRange", 
        "Year Range:",
        min   = year_min,
        max   = year_max,
        value = c(year_min, year_max),
        step  = 1,
        sep   = ""
      )
    ),
    
    mainPanel(
      h3("Plot 1: Solar Adoption Rates"),
      verbatimTextOutput("summary1"),
      plotOutput("plot1", height = "300px"),
      
      hr(),
      
      h3("Plot 2: Installations per 1,000 Residents"),
      fluidRow(
        column(2, wellPanel(h4("Years"),     textOutput("nYears"))),
        column(2, wellPanel(h4("Start"),     textOutput("startRate"))),
        column(2, wellPanel(h4("End"),       textOutput("endRate"))),
        column(2, wellPanel(h4("Δ Rate"),    textOutput("absChange"))),
        column(2, wellPanel(h4("% Change"),  textOutput("pctChange")))
      ),
      plotOutput("plot2", height = "400px")
    )
  )
)

#server 
server <- function(input, output, session) {
  
  #reactive filter
  data_common <- reactive({
    req(input$muni, input$population, input$disaster, input$yearRange)
    
    df <- raw_data %>%
      filter(
        year       >= input$yearRange[1],
        year       <= input$yearRange[2],
        pop        <= input$population,
        disaster   %in% as.numeric(input$disaster)
      )
    
    if (input$muni != "All") {
      df <- filter(df, muni_code == input$muni)
    }
    
    df
  })
  
  #plot1 summary chart by date
  summary_by_date <- reactive({
    data_common() %>%
      group_by(date) %>%
      summarise(
        mean_rate = mean(solar_rate, na.rm = TRUE),
        ci_lower  = quantile(solar_rate, 0.025, na.rm = TRUE),
        ci_upper  = quantile(solar_rate, 0.975, na.rm = TRUE),
        .groups   = "drop"
      )
  })
  
  #plot two, creation of summary by year
  summary_by_year <- reactive({
    data_common() %>%
      group_by(year) %>%
      summarise(
        mean_rate = mean(solar_rate_per_thousand, na.rm = TRUE),
        sd        = sd(solar_rate_per_thousand,   na.rm = TRUE),
        n         = n(),
        se        = sd / sqrt(n),
        ci_lower  = mean_rate - 1.96 * se,
        ci_upper  = mean_rate + 1.96 * se,
        .groups   = "drop"
      ) %>%
      arrange(year)
  })
  
  #output indicators for plot 1
  output$summary1 <- renderPrint({
    summary(data_common())
  })
  
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
  
  #metric charts information based on plot 2 information
  output$nYears     <- renderText({ nrow(summary_by_year()) })
  output$startRate  <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[1], 1) else NA
  })
  output$endRate    <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[nrow(df)], 1) else NA
  })
  output$absChange  <- renderText({
    df <- summary_by_year()
    if (nrow(df)) round(df$mean_rate[nrow(df)] - df$mean_rate[1], 1) else NA
  })
  output$pctChange  <- renderText({
    df <- summary_by_year()
    if (nrow(df)) {
      change <- df$mean_rate[nrow(df)] - df$mean_rate[1]
      paste0(round(change / df$mean_rate[1] * 100, 1), "%")
    } else NA
  })
  
  #output for plot 2
  output$plot2 <- renderPlot({
    df <- summary_by_year()
    
    ggplot(df, aes(x = year, y = mean_rate)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                  fill = "lightblue", alpha = 0.5) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = "Installations per 1,000 Residents Over Time",
        subtitle = paste0(
          "Filtered between years ", input$yearRange[1], "–",
          input$yearRange[2]
        ),
        x = "Year", y = "Installs per 1,000 Residents"
      ) +
      theme_minimal()
  })
}

#run the shinyApp!!
shinyApp(ui, server)
