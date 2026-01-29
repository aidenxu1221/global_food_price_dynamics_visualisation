library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(bslib)

# setting to ensure shape files can be loaded in
sf::sf_use_s2(FALSE)
Sys.setenv(SHAPE_RESTORE_SHX="YES")

# load data
df_price <- read.csv("FIT5147DVP_food_price.csv")
df_health <- read.csv("FIT5147DVP_price_health.csv")
world <- st_read("ne_110m_admin_0_countries.shp", quiet = TRUE)

# UI
ui <- navbarPage(
  # configure title and theme
  title = span("World Food Price Visual Analysis", style = "font-size: 24px;"),
  theme = bs_theme(version = 5, bootswatch = "sandstone", bg = "#F0F7F0", fg = "#212529"),
  
  # Add custom CSS for page styling
  header = tags$head(
    tags$style(HTML("
      body {font-family: Georgia, Times, 'Times New Roman', serif;}
      .navbar {min-height: 50px;}
      .nav.navbar-nav > li > a {font-size: 18px; padding-top: 18px; padding-bottom: 18px;}
      h1, h2, h3, h4, h5, h6 {font-family: Georgia, Times, 'Times New Roman', serif;}
      .leaflet-container {background: #ffffff}
      .control-label {font-size: 18px;}
      .selectize-input, .selectize-dropdown {font-size: 18px;}
      .irs-min, .irs-max, .irs-single, .irs-from, .irs-to, .irs-grid-text {font-size: 10px;}
      .checkbox label {font-size: 18px;}
      .irs-slider-label, .js-irs-label {font-size: 20px;}
      .nav-tabs {margin-bottom: 20px;}
      .metadata-section {background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;}
      .source-card {background: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 15px;}
    "))
  ),
  
  # Tab 1: Main Visualisation page
  tabPanel(
    "Home",
    # write titles in this page
    div(
      style = "text-align: center; margin: 40px 0;",
      h1("Food Price Fluctuations Pattern & Implications",
        style = "font-size: 40px; text-transform: uppercase; font-weight: 700;")
      ),
    
    # Introduction section layout
    fluidRow(
      column(
        width = 8,
        offset = 2,
        wellPanel(
          h3("Introduction", style = "font-weight: 700;"),
          h4("The fluctuations in food prices have affected people's life quality largely in the world. After COVID-19 pandemic, 
          the inflation such as the rise of food prices has increased the economic burden on many countries. The pressure of cost-of-living 
          has increased significantly. Human well-being might also be affected because changes in food prices may affect people's nutrition intake.",
          tags$br(),tags$br(),
          "This analysis can visualise the global food price inflation rates across different countries over time and identify 
          underlying fluctuation patterns. In addition, this visualisation also provides opportunities to explore and assess the implications 
          of price fluctuations on economy and health for different countries.", 
          style = "line-height: 1.2;")
        )
      )
    ),
    
    # Title for global food price inflation map section
    fluidRow(
      style = "margin: 20px 10px 10px 20px;",
      column(
        width = 12,
        div(
          style = "border-bottom: 3px solid #ddd; padding-bottom: 6px;",
          h2("Global Food Price Inflation Rate", style = "font-weight: 700;"))
      )
    ),
    
    # set up map section
    fluidRow(
      style = "margin: 0 15px;",
      column(width = 10,
             offset = 1,
             leafletOutput("map", height = "500px"))
    ),
    
    # set up map animation controls panel & text explanation section
    fluidRow(
      style = "margin: 15px;",
      # map controls panel
      column(
        width = 4,
        wellPanel(
          h3("Map Animation Controls", style = "font-weight: 700;"),
          # set up slider
          sliderInput("year",
                      "Select Year:",
                      min = min(df_price$Year),
                      max = max(df_price$Year),
                      value = min(df_price$Year),
                      step = 1,
                      sep = "",
                      ticks = TRUE
          ),
          # play and stop button
          div(
            style = "display: flex; gap: 10px; margin-top: 15px;",
            actionButton("play", "Play", icon = icon("play"), class = "btn-primary", style = "font-size: 18px;"),
            actionButton("stop", "Stop", icon = icon("stop"), class = "btn-danger", style = "font-size: 18px;")
          ),
          # text box to show message about animation
          div(
            style = "margin-top: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9; font-size: 18px;",
            textOutput("animation_status")
          )
        )
      ),
      # Map explanation text panel
      column(
        width = 8,
        wellPanel(
          tags$style(HTML(".viz-text {font-size: 22px;}")),
          h3("Map Interpretation", style = "font-weight: 700;"),
          tags$p(class = "viz-text", "From this map, you can identify the price inflation rate for each country according to the colour legend or hover-over tooltips. 
          You can also explore the patterns or trends of price fluctuations for each country from 2001 to 2023 by animation control. "),
          tags$p(class = "viz-text", "For instance, the following key messages could be discovered from this map: "),
          tags$div(
            tags$ul(
              tags$li(class = "viz-text", "Inflation rates over 100% primarily occurred in developing countries in South America and Africa."),
              tags$li(class = "viz-text", "The 2008 global financial crisis triggered worldwide food price inflation across all nations in 2008."),
              tags$li(class = "viz-text", "After COVID-19 pandemic, food inflation rates surged worldwide. By 2022, nearly all countries recorded positive inflation rates around or over 10%, indicating a significant global economic crisis. Argentina and Zimbabwe experienced the most severe inflation."),
              tags$li(class = "viz-text", "Political conflicts also influenced food prices. For instance, Iran has maintained severe inflation rates above 80% annually due to ongoing political tensions since 2021.")
            )
          ),
          h3("User Interaction Instruction", style = "font-weight: 700;"),
          tags$div(
            tags$ul(
              tags$li(class = "viz-text", "Zoom: Use mouse scroll to zoom in/out"),
              tags$li(class = "viz-text", "Pan: Click and drag to move the map"),
              tags$li(class = "viz-text", "Time Control: Use the slider to update the year; The year label is highlighted in the top right"),
              tags$li(class = "viz-text", "Animation: Use play/stop buttons to visualise inflation rate changes dynamically"),
              tags$li(class = "viz-text", "Country Details: Hover over countries to view detailed data"),
              tags$li(class = "viz-text", "Interactive Features: Click on a country to view its data in the following line chart and radar chart. When no country is selected or click the selected country again, world median data is displayed.")
            )
          )
        )
      )
    ),
    
    # Title for economic implication section
    fluidRow(
      style = "margin: 20px 10px 10px 20px;", 
      column(
        width = 12,
        div(
          style = "border-bottom: 3px solid #ddd; padding-bottom: 6px;",
          h2("Impacts of Food Price Fluctuation on Economy", style = "font-weight: 700;")
        )
      )
    ),
    
    # create chart for economic implication section
    fluidRow(
      style = "margin: 15px;",
      # Line Chart
      column(
        width = 9,
        wellPanel(plotlyOutput("lineChart", height = "600px"))
      ),
      # Interactive Controls Panel
      column(
        width = 3,
        wellPanel(
          h3("Line Chart Controls", style = "font-weight: 700;"),
          # Drag-down lists for country selection
          selectizeInput("selectedCountry",
                         "Select Country:",
                         choices = c("World Median" = "World Median"),  # Will be updated in server
                         selected = "World Median"
          ),
          # Drag-down for food category selection
          selectInput("selectedItem",
                      "Select Food Category:",
                      choices = c("Meat", "Vegetables and Fruit", "Cereal"),
                      selected = "Meat"
          ),
          # Checkbox lists for line selection/attributes selection
          checkboxGroupInput("selectedLines",
                             "Select indicators to display:",
                             choices = list(
                               "Food Price Inflation Rate" = "Food_Price_Inflation",
                               "Producer Price Index (PPI) Growth Rate" = "ppi_rate",
                               "Gross Production Index (GPI) Growth Rate" = "gpi_rate",
                               "Gross Domestic Product (GDP) Growth Rate" = "gdp_rate"
                             ),
                             selected = c("Food_Price_Inflation", "ppi_rate", "gpi_rate", "gdp_rate")
          )
        )
      )
    ),
    
    # text explanation section for line chart 1
    fluidRow(
      column(
        width = 8,
        offset = 2,
        wellPanel(
          tags$style(HTML(".linechart1-description {font-size: 22px;}")),
          tags$div(class = "linechart1-description",
            tags$p(
              "This line chart visualises the annual trends from 2001 to 2023 for four key economic indicators.",
              "By comparing these growth rates, you can analyse the economic implications of price fluctuations for a specific country. The chart provides data for three food categories: Meat, Vegetables & Fruits, and Cereals.",
              tags$br(), tags$br(),
              "The chart initially displays world median data if no country is selected on the map. When a country is selected, the data updates to show the country's indicators.",
              tags$br(),tags$br(),
              "For instance, according to World Median data, there were two peaks for price inflation rate in 2008 and 2022, respectively. Both GDP and GPI growth rate in these years decreased largely.
              Therefore, we can see how price inflation was negatively associated with the world economy."
            ),
            
            h3("User Interaction Instruction", style = "font-weight: 700;"),
            tags$div(class = "linechart1-description",
              tags$ul(
                tags$li("Click and drag to zoom into a specific area of the chart. Double-click to reset to full view"),
                tags$li("Hover over points to view detailed values"),
                tags$li("Use dropdown menu to choose specific countries or food categories"),
                tags$li("Select/Unselect indicators using the checkbox list"),
                tags$li("Selecting a country can highlight its location on the map and update all linked charts with country-specific data")
              )
            )
          )
        )
      )
    ),
    
    # Title for health implications section
    fluidRow(
      style = "margin: 20px 10px 10px 20px;",
      column(
        width = 12,
        div(
          style = "border-bottom: 3px solid #ddd; padding-bottom: 6px;",
          h2("Impacts of Food Price Fluctuation on Health", style = "font-weight: 700;")
        )
      )
    ),
    
    # radar chart and line chart 2 UI design
    fluidRow(
      style = "margin: 15px;",
      # Radar Chart
      column(
        width = 7,
        wellPanel(plotlyOutput("radarChart", height = "600px"))
      ),
      # Second Line Chart
      column(
        width = 5,
        wellPanel(plotlyOutput("lineChart2", height = "600px"))
      )
    ),
    
    # UI design for health section control panel and text explanation
    fluidRow(
      style = "margin: 15px;",
      # control panel
      column(
        width = 4,
        wellPanel(
          h3("Health Indicators Visualisation Controls", style = "font-weight: 700;"),
          selectizeInput("healthCountry",
                         "Select Country:",
                         choices = c("World Median" = "World Median"),
                         selected = "World Median")
        )
      ),
      # text explanation panel
      column(
        width = 8,
       wellPanel(
         tags$style(HTML(".radarline-description {font-size: 22px;}")),
         tags$div(class = "radarline-description",
           tags$p(
             "The radar chart can evaluate if a country's obesity and stunting rates deviate from world median levels in 2022. You can identify potential relationships between food prices and nutritional status."),
           tags$p(
             "The line chart records life satisfaction scores from 2005 to 2023. You can compare a country trend with world median to reveal:",
             tags$ul(
               tags$li("How life satisfaction score develops in the selected country"),
               tags$li("The degree satisfaction levels exceed or fall below world median")
             ),
             "These charts initially display world median data if no country is selected on the map.",
             tags$br(),tags$br(),
             "According to World Median data, the adult obesity rate was large (>20%), indicating people have excessive nutrition intake.
              Despite food price fluctuations, the world median life satisfaction score has increased significantly since 2005. Therefore, 
             most people in the world have begun to focus on other aspects that contribute to their sense of wellbeing instead of basic needs for food."
           ),
           
           h3("User Interaction Instruction", style = "font-weight: 700;"),
           tags$div(class = "radarline-description",
             tags$ul(
               tags$li("Click and drag to zoom into a specific area of the radar or line chart. Double-click to reset to full view"),
               tags$li("Hover over points to view detailed values"),
               tags$li("Use dropdown menu to choose specific countries"),
               tags$li("Selecting a country can highlight its location on the map and update all linked charts with country-specific data")
             )
           )
         )
       )
      )
    )
  ),
  
  # Tab 2: Data Source
  tabPanel(
    "About Data",
    # set up styling for this page
    tags$style(HTML("
      .metadata-section h2 {font-size: 28px; font-weight: bold; margin-bottom: 20px; border-bottom: 2px solid #eee;}
      .metadata-section p {font-size: 20px;}
      .source-card {font-size: 20px; padding: 20px; border-radius: 8px; margin-bottom: 25px;}
      .source-card h3 {border-bottom: 1px solid #dee2e6;}
      .metadata-section a {font-size: 20px;}
      .metadata-section a:hover {text-decoration: underline;}
    ")),
    
    fluidRow(
      column(
        width = 10,
        offset = 1,
        div(class = "metadata-section",
            h2("About the Data"),
            p("This visualisation is based on comprehensive data collected from multiple data sources, The data includes 
            annual global food prices, economic indicators, and health indicators from 2001 to 2023 across over 180 countries. 
            The implementation of this visualisation uses multiple format of data including tabular data and geospatial shapefile data."),
        ),
        # Data Sources section
        div(class = "metadata-section",
            h2("Data Sources"),
            # Food Price Data & Economic indicators data source
            div(class = "source-card",
                h3("Food Price Data & Economic Indicators Data"),
                p("Licensor: Food and Agriculture Organization (FAO) of the United Nations"),
                p("Access Date: 15 Sept 2024"),
                tags$ul(
                  tags$li("Food Price Inflation Rate"),
                  tags$li("Producer Price Index (PPI)"),
                  tags$li("Gross Production Index (GPI)"),
                  tags$li("Gross Domestic Product (GDP)")
                ),
                p("URLs:"),
                tags$ul(
                  tags$li(
                    "Food Price Inflation: ",
                    a(href = "https://bulks-faostat.fao.org/production/ConsumerPriceIndices_E_All_Data.zip",
                      "https://bulks-faostat.fao.org/production/ConsumerPriceIndices_E_All_Data.zip")
                  ),
                  tags$li(
                    "PPI Data: ",
                    a(href = "https://bulks-faostat.fao.org/production/Prices_E_All_Data.zip",
                      "https://bulks-faostat.fao.org/production/Prices_E_All_Data.zip")
                  ),
                  tags$li(
                    "GPI Data: ",
                    a(href = "https://bulks-faostat.fao.org/production/Production_Indices_E_All_Data.zip",
                      "https://bulks-faostat.fao.org/production/Production_Indices_E_All_Data.zip")
                  ),
                  tags$li(
                    "GDP Data: ",
                    a(href = "https://bulks-faostat.fao.org/production/Macro-Statistics_Key_Indicators_E_All_Data.zip",
                      "https://bulks-faostat.fao.org/production/Macro-Statistics_Key_Indicators_E_All_Data.zip")
                  )
                )
            ),
            
            # Health Metrics data source
            div(class = "source-card",
                h3("Health Indicators"),
                p("Licensor: World Health Organization (WHO)"),
                p("Access Date: 15 Sept 2024"),
                tags$ul(
                  tags$li("Obesity rates (adult, adolescent, child)"),
                  tags$li("Stunting rates (child)")
                ),
                p("URLs:", a(href = "https://cdn.who.int/media/docs/default-source/gho-documents/world-health-statistic-reports/2024/web_download.xlsx?sfvrsn=4e7bbebd_1",
                      "https://cdn.who.int/media/docs/default-source/gho-documents/world-health-statistic-reports/2024/web_download.xlsx?sfvrsn=4e7bbebd_1"))
            ),
            
            # life satisfaction Indicator data source
            div(class = "source-card",
                h3("Life Satisfaction Indicators"),
                p("Licensor: World Happiness Report 2024"),
                p("Access Date: 15 Sept 2024"),
                tags$ul(tags$li("Life Satisfaction Ladder Score")),
                p("URLs:", a(href = "https://happiness-report.s3.amazonaws.com/2024/DataForTable2.1.xls",
                             "https://happiness-report.s3.amazonaws.com/2024/DataForTable2.1.xls"))
            ),
            
            # Spatial data source
            div(class = "source-card",
                h3("Spatial Data"),
                p("Licensor: Natural Earth Data"),
                p("Access Date: 15 Sept 2024"),
                tags$ul(tags$li("Spatial Shapefiles for Countries")),
                p("URLs:", a(href = "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                             "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip"))
            ),
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  #### Coding for Map
  # Initialize reactive values for animation control
  animation_state <- reactiveVal(FALSE)
  status_message <- reactiveVal("Ready to play animation")
  timer <- reactiveTimer(1000)  # set timer
  
  # Track last update time for animation
  last_update <- reactiveVal(Sys.time())
  
  # Render animation status message
  output$animation_status <- renderText({status_message()})
  
  # create animation timer observer for map
  observe({
    if (!animation_state()) {return()}  # ensure animation initially at stop status
    timer()  # activate timer
    
    # use time to detect the animation speed
    current_time <- Sys.time()
    time_diff <- as.numeric(difftime(current_time, last_update(), units = "secs"))
    
    # set animation speed to 1 seconds
    if (time_diff >= 1.0) {
      current_year <- input$year
      next_year <- current_year + 1
      
      # if reach the final year, stop the animation
      if (next_year > max(df_price$Year)) {
        animation_state(FALSE)
        status_message("Animation completed! Reached year 2023")
      } else {
        status_message(sprintf("Playing animation... Current year: %d", next_year))
        updateSliderInput(session, "year", value = next_year)
        last_update(current_time)  # update the last time of animation
      }
    }
  })
  
  # handle play animation button event
  observeEvent(input$play, {
    animation_state(TRUE)
    last_update(Sys.time())
    status_message(sprintf("Starting animation from year %d", input$year))
  })
  
  # handle stop button event
  observeEvent(input$stop, {
    if (animation_state()) {
      animation_state(FALSE)
      status_message(sprintf("Animation stopped at year %d", input$year))
    }
  })
  
  # store the current selected country
  selected_country <- reactiveVal("World Median")
  
  # read shape files and merge it with dataframe to prepare map data
  world_map <- reactive({
    current_year_data <- df_price %>% 
      filter(Year == input$year) %>% 
      select(Country, Food_Price_Inflation, inflation_group)
    
    merged_data <- world %>%
      left_join(current_year_data, by = c("ADMIN" = "Country")) %>% 
      mutate(inflation_group = ifelse(is.na(inflation_group), "No data", inflation_group))
    return(merged_data)
  })
  
  # function to define mapping color for the map
  getColor <- function(inflation_group) {
    case_when(
      inflation_group == "No data" ~ "#708080",
      inflation_group == "Below -100%" ~ "#662506",
      inflation_group == "-100% to -75%" ~ "#993404",
      inflation_group == "-75% to -50%" ~ "#CC4C02",
      inflation_group == "-50% to -25%" ~ "#EC7014",
      inflation_group == "-25% to -10%" ~ "#FE9929",
      inflation_group == "-10% to 0%" ~ "#FEC44F",
      inflation_group == "0% to 10%" ~ "#DADAEB", 
      inflation_group == "10% to 25%" ~ "#BCB5D6",
      inflation_group == "25% to 50%" ~ "#B39DDB",
      inflation_group == "50% to 75%" ~ "#7E57C2",
      inflation_group == "75% to 100%" ~ "#5E35B1",
      inflation_group == "Above 100%" ~ "#4A1486",
      TRUE ~ "#708080" # ensure NA data to display grey color
    )
  }
  
  
  # define legend labels text and color for map
  legend_labels <- c(
    "Below -100%", "-100% to -75%", "-75% to -50%", "-50% to -25%", "-25% to -10%", "-10% to 0%", 
    "0% to 10%", "10% to 25%", "25% to 50%", "50% to 75%", "75% to 100%", "Above 100%", "No data"
  )
  legend_colors <- sapply(legend_labels, getColor)
  styled_labels <- sapply(legend_labels, function(label) {
    sprintf("<span style='font-size: 18px;'>%s</span>", label)
  })
  
  # Render map visualization with leaflet
  output$map <- renderLeaflet({
    data <- world_map()
    
    # Initialize leaflet map with Robinson projection
    leaflet(data, options = leafletOptions(
      crs = leafletCRS(
        crsClass = "L.Proj.CRS",
        code = "ESRI:54030", # Robinson projection for better global visualization
        proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
        resolutions = c(32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128)
      ))) %>%
      # centre the map view
      setView(lng = 0, lat = 6, zoom = 0) %>%
      addPolygons(
        layerId = ~ADMIN,  # use country name as layerId for event handling
        fillColor = ~getColor(inflation_group),
        weight = ~ifelse(ADMIN == selected_country(), 5, 0.2),  # Emphasize selected country with thicker border
        opacity = 1,
        color = ~ifelse(ADMIN == selected_country(), "red", "black"),  # Highlight selected country border in red
        fillOpacity = 1,
        # Configure hover highlighting
        highlight = highlightOptions(
          weight = 3,
          color = "black",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        # Configure tooltip content
        label = ~sprintf(
          "<strong>Country/Region:</strong> %s<br><strong>Year:</strong> %d<br><strong>Price Inflation Rate:</strong> %s",
          ADMIN, input$year, ifelse(is.na(Food_Price_Inflation), "No data", paste0(round(Food_Price_Inflation, 2), "%"))
        ) %>% lapply(HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "18px",
          direction = "auto"
        )
      ) %>%
      # Add legend to explain color 
      addLegend(
        position = "bottomleft",
        colors = legend_colors,
        labels = styled_labels,
        title = sprintf("<span style='font-size: 18px;'>Price Inflation Rate (Year: %d)</span>", input$year),
        opacity = 1
      ) %>% 
      # Add year display in top right corner
      addControl(
        html = sprintf(
          '<div style="font-size: 48px; color: black; text-shadow: 2px 2px 4px rgba(255,255,255,0.8);
          font-weight: bold; background: transparent;">Year: %d</div>',
          input$year
        ),
        position = "topright"
      )
  })
  
  # Update country selection dropdown
  observe({
    countries <- unique(df_price$Country)
    # ensure World Median option in the first option for drag down lists
    countries <- c("World Median", setdiff(countries, "World Median"))
    
    updateSelectizeInput(session, "selectedCountry",
                         choices = countries,
                         selected = selected_country())
  })
  
  # Implement debouncing for map clicks to prevent multiple rapid selections
  last_click_time <- reactiveVal(Sys.time() - 1)
  
  # When map is clicked, update selected country with debouncing
  observeEvent(input$map_shape_click, {
    # Get current time
    current_time <- Sys.time()
    # Get time difference in milliseconds
    time_diff <- as.numeric(difftime(current_time, last_click_time(), units = "secs")) * 1000
    
    # Only process click if more than 1200ms have passed (i.e. debouncing) since last click
    if (time_diff > 1200) {
      clicked_country <- input$map_shape_click$id
      
      if (!is.null(clicked_country)) {
        if (clicked_country == selected_country()) {
          # If clicking already selected country, switch to World Median
          selected_country("World Median")
        } else {
          # Select new country
          selected_country(clicked_country)
        }
      }
      
      # Update last click time
      last_click_time(current_time)
    }
  })
  
  # Sync country selection between map and dropdown
  observe({
    country <- selected_country()
    updateSelectizeInput(session, "selectedCountry", selected = country)
    input$year # Dependency to ensure map updates
  })
  
  # Handle country selection from dropdown lists
  observeEvent(input$selectedCountry, {
    if (!identical(input$selectedCountry, selected_country())) {
      selected_country(input$selectedCountry)
    }
  })
  
  #### coding for the selection with economic indicators line chart 
  # Prepare initial chart data for World Median
  initial_chart_data <- reactive({
    df_price %>%
      filter(
        Country == "World Median",
        Item == input$selectedItem
      )
  })
  
  # Prepare chart data for selected country
  chart_data <- reactive({
    df_price %>%
      filter(
        Country == input$selectedCountry,
        Item == input$selectedItem
      )
  })
  
  # Render economic indicators line chart
  output$lineChart <- renderPlotly({
    # Use either the initial data or the updated data based on user selection
    data <- if (is.null(input$selectedCountry)) {
      initial_chart_data()
    } else {
      chart_data()
    }
    
    
    # Display message at the center of the chart if no data available for selected country
    if (nrow(data) == 0 && input$selectedCountry != "World Median") {
      plot_ly() %>%
        layout(
          title = list(text = "Trends of food price indicators, production indicators, and economic indicators",
                       font = list(size = 20)),
          xaxis = list(title = "Year", font = list(size = 18)),
          yaxis = list(title = "Rate (%)", font = list(size = 18)),
          showlegend = TRUE,
          annotations = list(
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            text = "No data available. Please select another country.",
            showarrow = FALSE,
            font = list(size = 20)
          ),
          margin = list(t = 80, r = 80, b = 50, l = 80)
        )
    } else {
      # Create base plot with common layout settings
      p <- plot_ly() %>%
        layout(
          title = list(text = sprintf("Trends of food price indicators, production indicators, and economic indicators (%s)", input$selectedCountry),
                       font = list(size = 20)
                       ),
          xaxis = list(title = list(text = "Year", font = list(size = 18)),
                       range = c(1999, 2026),
                       gridwidth = 0.7,
                       gridcolor = '#cccccc',
                       tickvals = seq(2000, 2025, by = 5),  #define axis every 5 years
                       ticktext = seq(2000, 2025, by = 5),  #show axis label every 5 years
                       tickfont = list(size = 16)
                       ),
          yaxis = list(title = list(text = "Rate (%)", font = list(size = 18)),
                       gridwidth = 0.7,
                       gridcolor = '#cccccc',
                       tickfont = list(size = 16)
                       ),
          showlegend = TRUE,
          legend = list(font = list(size = 18)),
          margin = list(t = 80, r = 80, b = 50, l = 80)
        )
      
      ## Add selected indicator lines to plot
      # add Food Price Inflation line
      if ("Food_Price_Inflation" %in% input$selectedLines) {
        p <- p %>% add_trace(
          data = data,
          x = ~Year,
          y = ~Food_Price_Inflation,
          name = "Food Price Inflation Rate",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#AA4499'),
          marker = list(color = '#AA4499'),
          # configure hover over tooltip
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>%{fullData.name}</b>: %{y:.2f}%",
            "<br><b>Year</b>: %{x}",
            "<extra></extra>"
          )
        )
      }
      
      # add PPI Growth Rate line
      if ("ppi_rate" %in% input$selectedLines) {
        p <- p %>% add_trace(
          data = data,
          x = ~Year,
          y = ~ppi_rate,
          name = "PPI Growth Rate",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#FF1A1A'),
          marker = list(color = '#FF1A1A'),
          # configure hover over tooltip
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>%{fullData.name}</b>: %{y:.2f}%",
            "<br><b>Year</b>: %{x}",
            "<extra></extra>"
          )
        )
      }
      
      # add GPI Growth Rate line
      if ("gpi_rate" %in% input$selectedLines) {
        p <- p %>% add_trace(
          data = data,
          x = ~Year,
          y = ~gpi_rate,
          name = "GPI Growth Rate",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#88CCEE'),
          marker = list(color = '#88CCEE'),
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>%{fullData.name}</b>: %{y:.2f}%",
            "<br><b>Year</b>: %{x}",
            "<extra></extra>"
          )
        )
      }
      
      # add GDP Growth Rate line
      if ("gdp_rate" %in% input$selectedLines) {
        p <- p %>% add_trace(
          data = data,
          x = ~Year,
          y = ~gdp_rate,
          name = "GDP Growth Rate",
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#DDCC77'),
          marker = list(color = '#DDCC77'),
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>%{fullData.name}</b>: %{y:.2f}%",
            "<br><b>Year</b>: %{x}",
            "<extra></extra>"
          )
        )
      }
      p # display line chart
    }
  })
  
  # Prepare radar chart data for health indicators visualization
  radar_data <- reactive({
    # Get base data (World Median) for comparison
    base_data <- df_health %>%
      filter(Country == "World Median") %>%
      select(adult_obesity, teen_obesity, child_obesity, child_stunting, Food_Price_Inflation)
    
    # Get comparison data for selected country
    if (input$healthCountry != "World Median") {
      comparison_data <- df_health %>%
        filter(Country == input$healthCountry) %>%
        select(adult_obesity, teen_obesity, child_obesity, child_stunting, Food_Price_Inflation)
      
      # Check if comparison data exists and has no missing values
      if (nrow(comparison_data) > 0 && !any(is.na(comparison_data))) {
        return(list(base = base_data, comparison = comparison_data, has_data = TRUE))
      } else {
        return(list(base = base_data, has_data = FALSE))
      }
    }
    return(list(base = base_data, has_data = TRUE))
  })
  
  # Define color scheme for consistency across charts
  chart_colors <- list(
    world_median = "#0077BB",
    comparison = "#EE7733"
  )
  
  # Render radar chart for health indicators
  output$radarChart <- renderPlotly({
    data <- radar_data()
    
    # Display message if no data available for selected country
    if (!data$has_data && input$healthCountry != "World Median") {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Obesity & Stunting Levels (Year: 2022)", font = list(size = 20)),
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "No health indicators data available for this country",
              showarrow = FALSE,
              font = list(size = 20)
            ),
            margin = list(t = 80, r = 50, b = 80, l = 50)
          )
      )
    }
    
    # Define display names for radar chart axes
    axis_config <- list(
      adult_obesity = "Adult Obesity Rate",
      teen_obesity = "Children/Adolescents (5-18 years) Obesity Rate",
      child_obesity = "Children (<5 years) Obesity Rate",
      child_stunting = "Children (<5 years) Stunting Rate",
      Food_Price_Inflation = "Food Price Inflation Rate"
    )
    
    # Map column names to display names
    original_names <- names(data$base)
    display_names <- unname(axis_config[original_names])
    
    # Calculate range for radial axis (adding 10% padding to ensure data can be displayed)
    max_value <- max(c(unlist(data$base), 
                       if (!is.null(data$comparison)) unlist(data$comparison) else 0))
    range_max <- max_value * 1.1
    
    # Initialize radar chart with World Median data
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        type = 'scatterpolar',
        r = round(as.numeric(data$base[1,]), 2),
        theta = display_names,
        name = 'World Median',
        fill = 'toself',
        fillcolor = paste0(substr(chart_colors$world_median, 1, -2), ", 0.2)"),
        line = list(color = chart_colors$world_median, width = 3),
        marker = list(size = 10, color = chart_colors$world_median),
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
        hovertemplate = paste(
          "<b>World Median</b>",
          "<br><b>%{theta}</b>: %{r}%",
          "<extra></extra>"
        ),
        hoveron = 'points'  # only points have hover over
      )
    
    # Add comparison country data if available
    if (!is.null(data$comparison)) {
      p <- p %>%
        add_trace(
          type = 'scatterpolar',
          r = round(as.numeric(data$comparison[1,]), 2),
          theta = display_names,
          name = input$healthCountry,
          fill = 'toself',
          fillcolor = paste0(substr(chart_colors$comparison, 1, -2), ", 0.2)"),
          line = list(color = chart_colors$comparison, width = 3),
          marker = list(size = 10, color = chart_colors$comparison),
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>Country</b>:", input$healthCountry,
            "<br><b>%{theta}</b>: %{r}%",
            "<extra></extra>"
          ),
          hoveron = 'points'  # only at points have hover over
        )
    }
    
    # Configure radar chart layout
    p %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, range_max),
          tickfont = list(size = 14)
        ),
        angularaxis = list(tickfont = list(size = 18)),
        domain = list(x = c(0.1, 0.9), y = c(0.1, 0.9))  # Adjust chart size within container
      ),
      showlegend = TRUE,
      title = list(text = "Obesity & Stunting Levels (Year: 2022)", font = list(size = 20)),
      legend = list(x = 0, y = -0.1, font = list(size = 18)), 
      margin = list(t = 80, r = 50, b = 80, l = 50)
    )
  })
  
  # Prepare data for life satisfaction trend line chart
  line2_data <- reactive({
    # Calculate World Median life satisfaction scores
    base_data <- df_price %>%
      filter(Country == "World Median") %>%
      group_by(Year) %>%
      summarise(Life_Ladder = mean(Life_Ladder, na.rm = TRUE),
                .groups = 'drop')
    
    # Get comparison country data
    if (input$healthCountry != "World Median") {
      comparison_data <- df_price %>%
        filter(Country == input$healthCountry) %>%
        select(Year, Life_Ladder)
      
      # Check if comparison data exists and has valid values
      if (nrow(comparison_data) > 0 && !all(is.na(comparison_data$Life_Ladder))) {
        return(list(base = base_data, comparison = comparison_data, has_data = TRUE))
      } else {
        return(list(base = base_data, has_data = FALSE))
      }
    }
    
    return(list(base = base_data, has_data = TRUE))
  })
  
  # Render life satisfaction trend line chart
  output$lineChart2 <- renderPlotly({
    data <- line2_data()
    
    # Display message if no data available for selected country
    if (!data$has_data && input$healthCountry != "World Median") {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Trend of Life Satisfaction Score", font = list(size = 20)),
            showlegend = FALSE,
            annotations = list(
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "No life satisfaction data available for this country",
              showarrow = FALSE,
              font = list(size = 20)
            ),
            margin = list(t = 80, r = 80, b = 50, l = 80)
          )
      )
    }
    
    # Create base plot with World Median data
    p <- plot_ly() %>%
      add_trace(
        data = data$base,
        x = ~Year,
        y = ~Life_Ladder,
        name = 'World Median',
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = chart_colors$world_median, width = 3),
        marker = list(size = 10, color = chart_colors$world_median),
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
        hovertemplate = paste(
          "<b>World Median</b>",
          "<br><b>Year</b>: %{x}",
          "<br><b>Life Satisfaction Score</b>: %{y:.2f}",
          "<extra></extra>"
        )
      )
    
    # Add comparison country data if available
    if (!is.null(data$comparison)) {
      p <- p %>%
        add_trace(
          data = data$comparison,
          x = ~Year,
          y = ~Life_Ladder,
          name = input$healthCountry,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = chart_colors$comparison, width = 3),
          marker = list(size = 10, color = chart_colors$comparison),
          hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 18)),
          hovertemplate = paste(
            "<b>%{fullData.name}</b>",
            "<br><b>Year</b>: %{x}",
            "<br><b>Life Satisfaction Score</b>: %{y:.2f}",
            "<extra></extra>"
          )
        )
    }
    
    # Configure line chart layout
    p %>% layout(
      title = list(text = "Trend of Life Satisfaction Score", font = list(size = 20)),
      xaxis = list(title = list(text = "Year", font = list(size = 18)), 
                   range = c(2004, 2026),
                   gridwidth = 0.7,
                   gridcolor = '#cccccc',
                   tickvals = seq(2005, 2025, by = 5), #show axis ticks for line charts
                   ticktext = seq(2005, 2025, by = 5), #show axis label every 5 years
                   tickfont = list(size = 16)),
      yaxis = list(title = list(text = "Life Ladder Score", font = list(size = 18)), 
                   tickfont = list(size = 16),
                   gridwidth = 0.7,
                   gridcolor = '#cccccc'),
      showlegend = TRUE,
      legend = list(x = 0, y = -0.3, font = list(size = 18)),
      margin = list(t = 80, r = 80, b = 50, l = 80)
    )
  })
  
  # Keep health country selector in sync with main country selection
  observe({
    countries <- unique(df_health$Country)
    # Ensure World Median is the first option
    countries <- c("World Median", setdiff(countries, "World Median"))
    
    updateSelectizeInput(session, "healthCountry",
                         choices = countries,
                         selected = selected_country())
  })
  
  # Update health country selection when a country is clicked on the map
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!is.null(clicked_country)) {
      updateSelectizeInput(session, "healthCountry", selected = clicked_country)
    }
  })
  
  # Update selected country when health country drag down lists changes
  observeEvent(input$healthCountry, {
    if (!identical(input$healthCountry, selected_country())) {
      selected_country(input$healthCountry)
    }
  })
}

# run the application
shinyApp(ui = ui, server = server)
