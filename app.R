library(shiny)
library(ggplot2)
library(dplyr)
library(bslib) # For a modern look


# code run in the console to export the shiny results
# shinylive::export(appdir = ".", destdir = "docs")


# UI Definition
ui <- fluidPage(
  titlePanel("Agricultural Drone Spraying Cost Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Field & Farm Parameters"),
      numericInput("farm_size", "Total Farm Size (Acres):", value = 500, min = 10),
      numericInput("time_window", "Completion Time Window (Hours):", value = 98, min = 1),
      
      h4("Drone Technical Specs"),
      numericInput("tank_vol", "Tank Capacity (Gallons):", value = 8, min = 1),
      numericInput("swath_width", "Swath Width (ft):", value = 20, min = 5),
      numericInput("flight_speed", "Flight Speed (mph):", value = 15, min = 1),
      numericInput("recharge_time", "Battery Swap/Recharge (mins):", value = 10, min = 1),
      
      h4("Economic Parameters"),
      numericInput("drone_cost", "Cost per Drone ($):", value = 25000, min = 0),
      numericInput("labor_rate", "Labor Rate ($/hour):", value = 30, min = 0),
      numericInput("fuel_maint", "Fuel Costs ($/gallon):", value = 2.50, min = 0),
      
      hr(),
      helpText("This simulation assumes a mechanistic workflow of spray-ferry-recharge cycles.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cost Analysis", 
                 plotOutput("costPlot"),
                 br(),
                 wellPanel(
                   h4("Summary Statistics"),
                   textOutput("fleet_size"),
                   textOutput("final_cost")
                 )
        ),
        tabPanel("Assumptions",
                 p("1. The model assumes rectangular fields with optimized spray paths."),
                 p("2. Fleet size is calculated to meet the user-defined time window."),
                 p("3. Ownership costs are annualized over a 5-year lifespan with a 10% salvage value.")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  calc_metrics <- reactive({
    # 1. Calculate Field Efficiency (Mechanistic Approximation)
    # Coverage Rate (Acres/Hour) = (Speed * Swath * Efficiency) / 8.25
    # Including ferry and refill downtime
    raw_coverage <- (input$flight_speed * input$swath_width) / 8.25
    cycle_time_ratio <- 0.7 # Efficiency factor accounting for ferry/refill
    effective_coverage <- raw_coverage * cycle_time_ratio
    
    # 2. Determine Required Fleet Size
    total_hours_needed = input$farm_size / effective_coverage
    fleet_needed = ceiling(total_hours_needed / input$time_window)
    
    # 3. Calculate Costs
    # Fixed Costs (Annualized)
    annual_depreciation = (fleet_needed * input$drone_cost * 0.9) / 5
    # Variable Costs
    total_labor = (input$farm_size / effective_coverage) * input$labor_rate
    total_var = input$farm_size * input$fuel_maint
    
    total_cost = annual_depreciation + total_labor + total_var
    per_acre = total_cost / input$farm_size
    
    return(list(fleet = fleet_needed, per_acre = per_acre, coverage = effective_coverage))
  })
  
  output$fleet_size <- renderText({
    paste("Minimum Drone Fleet Size Required:", calc_metrics()$fleet, "units")
  })
  
  output$final_cost <- renderText({
    paste("Estimated Total Cost per Acre: $", round(calc_metrics()$per_acre, 2))
  })
  
  output$costPlot <- renderPlot({
    # Sensitivity Analysis: How cost changes with farm size
    sizes <- seq(100, max(input$farm_size * 2, 1000), length.out = 20)
    
    plot_data <- data.frame(Acres = sizes) %>%
      mutate(
        # Recalculate per acre cost for different sizes
        Fleet = ceiling((Acres / calc_metrics()$coverage) / input$time_window),
        Fixed = (Fleet * input$drone_cost * 0.9) / 5,
        Variable = (Acres / calc_metrics()$coverage) * input$labor_rate + (Acres * input$fuel_maint),
        PerAcre = (Fixed + Variable) / Acres
      )
    
    ggplot(plot_data, aes(x = Acres, y = PerAcre)) +
      geom_line(color = "#2c3e50", size = 1.2) +
      geom_point(aes(x = input$farm_size, y = calc_metrics()$per_acre), color = "red", size = 4) +
      labs(title = "Economies of Scale in Drone Spraying",
           y = "Cost Per Acre ($)",
           x = "Total Farm Size (Acres)") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar)
  })
}

shinyApp(ui = ui, server = server)