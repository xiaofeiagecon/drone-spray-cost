library(shiny)
library(bslib) # For a modern look

# code run in the console to export the shiny results
# shinylive::export(appdir = ".", destdir = "docs")

# Define UI
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Drone Spraying Cost Calculator"),
  
  layout_sidebar(
    sidebar = sidebar(
      numericInput("acres", "Total Farm Size (Acres):", value = 100, min = 1),
      numericInput("fields", "Number of Fields:", value = 5, min = 1),
      numericInput("wage", "Labor Wage ($/hour):", value = 25, min = 0),
      hr(),
      helpText("Enter your farm parameters to estimate drone operating costs.")
    ),
    
    card(
      card_header("Estimated Total Cost"),
      h2(textOutput("total_cost")),
      p("This estimate includes labor and basic operational overhead."),
      full_screen = TRUE
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  output$total_cost <- renderText({
    # Example Logic: (Acres * 5) + (Fields * 10) + (Labor Hours * Wage)
    # Adjust this formula based on your specific research models
    base_op_cost <- input$acres * 5 
    field_transition_cost <- input$fields * 15
    estimated_labor_hours <- input$acres / 20 # Assuming 20 acres/hr
    total_labor <- estimated_labor_hours * input$wage
    
    total <- base_op_cost + field_transition_cost + total_labor
    paste0("$", format(round(total, 2), nsmall = 2))
  })
}

# Run the app
shinyApp(ui = ui, server = server)