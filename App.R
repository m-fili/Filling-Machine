library(shiny)

# Define UI for the app
ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  titlePanel(HTML("<h1 
                  style='font-size: 28px; 
                  font-family: Arial, sans-serif; 
                  font-weight: bold; 
                  text-align: left;
                  '>Filling Machine: MF Engine Oil
                  </h1>")),

  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Get a Batch of 200 Bottles"),
      actionButton("reset", "Reset"), 
      downloadButton("downloadData", "Download CSV File"),
      br(),
      br(),
      textOutput("instruction"),
      fluidRow(column(6, offset = 3, tableOutput("measurements")))
    ),
    
    
    
    mainPanel(
      
      # Add an image at the top of the page
      tags$div(
        style = "text-align: center; margin-bottom: 20px;",
        tags$img(src = "cover.png", height = "350px")  # Change "figure.png" to your image file
      ),
      
      plotOutput("bottlePlot", click = "plot_click") 
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive value to store the batch volumes persistently
  batch_volumes <- reactiveValues(data = NULL)
  
  # Reactive value to store the sampled bottles' volumes
  volumes <- reactiveValues(
    data = data.frame(Sample = integer(), Bottle = integer(), Volume = numeric()),
    counter = 0
  )
  
  # Reactive value to store clicked bottles (for coloring)
  clicked_bottles <- reactiveValues(selected = rep(FALSE, 200))
  
  # Generate a new batch when the button is clicked
  observeEvent(input$generate, {
    batch_volumes$data <- rnorm(n=200, mean = 3.70, sd = 0.05)  # Generate batch of 200 bottles
    clicked_bottles$selected <- rep(FALSE, 200)  # Reset click tracking
    volumes$data <- data.frame(Sample = integer(), Bottle = integer(), Volume = numeric())  # Clear table
    volumes$counter <- 0  # Reset counter
  })
  
  # Plot 200 rectangles representing the bottles
  output$bottlePlot <- renderPlot({
    if (!is.null(batch_volumes$data)) {
      plot(NULL, xlab = "", ylab = "", xlim = c(0, 21), ylim = c(0, 11), xaxt = 'n', yaxt = 'n',
           main = "Click on a Container to Measure")
      
      index <- 1
      for (j in 10:1) {
        for (i in 1:20) {
          # Set color based on whether the bottle has been clicked
          color <- if (clicked_bottles$selected[index]) "darkgreen" else "lightblue"
          font_color <- if (clicked_bottles$selected[index]) "white" else "black"
          
          # Draw the rectangle with a black border for better contrast
          rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, col = color, border = "black", lwd = 2)
          
          # Add the index number
          text(i, j, labels = index, cex = 0.8, col = font_color)
          
          index <- index + 1
        }
      }
    }
  })
  
  # Capture the clicked bottle and measure its volume
  observeEvent(input$plot_click, {
    if (!is.null(batch_volumes$data)) {
      click_correction = 0.5
      click_x <- input$plot_click$x + click_correction
      click_y <- input$plot_click$y + click_correction
      
      if (!is.null(click_x) && !is.null(click_y)) {
        # Compute bottle coordinates
        bottle_x <- floor(click_x)
        bottle_y <- floor(click_y)
        
        # Convert to bottle index (ensuring valid range)
        if (bottle_x >= 1 && bottle_x <= 20 && bottle_y >= 1 && bottle_y <= 10) {
          bottle_number <- (10 - bottle_y) * 20 + bottle_x
          
          if (bottle_number >= 1 && bottle_number <= 200) {
            # Prevent duplicate clicks
            if (!clicked_bottles$selected[bottle_number]) {
              clicked_bottles$selected[bottle_number] <- TRUE
              
              volumes$counter <- volumes$counter + 1
              volumes$data <- rbind(volumes$data, data.frame(
                Sample = paste0("#", volumes$counter),
                Bottle = as.integer(bottle_number),  
                Volume = round(batch_volumes$data[bottle_number], 3)
              ))
            }
          }
        }
      }
    }
  })
  
  # Output the measured volumes in a table
  output$measurements <- renderTable({
    volumes$data
  }, digits = 3)
  
  # Reset all data when the reset button is clicked
  observeEvent(input$reset, {
    volumes$data <- data.frame(Sample = integer(), Bottle = integer(), Volume = numeric())
    volumes$counter <- 0
    clicked_bottles$selected <- rep(FALSE, 200)
  })
  
  # Download measured data as CSV
  output$downloadData <- downloadHandler(
    filename = function() { "FillingMachine_Measurements.csv" },
    content = function(file) {
      write.csv(volumes$data, file, row.names = FALSE)
    }
  )
  
  # Improved instructional text
  output$instruction <- renderText({
    if (is.null(batch_volumes$data)) {
      "Click 'Generate' to get a batch of engine oil bottles"
    } else if (nrow(volumes$data) == 0) {
      "Click on a bottle to measure its volume."
    } else {
      paste0("You have measured ", nrow(volumes$data), " bottles. Click more or reset.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
