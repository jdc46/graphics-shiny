library(shiny)
library(ggplot2)
library(plotly)
library(colourpicker)

ui <- fluidPage(
  
  titlePanel("Data Display Studio"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Upload a Data File",
        accept = c(".csv", ".txt")
      ),
      
      selectInput(
        "delimiter",
        "Delimiter",
        choices = c(
          "Auto Detect" = "auto",
          "Comma" = ",",
          "Tab" = "\t",
          "Whitespace" = "ws"
        ),
        selected = "auto"
      ),
      
      uiOutput("var_select"),
      
      selectInput(
        "plotType",
        "Plot Type",
        choices = c(
          "Scatterplot",
          "Histogram",
          "Boxplot"
        )
      ),
      
      selectInput(
        "theme",
        "Theme",
        choices = c(
          "Minimal",
          "Classic",
          "Dark"
        )
      ),
      
      colourInput(
        "color",
        "Color",
        value = "#2C7FB8"
      )
    ),
    
    mainPanel(
      plotlyOutput("plot"),
      hr(),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session){
  
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    delim <- input$delimiter
    # Auto-detect delimiter
    if (delim == "auto") {
      first_line <- readLines(file, n = 1)
      if (grepl(",", first_line)) {
        delim <- ","
      } else if (grepl("\t", first_line)) {
        delim <- "\t"
      } else {
        delim <- "ws"
      }
    }
    
    # Read file using selected/detected delimiter
    if (delim == ",") {
      read.csv(file, header = TRUE)
    } else if (delim == "\t") {
      read.delim(file, header = TRUE)
    } else {
      read.table(
        file,
        header = TRUE,
        sep = "",
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$preview <- renderTable({
    head(data())
  })
  
  output$var_select <- renderUI({
    df <- data()
    
    tagList(
      selectInput(
        "response",
        "Response Variable",
        choices = c("None" = "None",
                    setNames(names(df), names(df))),
        selected = "None"
      ),
      
      selectInput(
        "predictor",
        "Predictor Variable",
        choices = c("None" = "None",
                    setNames(names(df), names(df))),
        selected = "None"
      )
    )
  })
  
  output$plot <- renderPlotly({
    req(data())
    req(input$response)
    df <- data()
    p <- NULL
    if(input$plotType == "Scatterplot"){
      req(input$predictor != "")
      validate(
        need(input$predictor != "", "Please select a predictor variable")
      )
      p <- ggplot(
        df,
        aes(
          x = .data[[input$predictor]],
          y = .data[[input$response]]
        )
      ) +
        geom_point(
          color = input$color,
          size = 3
        )
    }
    
    if(input$plotType == "Histogram"){
      p <- ggplot(
        df,
        aes(
          x = .data[[input$response]]
        )
      ) +
        geom_histogram(
          fill = input$color,
          bins = 20
        )
    }
    
    if(input$plotType == "Boxplot") {
      
      validate(
        need(input$response != "None", "Please select a response variable.")
      )
      
      if(input$predictor == "None") {
        
        # Single boxplot
        p <- ggplot(df, aes(y = .data[[input$response]])) +
          geom_boxplot(fill = input$color)
        
      } else {
        
        # Grouped boxplots
        p <- ggplot(df,
          aes(
            x = factor(.data[[input$predictor]]),
            y = .data[[input$response]]
          )
        ) +
          geom_boxplot(fill = input$color)
      }
    }
    
    if(input$theme == "Minimal"){
      p <- p + theme_minimal()
    }
    
    if(input$theme == "Classic"){
      p <- p + theme_classic()
    }
    
    if(input$theme == "Dark"){
      p <- p + theme_dark()
    }
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
