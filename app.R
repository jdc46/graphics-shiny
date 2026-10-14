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
        "Upload a CSV File",
        accept = ".csv"
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
    read.csv(input$file$datapath)
  })
  
  output$preview <- renderTable({
    head(data())
  })
  
  output$var_select <- renderUI({
    df <- data()
    tagList(
      selectInput(
        "x",
        "X Variable",
        choices = names(df)
      ),
      selectInput(
        "y",
        "Y Variable",
        choices = names(df)
      )
    )
  })
  
  output$plot <- renderPlotly({
    req(input$x)
    df <- data()
    p <- NULL
    if(input$plotType == "Scatterplot"){
      p <- ggplot(
        df,
        aes_string(
          x = input$x,
          y = input$y
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
        aes_string(
          x = input$x
        )
      ) +
        geom_histogram(
          fill = input$color,
          bins = 20
        )
    }
    
    if(input$plotType == "Boxplot"){
      p <- ggplot(
        df,
        aes_string(
          x = input$x,
          y = input$y
        )
      ) +
        geom_boxplot(
          fill = input$color
        )
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
