library(shiny)

ui <- fluidPage(
  # Input() functions
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, 
              min = 1, max = 100),
  textInput(inputId = "title", label = "Histogram of random values"),
  actionButton(inputId = "go", label = "Submit"),
  actionButton(inputId = "normal", label = "Normal"),
  actionButton(inputId = "uniform", label = "Uniform"),
  plotOutput(outputId = "hist"),
  plotOutput(outputId = "box"),
  verbatimTextOutput(outputId = "stats")
  # Output() functions
)

server <- function(input, output) {
  
  rv <- reactiveValues(data = rnorm(100))

  observeEvent(input$normal, {rv$data <- rnorm(100)})
  observeEvent(input$uniform, {rv$data <- runif(100)})
  
  observeEvent(input$go, {
    print(as.numeric((input$go)))
  })
  
  #data <- reactive({
  #  rnorm(input$num)
  #})
  
  data <- eventReactive(input$go, {rnorm(input$num)})
  
  output$box <- renderPlot({
    boxplot(rv$data)
  })
  
  output$hist <- renderPlot({
    hist(data(), main = isolate({input$title}))
  })
  
  output$stats <- renderPrint({
   summary(data()) 
  })
  
}

shinyApp(ui = ui, server = server)