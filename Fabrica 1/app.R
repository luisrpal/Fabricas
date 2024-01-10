library(shiny)
library(shinydashboard)
# Section One ---------------------------------

# Define UI
ui <- fluidPage(
  titlePanel("Fabrica 1"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("input1", "Factor 1:", value = 0),
      numericInput("input2", "Factor 2:", value = 0),
      numericInput("input3", "Factor 3:", value = 0),
      numericInput("input4", "Factor 4:", value = 0),
      numericInput("input5", "Factor 5:", value = 0),
      numericInput("input6", "Factor 6:", value = 0),
      numericInput("input7", "Factor 7:", value = 0),
      actionButton("update_button", "Actualizar respuesta")
    ),
    
    mainPanel(
      h3("Table 1: Respuesta objetivo y sus especificaciones"),
      tableOutput("table1"),
      tabsetPanel(
        tabPanel("Table 1", h3("Factores y sus rangos historicos"), tableOutput("main_table1")),
        # tabPanel("Table 2", tableOutput("main_table2")),
        tabPanel("Bar Plot", plotOutput("bar_plot")),
        textOutput("message_text")
        
      )
    )
  )
)
# Section Two =================================

# Define server
#sc1=(input$input1-280)/20
server <- function(input, output) {
  # Initial non-editable table data
  initial_data <- matrix(as.integer(c(1,260,280,300,2,50,75,100,3,40,50,60,4,12,15,18,5,1,1,2,6,180,200,220,7,30,37,44)), nrow = 7, ncol = 4, byrow = TRUE)
  colnames(initial_data) <- c("Factor", "Mínimo histórico", "Media histórica", "Máximo histórico")
  rownames(initial_data) <- paste0("Row", 1:7)
  
  # Initial non-editable table data for Table 2
  initial_data_table1 <- matrix(c(73,77,81), nrow = 1, ncol = 3, byrow = TRUE)
  colnames(initial_data_table1) <- c("Límite inferior", "Objetivo","Límite superior")
  
  
  # Response function
  response_function <- function(input1, input2, input3, input4, input5, input6, input7) {
    result <-  55+
      ((input$input1-280)/20)*(-0.5)+                          #Factor 1
      ((input$input2-75)/25)*(-5)+                             #Factor 2
      ((input$input1-280)/20)*((input$input3-50)/10)*(0.45)+   #Interaccion 1,3
      ((input$input3-50)/10)*(0.01)+                           #Factor 3
      ((input$input3-50)/10)*((input$input4-15)/3)*(1.8)+      #Interaccion 3,4
      ((input$input2-75)/25)*((input$input3-50)/10)*(2.5)+     #Interaccion 2,3
      ((input$input4-15)/3)*(2.8)+                             #Factor 4
      ((input$input5-3)/2)*(0.01)+                             #Factor 5
      ((input$input6-200)/20)*(4)+                             #Factor 6
      ((input$input7-37)/7)*(0.1)+                             #Factor 7
      ((input$input1-280)/20)* ((input$input1-280)/20)*(1.6)+   #Factor 1^2
      rnorm(1,0,0.45)
    return(result)
  }
  
  # Reactive expression to calculate bar plot values
  bar_plot_data <- reactive({
    response_function(input$input1, input$input2, input$input3, input$input4, input$input5, input$input6, input$input7)
    
  })
  
  message_text <- reactive({
    if (bar_plot_data() < 73) {
      "Debajo del límite inferior de especificación"
    } else if (bar_plot_data() >= 73 && bar_plot_data() <= 81) {
      "Dentro de los limites de especificación"
    } else {
      "Por arriba del límite superior de especificación"
    }
  })
  
  # Render non-editable Table 1
  output$table1 <- renderTable({
    initial_data_table1
  }, editable = FALSE)
  
   # Render non-editable table based on initial data
  output$main_table1 <- renderTable({
    initial_data
  }, editable = FALSE)

  
  # Render bar plot based on input values
  output$bar_plot <- renderPlot({
    barplot(bar_plot_data(), main = "", col = "skyblue", ylab = "Sum of Inputs")
    mtext(message_text(), side = 3, line = 0.5, col = "red", cex = 1.2)
    mtext(paste("Respuesta:", (round(bar_plot_data(),2))), side = 1, line = 3, cex=1.2,col = "black")
    
  })
  
  
  
}

### Section Three ############################# 

# Run the application
shinyApp(ui, server)
