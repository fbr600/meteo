#https://deanattali.com/blog/building-shiny-apps-tutorial/
library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)

data <- read.csv("data/all.csv", stringsAsFactors = FALSE,sep="|",header=T)

dt     <- reshape2::melt(data,id="Time")
data$OutTemp <- as.numeric(data$OutTemp)
dt_day <- data %>% group_by(day) %>% summarize(OutTemp_min=min(OutTemp),OutTemp_mean=mean(OutTemp),OutTemp_max=max(OutTemp)) 

ui <- fluidPage(
  titlePanel("Meteors"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "Time",label = "periode",start = "2017-10-01",end = "2018-02-28"),
      selectInput(inputId = "variable",label = "meteor",choices=c("OutTemp","InTemp"))
    ),
    mainPanel(
      plotOutput("plot"),
      br(), br(),
      tableOutput("table")
    )
  )
)
server <- function(input, output) {
  filtered <- reactive({
    dt <- dt %>%
      filter(Time >= input$Time[1],
             Time <= input$Time[2],
             variable==input$variable) 
    dt$value <- as.numeric(dt$value)
    dt$Time <- as.POSIXct(dt$Time)
    dt
  })
  
  output$plot <- renderPlot({
    ggplot(filtered(),aes(Time,value))+geom_line()
  })
  
  output$table <- renderTable({
    dt <- filtered()
    dt$Time <- as.character(dt$Time)
    dt
  })
}
shinyApp(ui = ui, server = server)



