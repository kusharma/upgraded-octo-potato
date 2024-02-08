#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("helpers.R")

ui <- fluidPage(
  titlePanel("Fitness App"),
  tabsetPanel(
    tabPanel("Panel 1",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_1", "Select Single Variable", choices= categorical_variables)
               ),
               mainPanel(
                 plotOutput("plot_1"),
                 DT::dataTableOutput("table_1")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_2", "Select Variable 1", choices= categorical_variables),
                 selectInput("var_3", "Select Variable 2", choices= NULL,
                             selected = "gender")
               ),
               mainPanel(
                 plotOutput("plot_2"),
                 DT::dataTableOutput("table_2")
               )
             )
    ),
    tabPanel("Panel 2",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_ids", "Select IDs", choices=id_vector,
                             selected = c("000001", "000042", "000117", "000221", "000300"),
                             multiple = TRUE
                 ),
                 selectInput("var_3a", "Select Single Variable", choices= numerical_variables),
                 sliderInput(inputId ="var_part3week",label = "Select weeks",
                             min = 0, max = 52, value = c(0,52)
                 )),
               mainPanel(
                 plotOutput("plot_3"),
                 DT::dataTableOutput("table_3")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_4", "Select Single Variable", choices= categorical_variables),
                 selectInput(
                   "var_5", "Select number of weeks", choices=week_vector,
                   multiple = TRUE
                 )
               ),
               mainPanel(
                 plotOutput("plot_4"),
                 DT::dataTableOutput("table_4")
               )
             )
    ),

    tabPanel("Panel 3",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_5a", "Select the first variable", choices= categorical_variables),
                 selectInput("var_6", "Select the second variable", choices= categorical_variables,
                             selected = "gender"),
                 sliderInput(inputId ="var_part5week",label = "Select weeks",
                             min = 0, max = 52, value = c(0,52)
                 )),
               mainPanel(
                 plotOutput("plot_5"),
                 DT::dataTableOutput("table_5")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_7", "Select the first variable", choices= categorical_variables),
                 selectInput("var_8", "Select the second variable", choices= NULL,
                             selected = "gender"),
                 selectInput("var_part6week", "Select weeks", choices=week_vector,
                             multiple = TRUE
                 )
               ),
               mainPanel(
                 plotOutput("plot_6"),
                 DT::dataTableOutput("table_6")
               )
             )
    )
  )
)

server <- function(input, output, session) {

  output$plot_1<-renderPlot({
    output_graph1(fitness_data=fitness_long,
                  variable=input$var_1)
  })

  output$table_1<-DT::renderDataTable({
    output_table1(fitness_data=fitness_long,
                  variable=input$var_1)
  })

  observe({
    updateSelectInput(
      session = session,
      inputId = "var_3",
      choices = categorical_variables[categorical_variables != input$var_2]
    )
  })

  output$plot_2<-renderPlot({
    req(input$var_3)
    output_graph2(fitness_data=fitness_long,
                  variable_1=input$var_2,
                  variable_2=input$var_3)
  })

  output$table_2<-DT::renderDataTable({
    req(input$var_3)
    output_table2(fitness_data=fitness_long,
                  variable_1=input$var_2,
                  variable_2=input$var_3)
  })

  output$plot_3<-renderPlot({
    #req(input$var_3)
    #req(input$var_part3week)
    output_graph3(fitness_data=fitness_long,
                  variable_3=input$var_3a,
                  variable_ids=input$var_ids,
                  variable_week=input$var_part3week[1]:input$var_part3week[2]) # vector that goes from min to max
  })

  output$table_3<-DT::renderDataTable({
    output_table3(fitness_data=fitness_long,
                  variable_ids=input$var_ids,
                  variable_3=input$var_3a,
                  variable_week=input$var_part3week[1]:input$var_part3week[2])
  })

  output$plot_4<-renderPlot({
    req(input$var_5)
    output_graph4(fitness_data=fitness_long,
                  variable_4=input$var_4,
                  variable_week=input$var_5)
  })

  output$table_4<-DT::renderDataTable({
    req(input$var_5)
    output_table4(fitness_data=fitness_long,
                  variable_4=input$var_4,
                  variable_week=input$var_5)
  })

  output$plot_5<-renderPlot({
    #req(input$var_6)
    output_graph5(fitness_data=fitness_long,
                  variable_5=input$var_5a,
                  variable_6=input$var_6,
                  variable_week=input$var_part5week[1]:input$var_part5week[2])
  })



  output$table_5<-DT::renderDataTable({
    #req(input$var_6)
    output_table5(fitness_data=fitness_long,
                  variable_5=input$var_5a,
                  variable_6=input$var_6,
                  variable_week=input$var_part5week)
  })

  observe({
    updateSelectInput(
      session = session,
      inputId = "var_8",
      choices = categorical_variables[categorical_variables != input$var_7]
    )
  })

  output$plot_6<-renderPlot({
    req(input$var_8)
    req(input$var_part6week)
    output_graph6(fitness_data=fitness_long,
                  variable_7=input$var_7,
                  variable_8=input$var_8,
                  variable_week=input$var_part6week)
  })



  output$table_6<-DT::renderDataTable({
    req(input$var_8)
    req(input$var_part6week)
    output_table6(fitness_data=fitness_long,
                  variable_7=input$var_7,
                  variable_8=input$var_8,
                  variable_week=input$var_part6week)
  })
}


shinyApp(ui = ui, server = server)



