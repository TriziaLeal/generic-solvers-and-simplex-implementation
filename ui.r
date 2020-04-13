library(shiny)
library(shinythemes)
library(rhandsontable)
# Define UI for app   
ui <- tagList(
  navbarPage(
    theme = shinytheme("sandstone"),
    "CMSC 150",
    tabPanel("Polynomial Regression",
      sidebarPanel(
        fileInput( 
          "file_input_regression", 
          h4("File input"), 
          accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        numericInput("degreeInput","Input Polynomial Degree",1),
        numericInput("estimate","Input Value to Estimate",1),
        actionButton("solveRegression", "Solve Regression")

      ),
      mainPanel(
        #textOutput("fileWarning"),
        rHandsontableOutput("xy_regression"),
        textOutput("head_function_regression"),
        verbatimTextOutput("function_regression"),
        textOutput("head_estimate_regression"),
        verbatimTextOutput("estimate_regression"),
        plotOutput("plot1")
      )
    ),
    tabPanel("Quadratic Spline Interpolation",
         sidebarPanel(
           fileInput( 
             "file_input_qsi", 
             h4("File input"), 
             accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")
           ),
           numericInput("est","Input Value to Estimate",1),
           actionButton("solveQSI", "Solve")
           
         ),
         mainPanel(
           #textOutput("fileWarning"),
           rHandsontableOutput("xy_qsi"),
           textOutput("head_functions"),
           verbatimTextOutput("functions_interval"),
           textOutput("head_function"),
           verbatimTextOutput("function_interval"),
           textOutput("head_est"),
           verbatimTextOutput("est"),
           plotOutput("plot2")
         )
      ),
    tabPanel("Simplex",
        h2("Fairways Woods Company Shipping Analysis"),
        h4("Number to ship from plant to warehouse"),
        rHandsontableOutput("shipping_data"),
        br(),br(), 
        rHandsontableOutput("demand_data"),
        br(),br(),
        rHandsontableOutput("supply_data"),
        br(),br(),
        h4("Shipping costs from plant to warehouse"),
        rHandsontableOutput("shipping_cost_data"),
        rHandsontableOutput("total_data"),
        br(),br(),
        
        actionButton("solveSimplex", "Optimize"),
        br(),br(),
        numericInput("iter","Input iteration number",1),
        rHandsontableOutput("tableau_i")
    )
  )
)