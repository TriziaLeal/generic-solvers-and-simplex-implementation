source("polynomialregression.r")
source("qsi.r")
source("simplex.r")

server <- function(input, output) {
  observeEvent(input$solver, {
    print(paste0("You have chosen: ", input$solver))
  })

  observeEvent(input$solveRegression,{
      print(input$file_input_regression)
      if(!is.null(input$file_input_regression)){
        print(input$file_input_regression$datapath)
        xy_data <- read.csv(file = input$file_input_regression$datapath)
        output$xy_regression <- renderRHandsontable({
          rhandsontable(xy_data,overflow="visible",readOnly = TRUE)
        })
        result = PolynomialRegression(xy_data$x, xy_data$y, input$degreeInput)
        estimate = result$func(input$estimate)
        f = deparse(result$func)
        print(length(f))
        output$head_function_regression <- renderText({
          "Function"
        })
        output$function_regression <- renderPrint({
          paste0("f(x) = ",f[2:length(f)])
        })
        output$head_estimate_regression <- renderText({
          "Estimate"
        })
        output$estimate_regression <- renderText({
          paste0("f(",input$estimate,") = ",estimate)
        })
        output$plot1 <- renderPlot({
          plot(xy_data,col="red")
          model = lm(xy_data$y~poly(xy_data$x,input$degreeInput))
          lines(xy_data$x,predict(model),col="green")
        })
        #print(input$estimate)
        #print(estimate)
        data = data.frame(xy_data)
      }
  })
  observeEvent(input$solveQSI,{
    print(input$file_input_qsi)
    if(!is.null(input$file_input_qsi)){
      print(input$file_input_qsi$datapath)
      xy_data <- read.csv(file = input$file_input_qsi$datapath)
      output$xy_qsi <- renderRHandsontable({
        rhandsontable(xy_data,overflow="visible",readOnly = TRUE)
      })
      result = qsi(input$est,xy_data$x, xy_data$y)
      print(result)
      data = data.frame(xy_data)
      output$head_functions <- renderText({
        "Functions per Interval"
      })
      output$functions_interval <- renderPrint({
        result$intervalFunctions
      })
      output$head_function <- renderText({
        "Function"
      })
      output$function_interval <- renderPrint({
        result$intervalFunctions[result$interval]
      })
      output$head_est <- renderText({
        "Estimate"
      })   
      output$est <- renderPrint({
        result$f(input$est)
      })
      output$plot2 <- renderPlot({
        plot(xy_data,col="red")
        model = lm(xy_data$y~poly(xy_data$x,5,raw=TRUE))
        lines(xy_data$x,predict(model),col="green")
      })
    }
  })
  
  output$shipping_data <- renderRHandsontable({
    colnames = c("Total","California","Utah","New Mexico","Illinois","New York")
    rownames = c("Denver","Phoenix","Dallas")
    m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))
    shipping_data <- data.frame(m)
    rhandsontable(shipping_data,readOnly = TRUE,width = 600,rowHeaderWidth = 150,stretchH = "all")
  })
  output$demand_data <- renderRHandsontable({
    colnames = c("California","Utah","New Mexico","Illinois","New York")
    rownames = c("Demand")
    m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))  
    demands_data <- data.frame(m)
    rhandsontable(demands_data,width = 600,rowHeaderWidth = 150,stretchH = "all")
  })
  output$supply_data <- renderRHandsontable({
    colnames = c("Denver","Phoenix","Dallas")
    rownames = c("Supply")
    m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))  
    supply_data <- data.frame(m)
    rhandsontable(supply_data,width = 600,rowHeaderWidth = 150,stretchH = "all")
  })
  output$shipping_cost_data <- renderRHandsontable({
    colnames = c("California","Utah","New Mexico","Illinois","New York")
    rownames = c("Denver","Phoenix","Dallas")
    m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))  
    cost_data <- data.frame(m)
    rhandsontable(cost_data,width = 600,rowHeaderWidth = 150,stretchH = "all")
  })
  output$total_data <- renderRHandsontable({
    colnames = c("California","Utah","New Mexico","Illinois","New York")
    rownames = c("Total")
    m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))  
    total_data <- data.frame(m)
    rhandsontable(total_data,width = 600,rowHeaderWidth = 150,stretchH = "all")
  })

  observeEvent(input$solveSimplex,{
    demand_data = hot_to_r( input$demand_data )
    supply_data = hot_to_r( input$supply_data )
    cost_data = hot_to_r( input$shipping_cost_data )

    denverCosts = as.numeric(cost_data["Denver",])
    phoenixCosts = as.numeric(cost_data["Phoenix",])
    dallasCosts = as.numeric(cost_data["Dallas",])

    costsMatrix = matrix(nrow=3,ncol=5)
    costsMatrix[1,] =  denverCosts
    costsMatrix[2,] =  phoenixCosts
    costsMatrix[3,] =  dallasCosts
    
    totalcost = c(sum(costsMatrix[,1]),sum(costsMatrix[,2]),sum(costsMatrix[,3]),sum(costsMatrix[,4]),sum(costsMatrix[,5]))
    output$total_data <- renderRHandsontable({
      colnames = c("California","Utah","New Mexico","Illinois","New York")
      rownames = c("Total")
      m = matrix(totalcost,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))  
      total_data <- data.frame(m)
      rhandsontable(total_data,width = 600,rowHeaderWidth = 150,stretchH = "all")
    })
      
    d = as.numeric(demand_data["Demand",])
    s = as.numeric(supply_data["Supply",])


    result = simplex(costsMatrix,d,s)
    
    output$shipping_data <- renderRHandsontable({
      df <- data.frame(result$shipMatrix)
      rhandsontable(df,width=600,rowHeaderWidth = 150, stretchH="all")
    })
    print(result$tableau)
    output$tableau_i <- renderRHandsontable({
      tableau_i <- data.frame(result$tableau[[input$iter]])
      
      rhandsontable(tableau_i,width=1250,rowHeaderWidth = 50, stretchH="all")
    })
  })


}