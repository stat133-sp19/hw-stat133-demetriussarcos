library(shiny)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Savings Simulator"),

  fluidRow(
    column(3, sliderInput("initial",
                          "Initial Amount",
                          min = 1,
                          max = 100000,
                          value = 1000,
                          step = 500,
                          pre = "$",
                          sep = ","),
           sliderInput("contrib",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500,
                       pre = "$",
                       sep = ",")),
    column(3, sliderInput("return",
                          "Return Rate (in %)",
                          min = 0,
                          max = 20,
                          value = 5,
                          step = 0.1),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1)),
    column(3, sliderInput("years",
                          "Years",
                          min = 0,
                          max = 50,
                          value = 20,
                          step = 1),
           selectInput("facet",
                       "Facet?",
                       choices = list("No",
                                      "Yes"),
                       selected = "No"))
  ),
  hr(),
  h4("Timelines"),
  plotOutput("plot", width = "80%"),
  h4("Balances"),
  verbatimTextOutput("df")
)

server <- function(input, output) {
  
  dat <- reactive({
    # Investment Functions
    future_value <- function(amount, rate, years) {
      amount * ((1 + rate) ^ years)
    }
    annuity <- function(contrib, rate, years){
      (contrib/rate) * (((1 + rate) ^ years) - 1)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      (contrib/(rate - growth)) * ((1+rate)^years - (1+growth)^years)
    }
    
    # For-Loop
    no_contrib <- rep(0, input$years)
    fixed_contrib <- rep(0, input$years)
    growing_contrib <- rep(0, input$years)
    year <- 0:input$years
    
    for (t in 1:(max(input$years))) {
      FV <- future_value(input$initial, input$return/100, t)
      FVA <- FV + annuity(input$contrib, input$return/100, t)
      FVGA <- FV + growing_annuity(input$contrib, input$return/100, input$growth/100, t)
      no_contrib[1] <- input$initial
      fixed_contrib[1] <- input$initial
      growing_contrib[1] <- input$initial
      no_contrib[t+1] <- FV
      fixed_contrib[t+1] <- FVA
      growing_contrib[t+1] <- FVGA
    }
    
    dat <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    return(dat)
  })
  
  output$plot <- renderPlot({
    # Reshaping dat
    shaped_dat <- reshape2::melt(dat(), id.vars = 'year')
    
    # Plot
    timeline <- ggplot(shaped_dat, aes(x = year, value, color = variable)) +
      geom_line() +
      geom_point() +
      labs(subtitle = "Three modes of investing", 
           color = "Modality") +
      xlab("Year") +
      ylab("Value (in dollars)")
    
    if (input$facet == "Yes") {
      timeline <-  ggplot(shaped_dat, aes(year, value, color = variable, fill = variable)) +
        geom_area(stat = "identity", alpha = 0.5) +
        geom_line() +
        geom_point() +
        facet_wrap(~variable) +
        labs(subtitle = "Three modes of investing") +
        xlab("Year") +
        ylab("Value (in dollars)") +
        theme_bw()
    }
    return(timeline)
  })
  output$df <- renderPrint({
  return(dat())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

