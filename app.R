#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(gridExtra)
source('utilities.R')
# y <- 8; n <- 10; t=3; T=4; qtest = 0.8; alpha = 0.05; prior = 'Jeffries'
# plot_posterior(prior = prior,y=y,n=n,t=t,T=T,
#                qtest = qtest,alpha = alpha)
priors <- c('Jeffries','Flat', 'Bell')
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Mastery gained after multiple sessions"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput('y', 'Successful trials per session (comma delimited)', "6, 8, 9, 8"),
          textInput('n', 'Total trials per session (single value or comma delimited)', "10, 10, 10, 10"),
          numericInput("tau",
                      "Per session performance criterion (percentage)",
                      value = 80),
          selectInput("lr", "Keep learning from unsuccessful sessions?", c("Yes","No"),"No"),
          sliderInput("qtest",
                      "Desired mastery level",
                      min = 50,
                      max = 99,
                      value = 80,
                      step = 1),
          sliderInput("alpha",
                      "Desired confidence level",
                      min = 80,
                      max = 99,
                      value = 95,
                      step = 05),
        selectInput("prior", "Distribution of prior belief", priors)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
          h4("Posterior distribution of mastery:"),
          tags$ul(
            tags$li(textOutput('b')),
            tags$li(textOutput('theta_interval'))
          ),
          fluidRow(
            column(12, offset = 0,
                   tags$div(
                     id = "Instructions",
                     tags$h4("Details about tool settings"),
                     tags$ul(
                       tags$li("If 'Total trials per session' is same for all sessions, you can enter a single number."),
                       tags$li("If 'Keep learning from unsuccessful sessions' is set to 'Yes', it is assumed that the patient retains the learning from those sessions."),
                       tags$li("The 'Desired confidence level' is used to generate a one-sided posterior credible interval of the mastery (shaded area of plot)."),
                       tags$li("The 'Jeffries' prior corresponds to the prior belief used in the original MIEBL paper (Ramos, 2025).")
                     )
                   )
            ))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      y <- as.numeric(unlist(strsplit(input$y,",")))
      n <- as.numeric(unlist(strsplit(input$n,",")))
      tau <- input$tau
      T <- length(y)
      tau <- n*tau/100
      if(length(tau)==1){
        tau <- rep(tau,T)
      }
      t <- sum(y>= tau)
      if(length(n)==1)
        n <- rep(n,T)
      if(input$lr=="No"){
        y[y<tau]=0
        n[y<tau]=0
      }
      y <- sum(y)
      n <- sum(n)
      out <- plot_posterior1(prior = input$prior,y=y,n=n,t=t,T=T,
                     qtest = input$qtest,alpha = input$alpha/100)

    })

    output$theta_interval <- renderText({
      y <- as.numeric(unlist(strsplit(input$y,",")))
      n <- as.numeric(unlist(strsplit(input$n,",")))
      tau <- input$tau
      T <- length(y)
      tau <- n*tau/100
      if(length(tau)==1){
        tau <- rep(tau,T)
      }
      t <- sum(y>= tau)
      if(length(n)==1)
        n <- rep(n,T)
      if(input$lr=="No"){
        y[y<tau]=0
        n[y<tau]=0
      }
      y <- sum(y)
      n <- sum(n)
      out <- plot_posterior1(prior = input$prior,y=y,n=n,t=t,T=T,
                            qtest = input$qtest,alpha = input$alpha/100)
      paste('With', input$alpha, 'percent probability, the posterior mastery level is at least',floor(out[1]*100),'percent')
    })

    output$b <- renderText({
      y <- as.numeric(unlist(strsplit(input$y,",")))
      n <- as.numeric(unlist(strsplit(input$n,",")))
      tau <- input$tau
      T <- length(y)
      tau <- n*tau/100
      if(length(tau)==1){
        tau <- rep(tau,T)
      }
      t <- sum(y>= tau)
      if(length(n)==1)
        n <- rep(n,T)
      if(input$lr=="No"){
        y[y<tau]=0
        n[y<tau]=0
      }
      y <- sum(y)
      n <- sum(n)
      out <- plot_posterior1(prior = input$prior,y=y,n=n,t=t,T=T,
                            qtest = input$qtest,alpha = input$alpha/100)
      paste('Desired mastery is above',input$qtest,'percent with',floor(out[2]*100),'percent probability')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
