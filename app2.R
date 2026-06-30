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
plot_posterior <- function(prior,y,n,t,T,qtest,alpha){
  priors <- c('Jeffries','Flat', 'Bell')
  if(prior==priors[1])
    a <- b <- 0.5
  if(prior==priors[2])
    a <- b <- 1
  if(prior==priors[3])
    a <- b <- 2

  y_min <- y*t
  y_max <- n*t + (y-1)*(T-t)
  A_min <- y_min+a
  B_min <- n*T-y_min+b

  A_max <- y_max+a
  B_max <- n*T-y_max+b

  out1 <- qbeta(1-alpha,A_min,B_min)
  out2 <- qbeta(1-alpha,A_max,B_max)

  p1 <- ggplot(data.frame(x = c(0:1))) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b),
                  xlim = c(0,1),
                  geom = "area",
                  aes(x, alpha = 0.5)) +
    labs(title = "Prior distribution",
         x = "Prior mastery",
         y = "Distribution") +
    theme(legend.position="none")

  A <- y+a
  B <- n-y+b
  p2 <- ggplot(data.frame(x = c(0:1))) +
    stat_function(fun = dbeta,
                  args = list(shape1 = A, shape2 = B)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = A, shape2 = B),
                  xlim = c(qbeta(0.05,A,B),1),
                  geom = "area",
                  aes(x, alpha = 0.5)) +
    labs(title = "Mastery level for a single session",
         x = "Posterior mastery",
         y = "Distribution") +
    theme(legend.position="none")


  q <- seq(0.01,0.99,0.01)
  m1 <- m2 <- rep(NA,99)
  for(i in 1:99){
    m1[i] <- pbeta(q[i],A_min,B_min,lower.tail = F)
    m2[i] <- pbeta(q[i],A_max,B_max,lower.tail = F)
  }

  p3 <- ggplot(data.frame(q,m1,m2),aes(x=q)) + geom_line(aes(y=m1,col='red')) + geom_line(aes(y=m2,col='blue'))+
    geom_vline(xintercept = qtest/100) +
    labs(title = "Mastery range over all sessions",
         x = "Mastery level",
         y = "Minimum probability") +
    theme(legend.position="none")

  grid.arrange(p1,p2,p3)
  # plot(q,m1,'l',col='red',lwd=2, xlab = 'Mastery level',ylab = 'Minimum probability')
  # lines(q,m2,'l',col='blue', lwd=2)
  # abline(v=0.9,lty=3,lwd=1.5)
  if(qtest<1)
    qtest <- qtest*100
  qtest <- floor(qtest)
  out3 <- m1[qtest]
  out4 <- m2[qtest]
  return(c(out1,out2,out3,out4))
}

priors <- c('Jeffries','Flat', 'Bell')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          radioButtons("prior", "Select a prior", priors),
            numericInput("y",
                      "y",
                      value = 8),
            numericInput("n",
                      "n",
                      value = 10),
            numericInput("t",
                      "t",
                      value = 3),
            numericInput("T",
                      "T",
                      value = 4),
        sliderInput("alpha",
                    "confidence level",
                    min = 0.80,
                    max = 0.99,
                    value = 0.95,
                    step = 0.01),
        sliderInput("qtest",
                    "expertise level",
                    min = 50,
                    max = 99,
                    value = 80,
                    step = 1)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput('mastery')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({


        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')

      # plot_posterior(a=0.5,b=0.5,y=8,n=10,t=3,T=4,qtest = 0.954,alpha = 0.05)
      out <- plot_posterior(prior = input$prior,y=input$y,n=input$n,t=input$t,T=input$T,
                     qtest = input$qtest,alpha = input$alpha)

    })

    output$mastery <- renderText({
      out <- plot_posterior(prior = input$prior,y=input$y,n=input$n,t=input$t,T=input$T,
                            qtest = input$qtest,alpha = input$alpha)
      paste('Mastery level is',out[4])
    })
}

# Run the application
shinyApp(ui = ui, server = server)
