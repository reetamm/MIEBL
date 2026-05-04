plot_posterior1 <- function(prior,y,n,t,T,qtest,alpha){
  priors <- c('Jeffries','Flat', 'Bell')
  if(prior==priors[1])
    a <- b <- 0.5
  if(prior==priors[2])
    a <- b <- 1
  if(prior==priors[3])
    a <- b <- 2

  A <- y+a
  B <- n-y+b

  out1 <- qbeta(1-alpha,A,B)

  p1 <- ggplot(data.frame(x = c(0:1))) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = a, shape2 = b),
                  xlim = c(0,1),
                  geom = "area",
                  aes(x, alpha = 0.5)) +
    labs(title = "Prior belief about mastery",
         x = "Prior mastery",
         y = "Distribution") +
    theme(legend.position="none")

  p2 <- ggplot(data.frame(x = c(0:1))) +
    stat_function(fun = dbeta,
                  args = list(shape1 = A, shape2 = B)) +
    stat_function(fun = dbeta,
                  args = list(shape1 = A, shape2 = B),
                  xlim = c(qbeta(1-alpha,A,B),1),
                  geom = "area",
                  aes(x, alpha = 0.5)) +
    labs(title = "Posterior distribution of mastery",
         x = "Posterior mastery",
         y = "Distribution") + theme_bw() +
    theme(legend.position="none")


  q <- seq(0.01,0.99,0.01)
  m1 <- rep(NA,99)
  for(i in 1:99){
    m1[i] <- pbeta(q[i],A,B,lower.tail = F)
  }

  p3 <- ggplot(data.frame(q,m1),aes(x=q)) + geom_line(aes(y=m1)) +
    geom_vline(xintercept = qtest/100) +
    labs(title = "Confidence in desired mastery",
         x = "Mastery level",
         y = "Minimum probability") +
    theme(legend.position="none")

  # grid.arrange(p1,p2,p3)
  grid.arrange(p2)
  # plot(q,m1,'l',col='red',lwd=2, xlab = 'Mastery level',ylab = 'Minimum probability')
  # lines(q,m2,'l',col='blue', lwd=2)
  # abline(v=0.9,lty=3,lwd=1.5)
  if(qtest<1)
    qtest <- qtest*100
  qtest <- floor(qtest)
  out2 <- m1[qtest]
  return(c(out1,out2))
}
