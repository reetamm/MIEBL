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

plot_sessions <- function(y, n, qtest, alpha, prior, k, J, type){

  if(length(n) == 1){
    n <- rep(n, length(y))
  }

  n_obs <- length(y)

  # Forecast-only storage
  y_fore <- c()
  n_fore <- c()

  y_mean_fore <- c()   # Mean forecast vector

  if(type %in% c("window","floor")){
  for(i in 1:k){

    # Combine observed + forecasts for posterior
    y_all <- c(y, y_fore)
    n_all <- c(n, n_fore)

    if(type == "window"){

      if(i == 1){
        y_sum <- sum(y_all)
        n_sum <- sum(n_all)
        T_use <- length(y_all)
      } else {
        J_use <- min(J, length(y_all))

        y_recent <- tail(y_all, J_use)
        n_recent <- tail(n_all, J_use)

        y_sum <- sum(y_recent)
        n_sum <- sum(n_recent)
        T_use <- J_use
      }

    } else if(type == "floor"){

      y_sum <- sum(y_all)
      n_sum <- sum(n_all)
      T_use <- length(y_all)
    }

    out <- plot_posterior1(
      prior = prior,
      y = y_sum,
      n = n_sum,
      t = NA,
      T = T_use,
      qtest = qtest,
      alpha = alpha
    )

    L <- out[1]

    # Compute posterior mean manually
    if(prior == "Jeffries"){ a <- b <- 0.5 }
    if(prior == "Flat"){     a <- b <- 1 }
    if(prior == "Bell"){     a <- b <- 2 }

    A <- y_sum + a
    B <- n_sum - y_sum + b

    mean_post <- A / (A + B)

    #n_next <- n[1]
    n_next <- n[which(n > 0)[1]]

    # Lower-bound forecast (existing)
    if(type == "floor"){
      y_next <- floor(L * n_next)
    } else {
      y_next <- L * n_next
    }

    # Mean-based forecast (rounded)
    y_mean_next <- round(mean_post * n_next)

    # Append forecasts
    y_fore <- c(y_fore, y_next)
    n_fore <- c(n_fore, n_next)

    y_mean_fore <- c(y_mean_fore, y_mean_next)
  }
  }


  if(type == "decay"){

    k<-min(k,length(y))
    y_sum <- sum(y)
    n_sum <- sum(n)

    if(prior == "Jeffries"){ a <- b <- 0.5 }
    if(prior == "Flat"){     a <- b <- 1 }
    if(prior == "Bell"){     a <- b <- 2 }

    A <- y_sum + a
    B <- n_sum - y_sum + b

    mean_post <- A / (A + B)

    for(i in 1:k){

      if(i > 1){
        # shrink information
        n_sum <- n_sum - n[1]
        y_sum <- mean_post * n_sum

        A <- y_sum + a
        B <- n_sum - y_sum + b
      }

      # lower bound evolves
      L <- qbeta(1 - alpha, A, B)

      #n_next <- n[1]
      n_next <- n[which(n > 0)[1]]
      y_next <- L * n_next

      y_mean_next <- round(mean_post * n_next)

      y_fore <- c(y_fore, y_next)
      n_fore <- c(n_fore, n_next)
      y_mean_fore <- c(y_mean_fore, y_mean_next)
    }
  }

  # Observed performance
  sessions_obs <- 1:n_obs
  perf_obs <- (y / n) * 100

  # Forecast performance
  if(k > 0){
    sessions_fore <- (n_obs+1):(n_obs+k)
    perf_fore <- (y_fore / n_fore) * 100

    perf_mean_fore <- (y_mean_fore / n_fore) * 100
  }


  p <- ggplot() +

    # Observed line
    geom_line(aes(x = sessions_obs, y = perf_obs, color = "Observed"), size = 1) +

    # Observed points
    geom_point(aes(x = sessions_obs, y = perf_obs, color = "Observed"), size = 2) +

    # Lower bound forecast (blue dashed)
    {if(k > 0) geom_line(
      aes(x = c(n_obs, sessions_fore),
          y = c(perf_fore[1], perf_fore),
          color = "Predicted Lower Bound",
          linetype = "Predicted Lower Bound"),
      size = 1
    )} +

    # Mean forecast (green dashed)
    {if(k > 0) geom_line(
      aes(x = c(n_obs, sessions_fore),
          y = c(perf_mean_fore[1], perf_mean_fore),
          color = "Predicted Mean Performance",
          linetype = "Predicted Mean Performance"),
      size = 1
    )} +

    # Mastery line
    geom_hline(yintercept = qtest, linetype = "dashed", color = "red") +

    # Observed vs Predicted
    geom_vline(xintercept = n_obs, linetype = "dotted") +

    scale_color_manual(values = c(
      "Observed" = "black",
      "Predicted Lower Bound" = "blue",
      "Predicted Mean Performance" = "darkgreen"
    )) +

    scale_linetype_manual(values = c(
      "Observed" = "solid",
      "Predicted Lower Bound" = "dashed",
      "Predicted Mean Performance" = "dashed"
    )) +

    scale_x_continuous(breaks = 1:(n_obs + k), limits = c(1, n_obs + k)) +
    scale_y_continuous(limits = c(0, 100)) +

    guides(linetype = "none") +

    labs(
      title = "Session Performance Graph",
      x = "Session",
      y = "Performance (%)",
      color = ""
    ) +
    theme_bw() +
    theme(legend.position = 'bottom')

  print(p)

}
