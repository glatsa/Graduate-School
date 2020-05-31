###Clean Environment###
rm(list = ls())

###Load Libraries###
#install.packages("learnr")
#install.packages("CVXR")
library(learnr)
library(psych)
library(ggplot2)
library(GGally)
library(lubridate)
library(dplyr)
library(quantreg)
library(forecast)
library(tidyquant)
library(timetk)
library(quantmod)
library(matrixStats)
library(moments)
library(QRM)
library(qrmdata)
library(xts)
library(zoo)
library(plotly)
library(scales)
library(quadprog)
library(plyr)
library(CVXR)
library(PerformanceAnalytics)
library(corrplot)


###Functions###

ES_calc <- function(data, prob)
{
  threshold <- quantile(data, prob)
  result <- mean(data[data > threshold])
}

stat_fun <- function(x, na.rm = TRUE, ...)
{
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  # ...   = additional args passed to quantile
  c(
    mean     = mean(x, na.rm = na.rm),
    stdev    = sd(x, na.rm = na.rm),
    skewness = skewness(x, na.rm = na.rm),
    kurtosis = kurtosis(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...)
  )
}


data_moments <- function(data) {
  mean.r <- colMeans(data)
  median.r <- colMedians(data)
  sd.r <- colSds(data)
  IQR.r <- colIQRs(data)
  skewness.r <- skewness(data)
  kurtosis.r <- kurtosis(data)
  result <-
    data.frame(
      mean = mean.r,
      median = median.r,
      std_dev = sd.r,
      IQR = IQR.r,
      skewness = skewness.r,
      kurtosis = kurtosis.r
    )
  return(result)
}

garch_sim_t <-
  function(n = 1000,
           df = 30,
           omega = 0.1,
           alpha = 0.8,
           phi = 0.05,
           mu = 0.01)
  {
    n <- n # lots of trials, each a "day" or an "hour"
    # set.seed(seed)
    z <- rt(n, df = df)
    e <-  z # store variates
    y <-  z # returns: store again in a different place
    sig2 <-  z ^ 2 # create volatility series
    omega <-  omega #base variance
    alpha <-  alpha #vols Markov dependence on previous variance
    phi <-  phi # returns Markov dependence on previous period
    mu <-  mu # average return
    for (t in 2:n)
    {
      # Because of lag start at second
      e[t] <-
        sqrt(sig2[t]) * z[t]           # 1. e is conditional on sig
      y[t] <-  mu + phi * (y[t - 1] - mu) + e[t] # 2. generate returns
      sig2[t + 1] <-  omega + alpha * e[t] ^ 2 # 3. generate new sigma^2
    }
    return <- list(
      sim_df_vbl <-
        data_frame(
          t = 1:n,
          z = z,
          y = y,
          e = e,
          sig = sqrt(sig2)[-(n + 1)]
        ),
      sim_df_title <-
        data_frame(
          t = 1:n,
          "1. Unconditional innovations" = z,
          "4. Conditional returns" = y,
          "3. Conditional innovations" = e,
          "2. Conditional volatility" = sqrt(sig2)[-(n + 1)]
        )
    )
  }

quantInv <- function(distr, value)
{
  ecdf(distr)(value)
}

corr_rolling <- function(x)
{
  dim <- ncol(x)
  corr_r <- cor(x)[upper.tri(diag(dim),
                             diag = FALSE)]
  return(corr_r)
}

vol_rolling <- function(x)
{
  library(matrixStats)
  vol_r <- colSds(x)
  return(vol_r)
}

portolioGMVP <- function(Sigma)
{
  w <- Variable(nrow(Sigma))
  prob <- Problem(Minimize(quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}

### Set working directory ####
#setwd("FIN 654/shinyapp/")

###Gather Data###
#tickers <- c("IHG", "ACCYY", "DLAKY","ICAGY")
tickers <- c("IHG", "ACCYY", "ICAGY")
getSymbols(tickers) #, env = stocks_env) # using quantmod
data("EUR_USD")
data("GBP_USD")
OldValues <-
  read.csv(
    "OldValues.csv",
    stringsAsFactors = FALSE,
    header = TRUE
  )

###Format Data###
##DLAKY##
DLAKY.df <-
  read.csv("DLAKY.csv")
rownames(DLAKY.df) <- DLAKY.df[, 1]
DLAKY.df$Dates <-
  as.Date(DLAKY.df$rownames.DLAKY123., format = "%m/%d/%y")
DLAKY.df <- DLAKY.df[-1]
colnames(DLAKY.df) <- c("DLAKY", "Date")
DLAKY <- xts(DLAKY.df[, -2], order.by = DLAKY.df[, 2])

##Exchange Rate Set up##
EUR_USD <- data.frame(EUR_USD)
GBP_USD <- data.frame(GBP_USD)
EUR_USD <-
  data.frame(rownames(EUR_USD), EUR_USD, stringsAsFactors = FALSE)
GBP_USD <-
  data.frame(rownames(GBP_USD), GBP_USD, stringsAsFactors = FALSE)

##Euro Format##
CutoffPoint <-
  which(OldValues[, 1] == EUR_USD[nrow(EUR_USD), 1]) # may need to change value
PartialData_E <-
  data.frame(OldValues[(CutoffPoint + 1):nrow(OldValues), 1:2])
rownames(PartialData_E) <- PartialData_E[, 1]
titles <- c(EUR_USD[, 1], PartialData_E[, 1])
EUR_USD <- data.frame(list(c(EUR_USD[, 2], PartialData_E[, 2])))
rownames(EUR_USD) <- titles
EUR_USD$Dates <- as.Date(rownames(EUR_USD))
colnames(EUR_USD) <- c("EUR/USD", "Dates")
EURxts <- xts(EUR_USD[, -2], order.by = EUR_USD[, 2])
r_EURxts <- diff(log(EURxts))[-1]

##Pound Format##
CutoffPoint <-
  which(OldValues[, 1] == GBP_USD[nrow(GBP_USD), 1]) # may need to change value
PartialData_G <-
  data.frame(OldValues[(CutoffPoint + 1):nrow(OldValues), 1:2])
rownames(PartialData_G) <- PartialData_G[, 1]
titles <- c(GBP_USD[, 1], PartialData_G[, 1])
GBP_USD <- data.frame(list(c(GBP_USD[, 2], PartialData_G[, 2])))
rownames(GBP_USD) <- titles
GBP_USD$Dates <- as.Date(rownames(GBP_USD))
colnames(GBP_USD) <- c("Dates", "GBP/USD")
GBPxts <- xts(GBP_USD[, -2], order.by = GBP_USD[, 2])
r_GBPxts <- diff(log(GBPxts))[-1]

###Calculations for Data Data###
##IHG##
data <- IHG
data <- data[, 4]
colnames(data) <- "IHG"
r_IHG <- diff(log(data))[-1] #Calculate Return
# convert xts object to a tibble or data frame
p_IHG <- data
#p_IHG_M <- data %>% as_tibble() %>% mutate(date = index(data), month = month.abb[month(index(data))])

##ACCYY##
data <- ACCYY
data <- data[, 4] # only adjusted close
colnames(data) <- "ACCYY"
r_ACCYY <- diff(log(data))[-1] #Calculate Return
# convert xts object to a tibble or data frame
p_ACCYY <- data
#p_ACCYY_M <- data %>% as_tibble() %>% mutate(date = index(data), month = month.abb[month(index(data))])

##DLAKY##
data <- DLAKY
colnames(data) <- "DLAKY"
r_DLAKY <- diff(log(data))[-1] #Calculate Return
# convert xts object to a tibble or data frame
p_DLAKY <- data
#p_DLAKY_M <- data %>% as_tibble() %>% mutate(date = index(data), month = month.abb[month(index(data))])

##ICAGY##
data <- ICAGY
data <- data[, 4]
colnames(data) <- "ICAGY"
r_ICAGY <- diff(log(data))[-1] #Calculate Return
# convert xts object to a tibble or data frame
p_ICAGY <- data
#p_ICAGY_M <- data %>% as_tibble() %>% mutate(date = index(data), month = month.abb[month(index(data))])

###Combine Data###
price <-
  na.omit(merge(p_IHG, p_ACCYY, p_DLAKY, p_ICAGY, EURxts, GBPxts))
return <-
  merge(
    ACCYY = r_ACCYY,
    DLAKY = r_DLAKY,
    ICAGY = r_ICAGY,
    IHG = r_IHG,
    EUR_USD = r_EURxts,
    GBP_USD = r_GBPxts,
    all = FALSE
  ) # Return Data Frame
return <- return * 100

###Returns###
#daily return
daily_return <- (diff(log(price))[-1]) * 100
price_difference <- (price / lag(price) - 1)[-1]
summary(daily_return)
mean_return <- apply(daily_return, 2, mean)
covar.return <- cov(daily_return)
std.return <- sqrt(diag(covar.return))

###Intro Graph###
plotNormalizedPrice <- function() {
  renderPlot({
    plot(
      price / rep(price[1,], each = nrow(price)),
      col = rainbow6equal,
      legend.loc = "topleft",
      main = "Normalized prices"
    )
  })
}

### Price changes ###
plotPriceChanges <- function () {
  renderPlotly({
    p <- autoplot.zoo(price)  + ggtitle("Price Change Plot")
    #print(p)
    ggplotly(p)
  })
}

### Aboslute Price changes ###
plotPriceSizeChanges <- function () {
  renderPlotly({
    p <- autoplot.zoo(abs(price)) + ggtitle("Price Size Change Plot")
    #print(p)
    ggplotly(p)
  })
}

###Portfolio Standard Dievation###
##Correlation Matrix##
r_corr <- apply.monthly(return, FUN = cor)

#Overall Correlation Matrix#
r.corr.1 <- matrix(r_corr[1,],
                   nrow = 6,
                   ncol = 6,
                   byrow = FALSE)
rownames(r.corr.1) <-
  c("ACCYY", "DLAKY", "ICAGY", "IHG", "EUR_USD", "GBP_USD")
colnames(r.corr.1) <-
  c("ACCYY", "DLAKY", "ICAGY", "IHG", "EUR_USD", "GBP_USD")
r.corr.1

#Correlation Columns#
r_corr <- r_corr [, c(2, 3, 4, 5, 6, 9, 10, 11, 12, 16, 17, 18, 23, 24, 30)]
colnames(r_corr) <-
  c(
    "IHG_ACCYY",
    "IHG_DLAKY",
    "IHG_ICAGY",
    "IHG_EURO",
    "IHG_GBP",
    "ACCYY_DLAKY",
    "ACCYY_ICAGY",
    "ACCYY_EURO",
    "ACCYY_GBP",
    "DLAKY_ICAGY",
    "DLAKY_EURO",
    "DLAKY_GBP",
    "ICAGY_EURO",
    "ICAGY_GBP",
    "EURO_GBP"
  )

###Portfolio Optimization with Shorts###
##Calculating Data##


##starting Plot##
plotMarkovSHort <- function() {
  renderPlot({
    Amat <-
      cbind(rep(1, 6), mean_return)  ## set the equality constraints matrix
    mu.P <-
      seq(min(mean_return - 5e-04), max(mean_return + 5e-04), length = 600)  ## set of 300 possible target portfolio returns
    sigma.P <-
      mu.P  ## set up storage for std dev's of portfolio returns
    weights <-
      matrix(0, nrow = 1000, ncol = ncol(daily_return))  ## storage for portfolio weights
    colnames(weights) <- colnames(daily_return)
    
    for (i in 1:length(mu.P))
    {
      bvec = c(1, mu.P[i])  ## constraint vector
      result = solve.QP(
        Dmat = 2 * covar.return,
        dvec = rep(0, 6),
        Amat = Amat,
        bvec = bvec,
        meq = 2
      )
      sigma.P[i] = sqrt(result$value)
      weights[i,] = result$solution
    }
    par(mfrow = c(1, 1))
    plot(
      sigma.P,
      mu.P,
      type = "l",
      xlim = c(-.02,
               max(std.return) * 1.1),
      ylim = c(-.02, max(mean_return) * 1.1),
      lty = 5,
      lwd = 1
    )
    ##  plot## the efficient frontier
    ##(and inefficient portfolios below the## min var portfolio)
    mu.free = 2.1 / 253 ## input value of risk-free interest rate
    points(0, mu.free, cex = 1, pch = "o")  ## show risk-free asset
    
    sharpe = (mu.P - mu.free) / sigma.P  ## compute Sharpe's ratios
    ind = (sharpe == max(sharpe))  ## Find maximum Sharpe's ratio
    options(digits = 3)
    lines(
      c(0, 2),
      mu.free + c(0, 2) * (mu.P[ind] -
                             mu.free) / sigma.P[ind],
      lwd = 1,
      lty = 5,
      col = "blue"
    )
    ## show line of optimal portfolios
    points(sigma.P[ind], mu.P[ind], cex = 4, pch = "*")  ## show tangency portfolio
    ind2 = (sigma.P == min(sigma.P))  ## find the minimum variance portfolio
    points(sigma.P[ind2], mu.P[ind2], cex = 2, pch = "+")  ## show min var portfolio
    ind3 = (mu.P > mu.P[ind2])  ## finally the efficient frontier
    lines(
      sigma.P[ind3],
      mu.P[ind3],
      type = "l",
      xlim = c(0, max(std.return) * 1.1),
      ylim = c(min(mean_return) * 1.05,
               max(mean_return) * 1.1),
      lwd = 3,
      col = "red"
    )
    ##  plot the efficient frontier
    text(std.return[1], mean_return[1], "IHG", cex = 1.15)
    text(std.return[2], mean_return[2], "ACCYY", cex = 1.15)
    text(std.return[3], mean_return[3], "DLAKY", cex = 1.15)
    text(std.return[4], mean_return[4], "ICAGY", cex = 1.15)
    text(std.return[5], mean_return[5], "EUR_USD", cex = 1.15)
    text(std.return[6], mean_return[6], "GBP_USD", cex = 1.15)
  })
}
#weights[ind, ]
#sum(weights[ind, ])

###Portfolio Optimization without Shorts###
##Calculating Data##


##starting Plot##
plotMarkovNoShort <- function() {
  renderPlot({
    Amat <-
      cbind(rep(1, 6), mean_return, diag(1, nrow = 6)) ## set the equality constraints matrix
    mu.P <-
      seq(min(mean_return) + 1e-04, max(mean_return) - 1e-04, length = 300)  ## set of 300 possible target portfolio returns
    sigma.P <-
      mu.P  ## set up storage for std dev's of portfolio returns
    weights <-
      matrix(0, nrow = 300, ncol = 6)  ## storage for portfolio weights
    colnames(weights) <- colnames(daily_return)
    
    for (i in 1:length(mu.P)) 
    {
      bvec <- c(1, mu.P[i], rep(0, 6))  ## constraint vector
      result <-
        solve.QP (
          Dmat = 2 * covar.return,
          dvec = rep(0, 6),
          Amat = Amat,
          bvec = bvec,
          meq = 2
        )
      sigma.P[i] = sqrt(result$value)
      weights[i,] = result$solution
    }
    par(mfrow = c(1, 1))
    plot(
      sigma.P,
      mu.P,
      type = "l",
      xlim = c(-.02, max(std.return) * 1.1),
      ylim = c(-.02, max(mean_return) * 1.1),
      lty = 5,
      lwd = 1
    )
    ##  plot## the efficient frontier
    ##(and inefficient portfolios below the## min var portfolio)
    mu.free = 2.1 / 253 ## input value of risk-free interest rate
    points(0, mu.free, cex = 1, pch = "o")  ## show risk-free asset
    
    sharpe = (mu.P - mu.free) / sigma.P  ## compute Sharpe's ratios
    ind = (sharpe == max(sharpe))  ## Find maximum Sharpe's ratio
    options(digits = 3)
    lines(
      c(0, 2),
      mu.free + c(0, 2) * (mu.P[ind] - mu.free) / sigma.P[ind],
      lwd = 1,
      lty = 5,
      col = "blue"
    )
    ## show line of optimal portfolios
    points(sigma.P[ind], mu.P[ind], cex = 4, pch = "*")  ## show tangency portfolio
    ind2 = (sigma.P == min(sigma.P))  ## find the minimum variance portfolio
    points(sigma.P[ind2], mu.P[ind2], cex = 2, pch = "+")  ## show min var portfolio
    ind3 = (mu.P > mu.P[ind2])  ## finally the efficient frontier
    lines(
      sigma.P[ind3],
      mu.P[ind3],
      type = "l",
      xlim = c(0, max(std.return) * 1.1),
      ylim = c(min(mean_return) * 1.05,
               max(mean_return) * 1.1),
      lwd = 3,
      col = "red"
    )
    ##  plot the efficient frontier
    text(std.return[1], mean_return[1], "IHG", cex = 1.15)
    text(std.return[2], mean_return[2], "ACCYY", cex = 1.15)
    text(std.return[3], mean_return[3], "DLAKY", cex = 1.15)
    text(std.return[4], mean_return[4], "ICAGY", cex = 1.15)
    text(std.return[5], mean_return[5], "EUR_USD", cex = 1.15)
    text(std.return[6], mean_return[6], "GBP_USD", cex = 1.15)
  })
}
#weights[ind, ]
#sum(weights[ind, ])

Amat <-
  cbind(rep(1, 6), mean_return)  ## set the equality constraints matrix
mu.P <-
  seq(min(mean_return - 5e-04), max(mean_return + 5e-04), length = 600)  ## set of 300 possible target portfolio returns
sigma.P <-
  mu.P  ## set up storage for std dev's of portfolio returns
weights <-
  matrix(0, nrow = 1000, ncol = ncol(daily_return))  ## storage for portfolio weights
colnames(weights) <- colnames(daily_return)

for (i in 1:length(mu.P))
{
  bvec = c(1, mu.P[i])  ## constraint vector
  result = solve.QP(
    Dmat = 2 * covar.return,
    dvec = rep(0, 6),
    Amat = Amat,
    bvec = bvec,
    meq = 2
  )
  sigma.P[i] = sqrt(result$value)
  weights[i,] = result$solution
}
##  plot## the efficient frontier
##(and inefficient portfolios below the## min var portfolio)
mu.free = 2.1 / 253 ## input value of risk-free interest rate

sharpe = (mu.P - mu.free) / sigma.P  ## compute Sharpe's ratios
ind = (sharpe == max(sharpe))  ## Find maximum Sharpe's ratio
w_all <- weights[ind, ]

Amat <-
  cbind(rep(1, 6), mean_return, diag(1, nrow = 6)) ## set the equality constraints matrix
mu.P <-
  seq(min(mean_return) + 1e-04, max(mean_return) - 1e-04, length = 300)  ## set of 300 possible target portfolio returns
sigma.P <-
  mu.P  ## set up storage for std dev's of portfolio returns
weights <-
  matrix(0, nrow = 300, ncol = 6)  ## storage for portfolio weights
colnames(weights) <- colnames(daily_return)

for (i in 1:length(mu.P)) {
  bvec <- c(1, mu.P[i], rep(0, 6))  ## constraint vector
  result <-
    solve.QP (
      Dmat = 2 * covar.return,
      dvec = rep(0, 6),
      Amat = Amat,
      bvec = bvec,
      meq = 2
    )
  sigma.P[i] = sqrt(result$value)
  weights[i,] = result$solution
}
##  plot## the efficient frontier
##(and inefficient portfolios below the## min var portfolio)
mu.free = 2.1 / 253 ## input value of risk-free interest rate

sharpe = (mu.P - mu.free) / sigma.P  ## compute Sharpe's ratios
ind = (sharpe == max(sharpe))  ## Find maximum Sharpe's ratio
w_all <- cbind("Markowitz" =w_all, "Markowitz w/ no Short" = weights[ind,])

###Simulation###
##Set Varriable##
N <- ncol(daily_return)  # number of stocks
A <- nrow(daily_return)  # number of days

##Seperate data data##
T_trn <- round(0.7 * A)  # 70% of data
X_log_trn <- daily_return[1:T_trn,]
X_log_tst <- daily_return[(T_trn + 1):T,]
X_lin_trn <- price_difference[1:T_trn,]
X_lin_tst <- price_difference[(T_trn + 1):T,]
mu <- colMeans(X_log_trn)
Sigma <- cov(X_log_trn)
w_diag <- 1 / sqrt(diag(Sigma))
w_diag <- w_diag / sum(w_diag)
w_GMVP <- portolioGMVP(Sigma)

# plot
w_all <- cbind(w_all, "GMVP" = w_GMVP)
w_all <- cbind(w_all, "risk-parity-diag" = w_diag)
w_all <- round(w_all, digits = 2)

plotAllocation <- function() {
  renderPlot({
    #barplot(t(w_all), main = "Portfolio allocation", xlab = "stocks", ylab = "dollars", beside = TRUE, legend = colnames(w_all), col = rainbow8equal[1:2])
    barplot(
      t(w_all),
      main = "Portfolio allocation",
      xlab = "stocks",
      ylab = "dollars",
      beside = TRUE,
      legend = colnames(w_all),
      col = rainbow8equal[1:4]
    )
  })
}

# compute returns of all portfolios
ret_all <- xts(price_difference %*% w_all, index(price_difference))
ret_all_trn <- ret_all[1:T_trn,]
ret_all_tst <- ret_all[-c(1:T_trn),]

# performance
table.AnnualizedReturns(ret_all_trn)
table.AnnualizedReturns(ret_all_tst)

plotCumReturns <- function() {
  renderPlot({
    chart.CumReturns(
      ret_all,
      main = "Performance of different portfolios",
      wealth.index = TRUE,
      legend.loc = "topleft",
      colorset = rich8equal
    )
    addEventLines(
      xts("training", index(price_difference[T_trn])),
      srt = 90,
      pos = 2,
      lwd = 2,
      col = "darkblue"
    )
  })
}

plotDrawDown <- function() {
  renderPlot({
    chart.Drawdown(
      ret_all,
      main = "Performance of different portfolios",
      legend.loc = "bottomleft",
      colorset = rich8equal
    )
    addEventLines(
      xts("training", index(price_difference[T_trn])),
      srt = 90,
      pos = 2,
      lwd = 2,
      col = "darkblue"
    )
  })
}

### Exploratory analysis ###

## ACCYY ##
plotVarAccyy <- function(input) {
  renderPlotly({
    returns1 <- r_ACCYY * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q > 1,
               0.99,
               ifelse(input$alpha.q < 0, 0.001, input$alpha.q))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}

## DLAKY ##
plotVarDLAKY <- function(input) {
  renderPlotly({
    returns1 <- r_DLAKY * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q > 1,
               0.99,
               ifelse(input$alpha.q < 0, 0.001, input$alpha.q))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}

## ICAGY ##
plotVarICAGY <- function(input) {
  renderPlotly({
    returns1 <- r_ICAGY * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q > 1,
               0.99,
               ifelse(input$alpha.q < 0, 0.001, input$alpha.q))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}

## IHG ##
plotVarIHG <- function(input) {
  renderPlotly({
    returns1 <- r_IHG * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q > 1,
               0.99,
               ifelse(input$alpha.q < 0, 0.001, input$alpha.q))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}

## EUR ##
plotVarEUR <- function(input) {
  renderPlotly({
    returns1 <- r_EURxts * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q2 > 1,
               0.99,
               ifelse(input$alpha.q2 < 0, 0.001, input$alpha.q2))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}

## GBP ##
plotVarGBP <- function(input) {
  renderPlotly({
    returns1 <- r_GBPxts * 100
    colnames(returns1) <-
      "Returns" #kluge to coerce column name for df
    returns1.df <-
      data.frame(Returns = returns1[, 1],
                 Distribution = rep("Historical", each = length(returns1)))
    alpha <-
      reactive({
        ifelse(input$alpha.q2 > 1,
               0.99,
               ifelse(input$alpha.q2 < 0, 0.001, input$alpha.q2))
      })
    
    # Value at Risk
    VaR.hist <- quantile(returns1, alpha())
    VaR.text <- paste("Value at Risk =", round(VaR.hist, 2))
    VaR.y <- max(density(returns1.df$Returns)$y)
    
    # Expected Shortfall
    ES.hist <- median(returns1[returns1 > VaR.hist])
    ES.text <- paste("Expected Shortfall =", round(ES.hist, 2))
    
    p <-
      ggplot(returns1.df, aes(x = Returns, fill = Distribution)) +
      geom_density(alpha = 0.5) +
      geom_vline(
        aes(xintercept = VaR.hist),
        linetype = "dashed",
        size = 1,
        color = "firebrick1"
      ) +
      geom_vline(aes(xintercept = ES.hist),
                 size = 1,
                 color = "firebrick1") +
      annotate("text",
               x = 2 + VaR.hist,
               y = VaR.y * 1.05,
               label = VaR.text) +
      annotate("text",
               x = 1.5 + ES.hist,
               y = VaR.y * 1.1,
               label = ES.text) + scale_fill_manual(values = "dodgerblue4") +
      xlim(-10, 10)
    ggplotly(p)
  })
}


### Stats ###
statsTable <- function() {
  library(moments)
  library(matrixStats)
  per_ret <- return[, 1:4] * 100
  #answer <- data_moments(per_ret)
  #answer <- round(answer, 4)
  #renderTable(answer)
  stat_fun(return[, 1:4])
}
#statsTable()


### Correlations ###
#ACCYY DLAKY ICAGY IHG EURO GBP
returnPercent <- return * 100
window <- 90 #reactive({input$window})
corr.returns <-
  rollapply(
    return,
    width = window,
    corr_rolling,
    align = "right",
    by.column = FALSE
  )
corr.returns.df <-
  na.omit(
    data.frame(
      Date = index(corr.returns),
      dlaky.accyy = corr.returns[, 1],
      icagy.accyy = corr.returns[, 2],
      icagy.dlaky = corr.returns[, 3],
      ihg.accyy = corr.returns[, 4],
      ihg.dlaky = corr.returns[, 5],
      ihg.icagy = corr.returns[, 6],
      eur.accyy = corr.returns[, 7],
      eur.dlaky = corr.returns[, 8],
      eur.icagy = corr.returns[, 9],
      eur.ihg = corr.returns[, 10],
      gbp.accyy = corr.returns[, 11],
      gbp.dlaky = corr.returns[, 12],
      gbp.icagy = corr.returns[, 13],
      gbp.ihg = corr.returns[, 14],
      gbp.eur = corr.returns[, 15]
    )
  )
#corr.returns.df <- na.omit(data.frame(Date = index(corr.returns), dlaky.=corr.returns[,1],
#                                     accyy.icagy=corr.returns[,2], accyy.ihg=corr.returns[,3],accyy.eur=corr.returns[,4],accyy.gbp=corr.returns[,5],
#                                     dlaky.icagy=corr.returns[,6], dlaky.ihg=corr.returns[,7], dlaky.eur=corr.returns[,8], dlaky.gbp=corr.returns[,9],
#                                     icagy.ihg=corr.returns[,10], icagy.eur=corr.returns[,11], icagy.gbp=corr.returns[,12],
#                                     ihg.eur=corr.returns[,13], ihg.gbp=corr.returns[,14],
#                                     eur.gbp=corr.returns[,15]))
#pairs.panels(corr.returns.df[])


plotCorr <- function() {
  renderPlot({
    corrplot.mixed(r.corr.1, tl.col = 'black')
  })
}


## DLAKY & ICAGY ##
plotDlakyIcagy <- function(input) {
  renderPlotly({
    window <- reactive({
      input$window
    })
    corr.returns <-
      rollapply(
        return,
        width = window(),
        corr_rolling,
        align = "right",
        by.column = FALSE
      )
    corr.returns.df <-
      na.omit(
        data.frame(
          Date = index(corr.returns),
          dlaky.accyy = corr.returns[, 1],
          icagy.accyy = corr.returns[, 2],
          icagy.dlaky = corr.returns[, 3],
          ihg.accyy = corr.returns[, 4],
          ihg.dlaky = corr.returns[, 5],
          ihg.icagy = corr.returns[, 6],
          eur.accyy = corr.returns[, 7],
          eur.dlaky = corr.returns[, 8],
          eur.icagy = corr.returns[, 9],
          eur.ihg = corr.returns[, 10],
          gbp.accyy = corr.returns[, 11],
          gbp.dlaky = corr.returns[, 12],
          gbp.icagy = corr.returns[, 13],
          gbp.ihg = corr.returns[, 14],
          gbp.eur = corr.returns[, 15]
        )
      )
    p <-
      ggplot(corr.returns.df, aes(x = Date, y = icagy.dlaky)) + geom_line()
    ggplotly(p)
  })
}


## ICAGY & EURO ##
plotICAGYEuro <- function(input) {
  renderPlotly({
    window <- reactive({
      input$window
    })
    corr.returns <-
      rollapply(
        return,
        width = window(),
        corr_rolling,
        align = "right",
        by.column = FALSE
      )
    corr.returns.df <-
      na.omit(
        data.frame(
          Date = index(corr.returns),
          dlaky.accyy = corr.returns[, 1],
          icagy.accyy = corr.returns[, 2],
          icagy.dlaky = corr.returns[, 3],
          ihg.accyy = corr.returns[, 4],
          ihg.dlaky = corr.returns[, 5],
          ihg.icagy = corr.returns[, 6],
          eur.accyy = corr.returns[, 7],
          eur.dlaky = corr.returns[, 8],
          eur.icagy = corr.returns[, 9],
          eur.ihg = corr.returns[, 10],
          gbp.accyy = corr.returns[, 11],
          gbp.dlaky = corr.returns[, 12],
          gbp.icagy = corr.returns[, 13],
          gbp.ihg = corr.returns[, 14],
          gbp.eur = corr.returns[, 15]
        )
      )
    p <-
      ggplot(corr.returns.df, aes(x = Date, y = eur.icagy)) + geom_line()
    ggplotly(p)
  })
}


## Extremes  ##
price.last <- as.numeric(tail(price, n = 1))
position.rf <- c(1 / 4, 1 / 4, 1 / 4, 1 / 4, 0, 0)
w <- position.rf * price.last
weights.rf <-
  matrix(w,
         nrow = nrow(return),
         ncol = ncol(return),
         byrow = TRUE)
loss.rf <- -rowSums(expm1(return / 100) * weights.rf)
loss.rf.df <-
  data.frame(Loss = loss.rf,
             Distribution = rep("Historical", each = length(loss.rf)))

## Empirical loss ##
alpha.tolerance <- .95
VaR.hist <- quantile(loss.rf, probs = alpha.tolerance, names = FALSE)
## Just as simple Expected shortfall
ES.hist <- median(loss.rf[loss.rf > VaR.hist])
VaR.text <-
  paste("Value at Risk =\n", round(VaR.hist, 2)) # ="VaR"&c12
ES.text <- paste("Expected Shortfall \n=", round(ES.hist, 2))
title.text <- paste(round(alpha.tolerance * 100, 0), "% Loss Limits")
renderPlotly({
  p <-
    ggplot(loss.rf.df, aes(x = Loss, fill = Distribution)) + geom_histogram(alpha = 0.8) + geom_vline(
      aes(xintercept = VaR.hist),
      linetype = "dashed",
      size = 1,
      color = "blue"
    ) + geom_vline(aes(xintercept = ES.hist), size = 1, color = "blue") + annotate("text",
                                                                                   x = VaR.hist,
                                                                                   y = 40,
                                                                                   label = VaR.text) + annotate("text",
                                                                                                                x = ES.hist,
                                                                                                                y = 20,
                                                                                                                label = ES.text) + xlim(0, 500) + ggtitle(title.text)
  ggplotly(p)
})

## Mean Excess loss ##
data <- as.vector(loss.rf) # data is purely numeric
umin <-  min(data)         # threshold u min
umax <-  max(data) - 0.1   # threshold u max
nint <-
  500                # grid length to generate mean excess plot
grid.0 <- numeric(nint)    # grid store
e <- grid.0                # store mean exceedances e
upper <- grid.0            # store upper confidence interval
lower <- grid.0            # store lower confidence interval
u <- seq(umin, umax, length = nint) # threshold u grid
alpha <- 0.95                  # confidence level
for (i in 1:nint) {
  data <- data[data > u[i]]  # subset data above thresholds
  e[i] <- mean(data - u[i])  # calculate mean excess of threshold
  sdev <- sqrt(var(data))    # standard deviation
  n <-
    length(data)          # sample size of subsetted data above thresholds
  upper[i] <-
    e[i] + (qnorm((1 + alpha) / 2) * sdev) / sqrt(n) # upper confidence interval
  lower[i] <-
    e[i] - (qnorm((1 + alpha) / 2) * sdev) / sqrt(n) # lower confidence interval
}
mep.df <-
  data.frame(
    threshold = u,
    threshold.exceedances = e,
    lower = lower,
    upper = upper
  )
loss.excess <- loss.rf[loss.rf > u]

plotExcessLoss <- function() {
  renderPlotly({
    p <-
      ggplot(mep.df, aes(x = threshold, y = threshold.exceedances)) + geom_line() + geom_line(aes(x = threshold, y = lower), colour = "red") + geom_line(aes(x = threshold,  y = upper), colour = "red") + xlim(-5, 5)
    ggplotly(p)
  })
}

## GPD ##
library(QRM)
alpha.tolerance <- 0.95
u <- quantile(loss.rf, alpha.tolerance , names = FALSE)
fit <- fit.GPD(loss.rf, threshold = u) # Fit GPD to the excesses
xi.hat <- fit$par.ests[["xi"]] # fitted xi
beta.hat <- fit$par.ests[["beta"]] # fitted beta
data <- loss.rf
n.relative.excess <- length(loss.excess) / length(loss.rf) # = N_u/n
VaR.gpd <-
  u + (beta.hat / xi.hat) * (((1 - alpha.tolerance) / n.relative.excess) ^
                               (-xi.hat) - 1)
ES.gpd <- (VaR.gpd + beta.hat - xi.hat * u) / (1 - xi.hat)
n.relative.excess <- length(loss.excess) / length(loss.rf) # = N_u/n
VaR.gpd <-
  u + (beta.hat / xi.hat) * (((1 - alpha.tolerance) / n.relative.excess) ^
                               (-xi.hat) - 1)
ES.gpd <- (VaR.gpd + beta.hat - xi.hat * u) / (1 - xi.hat)
# Plot away

plotGPD <- function() {
  renderPlotly({
    VaRgpd.text <- paste("GPD: Value at Risk =", round(VaR.gpd, 2))
    ESgpd.text <- paste("Expected Shortfall =", round(ES.gpd, 2))
    title.text <- paste(VaRgpd.text, ESgpd.text, sep = " ")
    loss.plot <-
      ggplot(loss.rf.df, aes(x = Loss, fill = Distribution)) + geom_density(alpha = 0.2)
    loss.plot <-
      loss.plot + geom_vline(
        aes(xintercept = VaR.gpd),
        colour = "blue",
        linetype = "dashed",
        size = 0.8
      )
    loss.plot <-
      loss.plot + geom_vline(aes(xintercept = ES.gpd),
                             colour = "blue",
                             size = 0.8)
    #+ annotate("text", x = 300, y = 0.0075, label = VaRgpd.text, colour = "blue") + annotate("text", x = 300, y = 0.005, label = ESgpd.text, colour = "blue")
    loss.plot <- loss.plot + xlim(0, 5) + ggtitle(title.text)
    ggplotly(loss.plot)
  })
}

## Confidence and Risk measure ##
plotConf <- function() {
  renderPlot({
    showRM(fit,
           alpha = 0.99,
           RM = "ES",
           method = "BFGS")
  })
}

## Risk offset
###Risk offset
risky <-  0.1
riskless <- 0.0210
sigma <- 0.20
threshold <- -0.15
alpha <- 0.08
z_star <-  qnorm(alpha)
w_star <- (threshold - riskless) / (risky - riskless + sigma * z_star)
w_star

### Risk Offset Graph

r_f <- 0.0210
mu <- 0.1
sigma <- 0.20
#
sigma_p <- (0:99) * (.3 / 100) #seq(0, .3, 100)
mu_p <- r_f + (mu - r_f) * sigma_p / sigma
w <- sigma_p / sigma
threshold <- -0.15
alpha <- 0.08
z_star <-  qnorm(alpha)
w_star <- (threshold - riskless) / (risky - riskless + sigma * z_star)
sim_df <- data_frame(sigma_p = sigma_p, mu_p = mu_p, w = w)
#
label_42 <-
  paste(
    alpha * 100,
    "% alpha, ",
    threshold * 100,
    "% threshold, \n",
    round(w_star * 100, 2),
    "% risky asset",
    sep = ""
  )
label_0 <-
  paste(alpha * 100,
        "% alpha, ",
        threshold * 100,
        "% threshold, \n",
        0 * 100,
        "% risky asset",
        sep = "")
label_100 <-
  paste(alpha * 100,
        "% alpha, ",
        threshold * 100,
        "% threshold, \n",
        1.00 * 100,
        "% risky asset",
        sep = "")

plotRisk <- function() {
  renderPlot({
    p <- ggplot(sim_df, aes(x = sigma_p, y = mu_p)) +
      geom_line(color = "blue", size = 1.1) +
      geom_point(aes(x = 0.0 * sigma, y = r_f + (mu - r_f) * 0.0),
                 color = "red",
                 size = 3.0) +
      annotate(
        "text",
        x = 0.2 * sigma,
        y = r_f + (mu - r_f) * 0.0 + 0.01,
        label = label_0
      ) +
      geom_point(
        aes(x = w_star * sigma, y = r_f + (mu - r_f) * w_star),
        shape = 21,
        color = "red",
        fill = "white",
        size = 4,
        stroke = 4
      ) +
      annotate(
        "text",
        x = w_star * sigma,
        y = r_f + (mu - r_f) * w_star + 0.01,
        label = label_42
      ) +
      geom_point(aes(x = 1.0 * sigma, y = r_f + (mu - r_f) * 1.00),
                 color = "red",
                 size = 3.0) +
      annotate(
        "text",
        x = 1.0 * sigma,
        y = r_f + (mu - r_f) * 1.00 + 0.01,
        label = label_100
      ) +
      xlab("standard deviation of portfolio return") +
      ylab("mean of portfolio return") +
      ggtitle("Risk-return tradeoff of cash and risky asset")
    p
  })
}
