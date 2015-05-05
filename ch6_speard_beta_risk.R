library(quantmod)

f3_plot_spread_level <- function (SPY=NA, AAPL=NA) {
    start_date = "2009-01-01"
    end_date   = "2011-12-31"
    
    if(is.na(SPY)) {
        SPY = getSymbols('SPY', from = start_date,
                     to = end_date, adjust=T, auto.assign = FALSE)
    }
    
    if(is.na(AAPL)) {
        AAPL = getSymbols('AAPL', from = start_date,
                      to = end_date, adjust=T, auto.assign = FALSE)
    }
    
    x = SPY[,6]
    y = AAPL[,6]
    
    results = calculate_beta_and_level(x,y,start_date,end_date)
    
    message ("Beta: ")
    results$beta
    
    message ("Level: ")
    results$level
    
    plot(results$spread, ylab="Spread Value",
            main = "AAPL - beta * SPY",
            cex.main = 0.8,
            cex.lab = 0.8,
            cex.axis = 0.8
        )
    
}

# Function to calculate the spread
# Spread is price that cal from previous regression
# It's an assumed price which compares to it's pair
calculate_spread = function(x,y,beta) {
    return(y - beta * x)
}

# Function to calculate the beta and level
# givrn start and end dates
calculate_beta_and_level <- function (x,y,start_date,end_date) {
    require(xts)
    
    time_range <-paste(start_date, "::", end_date, sep="")
    x = x[time_range]
    y = y[time_range]
    
    dx = diff(x[time_range])
    dy = diff(y[time_range])
    r = prcomp( ~ dx + dy)
    
    beta = r$rotation[2,1] / r$rotation[1,1]
    spread = calculate_spread(x,y,beta)
    names(spread) = "spread"
    level = mean(spread, na.rm=TRUE)
    
    outL = list()
    outL$spread = spread
    outL$beta = beta
    outL$level = level
    
    return(outL)
}

# Function to calculate buy and sell signals
# with upper and lower threshold
calculate_buy_sell_signals <- function(spread, beta, level, 
           lower_threshold, upper_threshold) {
    
    buy_signals = ifelse(spread <= level - lower_threshold,1,0)
    sell_signals= ifelse(spread >= level + upper_threshold,1,0)
    
    # bind these vectors into a matrix
    output = cbind(spread, buysignals, sell_signals)
    colnames(output) = c("spread", "buy_signals", "sell_signals")
    
    return(output)
}

f2_TLS_regression_SPY_AAPL <- function() {
    SPY = getSymbols('SPY', from = '2011-01-01',
        to = '2012-12-31', adjust=T, auto.assign = FALSE)
    
    AAPL = getSymbols('AAPL', from = '2011-01-01',
                     to = '2012-12-31', adjust=T, auto.assign = FALSE)
    
    x = diff(as.numeric(SPY[,4]))
    y = diff(as.numeric(AAPL[,4]))
    
    plot(x,y, main="Scatter plot of return. SPY vs AAPL",
        cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
    abline(lm(y~x))
    abline(lm(x~y), lty=2)
    grid()
    
    # Total least squares regression
    r = prcomp( ~ x + y )
    slope = r$rotation[2,1] / r$rotation[1,1]
    intercept = r$center[2] - slope * r$center[1]
    
    # Show the first principal component on the plot
    abline(a=intercept, b=slope, lty=3)
    
    
}

f1_scatter_plot_coke_pepsi <- function() {
    pepsi = getSymbols('PEP', from = '2013-01-01',
        to = '2014-01-01', adjust=T, auto.assign = FALSE)
    
    coke = getSymbols('COKE', from = '2013-01-01',
        to = '2014-01-01', adjust=T, auto.assign = FALSE)
    Sys.setenv(TZ = "UTC")
    
    prices = cbind(pepsi[,6], coke[,6]) # use Adjusted price
    price_changes = apply(prices, 2, diff) # daily price differences by each stock
    plot ( price_changes[,1], price_changes[,2],
        xlab = "Coke price changes",
        ylab = "Pepsi price changes",
        main = "Pepsi vs Coke",
        cex.main  = 0.8,
        cex.lab = 0.8,
        cex.axis = 0.8
    )
    grid() # add grid to plot above
    ans = lm(price_changes[,1] ~ price_changes[,2])
    beta = ans$coefficients[2]
    
    cat("Cal lm =")
    print(ans)
    cat("\n")
    cat("Beta =", beta, "\n")
    
    # use prices percentage change
    prices = cbind(
        Delt(pepsi[,6]), 
        Delt(coke[,6])
    )
    
}

f1_1_scatter_plot_coke_pepsi_percent <- function() {
    pepsi = getSymbols('PEP', from = '2013-01-01',
                       to = '2014-01-01', adjust=T, auto.assign = FALSE)
    
    coke = getSymbols('COKE', from = '2013-01-01',
                      to = '2014-01-01', adjust=T, auto.assign = FALSE)
    Sys.setenv(TZ = "UTC")
    
    # use prices percentage change
    price_changes = cbind(
        Delt(pepsi[,6]), 
        Delt(coke[,6])
    )
    price_changes = price_changes[complete.cases(price_changes),]
    price_changes = as.data.frame(price_changes)
    
    plot ( price_changes[,1], price_changes[,2],
           xlab = "Coke % changes",
           ylab = "Pepsi % changes",
           main = "Pepsi vs Coke %",
           cex.main  = 0.8,
           cex.lab = 0.8,
           cex.axis = 0.8
    )
    grid() # add grid to plot above
    ans = lm(price_changes[,1] ~ price_changes[,2])
    beta = ans$coefficients[2]
    
    cat("Cal lm =")
    print(ans)
    cat("\n")
    cat("Beta =", beta, "\n")
}





