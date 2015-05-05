library(quantmod)

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





