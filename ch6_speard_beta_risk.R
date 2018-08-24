library(quantmod)
library(reshape2)
library(ggplot2)

f5_out_of_sample_spread <- function() {
    # Rolling window of trading days
    window_length = 15

    # Time range
    start_date = "2011-01-01"
    end_date   = "2011-12-31"
    range <-paste(start_date, "::", end_date, sep="")

    # Stocks pair
    SPY = getSymbols('SPY', from = start_date,
                     to = end_date, adjust=T, auto.assign = FALSE)

    AAPL = getSymbols('AAPL', from = start_date,
                      to = end_date, adjust=T, auto.assign = FALSE)

    # Stocks pair
    x <- SPY[range, 6]
    y <- AAPL[range, 6]

    dF = cbind(x,y)
    names(dF) = c("x", "y")

    beta_out_of_sample = rolling_beta(diff(dF), window_length)

    # Buy and sell threshold
    data_out <- merge(beta_out_of_sample, dF)
    data_out$beta_price <- lag(beta_out_of_sample, 1) * data_out$x
    data_out$spread <-data_out$y -data_out$beta_price


    threshold <- sd(data_out$spread, na.rm = TRUE) / 2

    # Generate signals
    buys <- ifelse(data_out$spread > threshold, 1, 0)
    sells <- ifelse(data_out$spread < -threshold, -1, 0)

    data_out$signal <- buys + sells

    f5_plot_spread(data_out)
    f5_plot_price(data_out)

    f5_trade_spread(data_out)


}

f5_trade_spread <- function(data_out) {
    prev_x_qty <- 0
    trade_size <- 100
    position <- 0

    signal <- as.numeric(data_out$signal)
    signal[is.na(signal)] <- 0
    beta <- as.numeric(data_out$beta_out_of_sample)

    qty_x <- rep(0,length(signal))
    qty_y <- rep(0,length(signal))

    for(i in 1:length(signal)) {
        cat("Current signal:", signal[i], "position:", position, "-- prev_x_qty:", prev_x_qty )
        if(signal[i] == 1  && position == 0) {
            # Initial Buy
            prev_x_qty <- round(beta[i] * trade_size)
            qty_x[i] <- -prev_x_qty # Short
            qty_y[i] <- trade_size # long
            position <- 1
        }

        if(signal[i] == -1  && position == 0) {
            # Initial Sell
            prev_x_qty <- round(beta[i] * trade_size)
            qty_x[i] <- prev_x_qty
            qty_y[i] <- -trade_size
            position <- -1
        }

        if(signal[i] == 1  && position == -1) {
            # In short spread, need to Buy
            qty_x[i] <- -(round(beta[i] * trade_size) + prev_x_qty)
            prev_x_qty <- round(beta[i] * trade_size)
            qty_y[i] <- 2 * trade_size
            position <- 1
        }

        if(signal[i] == -1  && position == 1) {
            # In long spread, need to Sell
            qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
            prev_x_qty <- round(beta[i] * trade_size)
            qty_y[i] <- -2 * trade_size
            position <- -1
        }
        cat("  ==>>", "qty_x:", qty_x[i], "qty_y:", qty_y[i], "\n")

    }

    # End of test
    qty_x[length(qty_x)] <- -sum(qty_x)
    qty_y[length(qty_y)] <- -sum(qty_y)

    data_out$qty_x <- qty_x
    data_out$qty_y <- qty_y

    tail(data_out,20)

    # Plot equity
    data_out$equity_curve_x <- compute_equity_curve(data_out$qty_x, data_out$x)
    data_out$equity_curve_y <- compute_equity_curve(data_out$qty_y, data_out$y)

    data_out$equity_curve_sum <- data_out$equity_curve_x + data_out$equity_curve_y

    f5_plot_equity_curve(data_out)
}

f5_plot_equity_curve <- function(data_out) {
    # ggplot(data = data_out, aes(x = Index, y = equity_curve_sum)) + geom_line()

    plot(data_out$equity_curve_sum, ylab="Rolling Spread",
         main = "AAPL vs SPY equity",
         cex.main = 0.8,
         cex.lab = 0.8,
         cex.axis = 0.8
    )

    point_type <- rep(NA, nrow(data_out))
    buy_index <- which(data_out$signal == 1)
    sell_index <- which(data_out$signal == -1)

    point_type[buy_index] <- 21
    point_type[sell_index] <- 24
    points(data_out$y, pch = point_type)
    lines(data_out$y)
}

compute_equity_curve <- function(qty, price) {
    cash_buy  <- ifelse(sign(qty) == 1, qty * price, 0)
    cash_sell <- ifelse(sign(qty) == -1, -qty * price, 0)

    position <- cumsum(qty)
    cumulative_buy <- cumsum(cash_buy)
    cumulative_sell <- cumsum(cash_sell)

    equity <- cumulative_sell - cumulative_buy + (position * price)
    return(equity)
}



f5_plot_spread <- function(data_out) {
    plot(data_out$spread, ylab="Rolling Spread",
         main = "AAPL vs SPY out of sample",
         cex.main = 0.8,
         cex.lab = 0.8,
         cex.axis = 0.8
    )
    abline(h = threshold, lwd = 2)
    abline(h = -threshold, lwd = 2)

    point_type <- rep(NA, nrow(data_out))
    buy_index <- which(data_out$signal == 1)
    sell_index <- which(data_out$signal == -1)

    point_type[buy_index] <- 21
    point_type[sell_index] <- 24
    points(data_out$spread, pch = point_type)
}

f5_plot_price <- function(data_out) {
    plot(data_out$y, ylab="AAPL price",
         main = "AAPL vs SPY out of sample",
         cex.main = 0.8,
         cex.lab = 0.8,
         cex.axis = 0.8
    )


    point_type <- rep(NA, nrow(data_out))
    buy_index <- which(data_out$signal == 1)
    sell_index <- which(data_out$signal == -1)

    point_type[buy_index] <- 21
    point_type[sell_index] <- 24
    points(data_out$y, pch = point_type)
}

# f5_plot_price <- function(data_out) {
#     # data_plot = cbind(data_out$x, data.frame(rep("SPY", nrow(data_out)))
#
#     # merge(data_out$x, "DUDE")
#
#     data_out[complete.cases(data_out), ] # remove NA
#
#     data_plot_bp = cbind(
#         as.character(index(data_out)),
#         as.vector(data_out$beta_price),
#         rep("SPY_BETA", nrow(data_out))
#     )
#     colnames(data_plot_bp) = c("date", "price", "label")
#     data_plot_bp  = data.frame(data_plot_bp)
#
#     data_plot_y = cbind(
#         as.character(index(data_out)),
#         as.vector(data_out$y),
#         rep("AAPL", nrow(data_out))
#     )
#     colnames(data_plot_y) = c("date", "price", "label")
#     data_plot_y  = data.frame(data_plot_y)
#
#     data_plot <- rbind(data_plot_bp, data_plot_y)
#     data_plot$price = round(data_plot$price)
#
#     ggplot(data_plot, aes(x = date, y = price, group=label)) +
#         geom_line() +
#         theme_bw()
#
# }


f4_rolling_beta <- function() {
    # Rolling window of trading days
    window_length = 10

    # Time range
    start_date = "2011-01-01"
    end_date   = "2011-12-31"
    range <-paste(start_date, "::", end_date, sep="")

    # Stocks pair
    SPY = getSymbols('SPY', from = start_date,
            to = end_date, adjust=T, auto.assign = FALSE)

    AAPL = getSymbols('AAPL', from = start_date,
            to = end_date, adjust=T, auto.assign = FALSE)

    # Stocks pair
    x <- SPY[range, 6]
    y <- AAPL[range, 6]

    dF = cbind(x,y)
    names(dF) = c("x", "y")

    betas = rolling_beta(diff(dF), window_length)

    data = merge(betas, dF)
    data$spread = data$y - lag(betas,1) * data$x

    returns = diff(dF) / dF
    return_beta = rolling_beta(returns, window_length)
    data$spreadR = diff(data$y) / data$y -
        return_beta * diff(data$x) / data$x

    print(tail(data))

    threshold <- sd(data$spread, na.rm = TRUE)

    plot(data$spread, ylab="Rolling Spread",
         main = "AAPL vs SPY In-sample",
         cex.main = 0.8,
         cex.lab = 0.8,
         cex.axis = 0.8
    )
    abline(h = threshold, lty = 2)
    abline(h = -threshold, lty = 2)

    return(data)
}

# Calculate betas
run_regression <- function(dF) {
    return(coef(lm(y ~ x-1, data=as.data.frame(dF))) )
}

rolling_beta <- function(z, width) {
    rollapply(z,width=width, FUN=run_regression,
            by.column=FALSE, align="right"
        )
}

# f3_1_spread_out_of_sample(results=results)
f3_1_spread_out_of_sample <- function(SPY=NA, AAPL=NA,results=NA) {
    start_date_out_sample = "2012-01-01"
    end_date_out_sample = "2012-10-22"

    if(is.na(SPY)) {
        SPY = getSymbols('SPY', from = start_date_out_sample,
                         to = end_date_out_sample, adjust=T, auto.assign = FALSE)
    }

    if(is.na(AAPL)) {
        AAPL = getSymbols('AAPL', from = start_date_out_sample,
                          to = end_date_out_sample, adjust=T, auto.assign = FALSE)
    }

    x = SPY[,6]
    y = AAPL[,6]

    range = paste(start_date_out_sample,"::",
            end_date_out_sample, sep="")

    # Out of sample analysis
    spread_out_of_sample = calculate_spread(
        x[range],
        y[range],
        results$beta
    )

    plot(spread_out_of_sample, ylab="Spread out of sample",
         main = "AAPL - beta * SPY",
         cex.main = 0.8,
         cex.lab = 0.8,
         cex.axis = 0.8
    )
    abline(h = results$level, lwd=2)
}


# results = f3_plot_spread_level()
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

    message ("Beta: ", results$beta)
    message ("Level: ", results$level)


    plot(results$spread, ylab="Spread Value",
            main = "AAPL - beta * SPY",
            cex.main = 0.8,
            cex.lab = 0.8,
            cex.axis = 0.8
        )

    return(results)
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





