library(quantmod)

# data_out <- spread_trade()
# trade_sim(data_out)

spread_trade <- function() {

    window_length = 10

    # Time range
    start_date = "2012-06-01"
    end_date   = "2012-12-31"
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

    beta = rolling_beta(dF, window_length)
    names(beta) = c("intercept", "beta")

    #Cal spread: y = x * slope + Intercept
    data_out <- merge(beta, dF)
    data_out$spread = data_out$y - (data_out$x * lag(beta,1)$beta + lag(beta,1)$intercept)

    # data_out$threshold <- rolling_sd(data_out$spread, window_length)

    # Generate signals
    # threshold <- sd(data_out$spread, na.rm = TRUE)
    data_out$threshold <- rolling_sd(data_out$spread, window_length)

    # Generate signals
    # buys <- ifelse(data_out$spread > threshold, 1, 0)
    # sells <- ifelse(data_out$spread < -threshold, -1, 0)
    buys <- ifelse(data_out$spread > data_out$threshold, 1, 0)
    sells <- ifelse(data_out$spread < -data_out$threshold, -1, 0)


    data_out$signal <- buys + sells


    return(data_out)

}

trade_sim <- function(data_out) {
    prev_x_qty <- 0
    trade_size <- 100
    position <- 0

    signal <- as.numeric(data_out$signal)
    signal[is.na(signal)] <- 0

    beta <- as.numeric(data_out$beta)
    intc <- as.numeric(data_out$intercept)

    price_x <- as.numeric(data_out$x)
    price_y <- as.numeric(data_out$y)

    qty_x <- rep(0,length(signal))
    qty_y <- rep(0,length(signal))

    for(i in 1:length(signal)) {
        cat( as.character(index(data_out[i])), " sig:", signal[i], "pos:", position, "B:", beta[i], "Intc:", intc[i] , "--","prev_x_qty:", prev_x_qty )
        if(signal[i] == 1  && position == 0) {
            # Initial Buy
            # prev_x_qty <- round(beta[i] * trade_size)

            # Balance price
            prev_x_qty <- round(price_y[i] * trade_size / price_x[i])

            qty_x[i] <- -prev_x_qty # Short
            qty_y[i] <- trade_size # long
            position <- 1
        }

        if(signal[i] == -1  && position == 0) {
            # Initial Sell
            # prev_x_qty <- round(beta[i] * trade_size)

            # Balance price
            prev_x_qty <- round(price_y[i] * trade_size / price_x[i])

            qty_x[i] <- prev_x_qty
            qty_y[i] <- -trade_size
            position <- -1
        }

        if(signal[i] == 1  && position == -1) {
            # In short spread, need to Buy
            # qty_x[i] <- -(round(beta[i] * trade_size) + prev_x_qty)
            # prev_x_qty <- round(beta[i] * trade_size)

            # Balance price
            target_qty <- round(price_y[i] * trade_size / price_x[i])
            qty_x[i] <- -(target_qty + prev_x_qty)
            prev_x_qty <- target_qty

            qty_y[i] <- 2 * trade_size
            position <- 1
        }

        if(signal[i] == -1  && position == 1) {
            # In long spread, need to Sell
            # qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
            # prev_x_qty <- round(beta[i] * trade_size)

            # Balance price
            target_qty <- round(price_y[i] * trade_size / price_x[i])
            qty_x[i] <- target_qty + prev_x_qty
            prev_x_qty <- target_qty

            qty_y[i] <- -2 * trade_size
            position <- -1
        }

        # adjust qty  by Beta to balance the risk
        # if(signal[i] == -1  && position == -1) {
        #     qty_x[i] <- round(beta[i] * trade_size) - prev_x_qty
        #     prev_x_qty <- round(beta[i] * trade_size)
        # }
        #
        # if(signal[i] == 1  && position == 1) {
        #     qty_x[i] <- round(beta[i] * trade_size) - prev_x_qty
        #     prev_x_qty <- round(beta[i] * trade_size)
        # }

        cat("  ==>>", "qty_x:", qty_x[i], "qty_y:", qty_y[i])
        cat(" P(x,y) (", data_out[i]$x, "," ,data_out[i]$y, ")" )
        cat(" Vol(x,y) (", prev_x_qty * -(position), ",", position * trade_size, ")"  )
        cat("\n")

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

    plot_equity_curve(data_out)

}

plot_equity_curve <- function(data_out) {
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
    points(data_out$equity_curve_sum, pch = point_type)
    lines(data_out$equity_curve_sum)
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

cal_threshold <- function(spread) {
    sd(spread, na.rm = FALSE)
}

rolling_sd <- function(z, width) {

    rollapply(z,width=width, FUN=cal_threshold,
              by.column=FALSE, align="right"
    )
}

# Calculate betas
run_regression <- function(dF) {
    # return(coef(lm(y ~ x-1, data=as.data.frame(dF))) )

    fit <- lm(y ~ x, data=as.data.frame(dF))
    return(coef(fit))

}

rolling_beta <- function(z, width) {
    rollapply(z,width=width, FUN=run_regression,
              by.column=FALSE, align="right"
    )
}