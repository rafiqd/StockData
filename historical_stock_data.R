library(RCurl)
library(XML)
library(ggplot2)

get_historical_data <- function(symbol){
    url <- sprintf("https://www.nasdaq.com/symbol/%s/historical",
                  symbol)
    html <- getURL(url, followlocation = TRUE)
    doc = htmlTreeParse(html, useInternalNodes = TRUE)
    text <- xpathSApply(doc, "//div[@id='historicalContainer']//table/tbody//tr")
    data_list = list()
    i <- 1
    for(t in text){
        t <- as(t, "character" )
        x <- gsub(" |[\r\n]|[\n]|(&#13;)|<tr>|</tr>", "", t)
        x <- gsub("</td><td>", ",", x)
        x <- gsub("<td>|</td>", "", x)
        if(x == ",,,,,")
            next()
        x <- strsplit(x, ",")
        
    
        dat <- data.frame(as.Date(x[[1]][1], "%m/%d/%Y"), 
                          as.numeric(x[[1]][2]), 
                          as.numeric(x[[1]][3]), 
                          as.numeric(x[[1]][4]), 
                          as.numeric(x[[1]][5]), 
                          as.numeric(x[[1]][6])
                          )
        data_list[[i]] <- dat
        i <- i + 1
    }
    big_data = do.call(rbind, data_list)
    colnames(big_data) <- c("Date", "Open", "High", "Low", "Close/Last", "volume")
    return(big_data)
}

get_rsi <- function(dat, start=15){
    
    dat <- dat[order(dat$Date),]
    avg_loss = rep(0, start-1)
    avg_gain = rep(0, start-1)
    rsi = rep(0, start-1)
    for(i in start:nrow(dat)){
        if(i == start){
            # calculate the first RSI by using the previous 14 days
            first_avg_gain = 0
            first_avg_loss = 0
            for(j in 2:start){
                diff = dat[j,"Close/Last"] - dat[j-1, "Close/Last"]
                if(diff > 0){
                    first_avg_gain = first_avg_gain + diff
                }else{
                    first_avg_loss = first_avg_loss + (diff * -1)
                }
                    
            }
            avg_loss[i] <- first_avg_loss / (start-1)
            avg_gain[i] <- first_avg_gain / (start-1)
            rs <- avg_gain[i] / avg_loss[i]
            
            rsi[i] <- 100 - (100 / (1 + rs))
            next()
        }
        
        current_gain = 0 
        current_loss = 0
        current_delta = dat[i, "Close/Last"] - dat[i-1, "Close/Last"]
        if(current_delta > 0){
            current_gain = current_delta
        }else{
            current_loss = current_delta * -1
        }
        # start is one higher than it should be since our first datapoint doesn't have
        # an gain/loss avaiable, so we start at 15 instead of 14, thus this needs to be
        # start-2 so that it's the previous 13 instead of 14
        current_avg_loss =  (avg_loss[i-1] * (start-2) + current_loss) / (start-1)
        current_avg_gain = (avg_gain[i-1] * (start-2) + current_gain) / (start-1)
        avg_gain[i] <- current_avg_gain
        avg_loss[i] <- current_avg_loss
        rs <- current_avg_gain / current_avg_loss
        rsi[i] <- 100 - (100 / (1 + rs))
        
    }
    full_dat <- cbind(dat, "Average Gain"=avg_gain, "Average Loss"=avg_loss, "RSI"=rsi)
    return(full_dat)
}

plot_rsi <- function(dat, days_back=51){
    if(days_back > 51)
        print("Not enough data to go further back")
        return()
    
    qplot(Date, RSI, data = dat )
}

main <- function(){
    
    historical_data <- get_historical_data("AMZN")
    rsi_dat <- get_rsi(historical_data, 15)
    sub_setted <-  rsi_dat[15:nrow(rsi_dat),]
    plot_rsi(dat)
    
}
