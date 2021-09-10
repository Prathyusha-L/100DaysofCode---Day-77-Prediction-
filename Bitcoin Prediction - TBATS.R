library(forecast)
library(ggplot2)
library(ggfortify)
library(ggplot2)
library(highcharter)
library(zoo)
library(tseries)


###Using api()
api_file <- "https://blockchain.info/charts/market-price?timespan=365days&format=csv"
csv_data <- read.csv(file=api_file, header = FALSE)
csv_df <- data.frame(csv_data)



csv_df$Close <- csv_df$V2


edf  <- csv_df

tail(edf)
edf$v7_MA = ma(edf$Close, order=7)



#STL  
edf_ma <- ts(na.omit(edf$v7_MA), frequency=10)
decomp_edf <- stl(edf_ma, s.window="periodic")
#plot(decomp_rental)
adj_edf <- seasadj(decomp_edf)




#hfit<-HoltWinters(adj_edf)
#hfit<-auto.arima(adj_edf)
hfit<-tbats(adj_edf)
f2<-forecast(hfit, h=(30*3))

df <- fortify(f2)
df$Data<-round(df$Data)
df$Fitted<-round(df$Fitted)
df$Index<-seq(as.Date("2016-12-26"), (as.Date("2017-12-25")+(30*(3))),length.out=length(df$Index))


highchart(type = "stock") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_title(text = "Bitcoin Prediction using Prediction models") %>% 
  hc_add_series(df, "line", hcaes(Index, Data), name = "Actual Price", color="black") %>% 
  hc_add_series(df, "line", hcaes(Index, Fitted), name = "Predicted Price") %>%
  hc_add_series(df, "line", hcaes(Index, `Point Forecast`), name = "Forecasted Price") %>% 
  hc_add_series(df, "arearange", hcaes(Index, low = `Lo 95`, high = `Hi 95`), name = "Prediction Interval") 


