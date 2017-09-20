library(ggplot2)
library(TTR)

stockholm_temperatures<-read.csv("/Users/batman/desktop/results_stockholm.csv")
global_temperatures <-read.csv("/Users/batman/desktop/results_global.csv")

#cleaning data frame with Stockholm average temperatures
no_na_sthlm_tem <- na.omit(stockholm_temperatures)
clean_sthlm_temp<- no_na_sthlm_tem[no_na_sthlm_tem$avg_temp > 0,]
print(clean_sthlm_temp)

#plotting cleaned Stockholm temperatures
ggplot(data=clean_sthlm_temp, aes(year, avg_temp)) + geom_line() +  
  xlab("") + ylab("Stockholm Average")

#making a timeseries and smoothing average  of stockholm temperatures
stockholm_ts<- ts(clean_sthlm_temp[,c("avg_temp")], frequency=12, start=c(1743,1))

#plotting timeseries of stockholm temperatures
plot.ts(stockholm_ts)
smooth_plot_stockholm<- SMA(stockholm_ts,n=3)
plot.ts(smooth_plot_stockholm)

#cleaning data frame with Global average temperatures
no_na_global_tem <- na.omit(global_temperatures)
clean_global_temp<- no_na_global_tem[no_na_global_tem$avg_temp > 0,]
print(clean_global_temp)

#plotting cleaned Global temperatures
ggplot(data=clean_global_temp, aes(year, avg_temp)) + geom_line() +  
  xlab("") + ylab("Global Average")

#making a timeseries and smoothing average of global temperatures
global_ts<- ts(clean_global_temp[,c("avg_temp")], frequency=12, start=c(1743,1))

#plotting timeseries of global temperatures
plot.ts(global_ts)
smooth_plot_global<- SMA(global_ts,n=3)
plot.ts(smooth_plot_global)

#making one combined_ts from Stockholm and Global timeseries data (FROM HERE STARTS THE BOOBOO)
combined_ts <-ts.union(stockholm_ts,global_ts, dframe=FALSE)

#plottig combined_ts
ts.plot(stockholm_ts, global_ts,
        gpars=list(xlab="year", ylab="avg_temp", lty=c(1:3)))

require(zoo) 
plot(smooth_plot_global)
lines(rollapply(smooth_plot_global, width = 10, FUN = mean), col = 'red')
plot(smooth_plot_stockholm)
lines(rollapply(smooth_plot_stockholm, width = 10, FUN = mean), col = 'blue')

