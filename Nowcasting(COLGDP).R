######################### Nowcasting Colombia's GDP Annual Growth ##########################

"The objective of this project is to produce nowcasts of GDP growth in Colombia. A DFM would be used and 
be comapred against commoan benchmark models (ARIMA and a RW)). Monthly variables would be used to improve the 
information set used when forecasting current quarter GDP growth, as publication lags suggest better estimates of 
growth just before its publication as more information is available."

### Import packges

for(pkg in c("fpp2","readxl","gets","nowcasting")){
  library(pkg, character.only = TRUE)
}

dframe1 <- ts(read_excel("~/Documents/Estadistica/Predictive Analytics (BC)/D4data.xlsx", sheet = "NOWCASTDF"),frequency = 12,start = c(2006,1))[,-1]
GDPgrowth<-ts(na.omit(read_excel("~/Documents/Estadistica/Predictive Analytics (BC)/D4data.xlsx", sheet = "NOWCASTS")),frequency = 4,start = c(2006,1))[,2]   

# Descriptive statistics

autoplot(GDPgrowth)+labs(y="%",x="Date",title = "Colombia's GDP Growth",subtitle = "Y-O-Y")
autoplot(dframe1[,c("ISE","ISEP","ISES","ISET")])+labs(y="%",x="Date",title = "Colombia's ISE Growth",subtitle = "Y-O-Y")
autoplot(dframe1[,"UNEM"])+labs(y="%",x="Date",title = "Colombia's Unemployment Rate")
autoplot(dframe1[,"INF"])+labs(y="%",x="Date",title = "Colombia's Inflation Rate",subtitle = "CPI Y-O-Y")

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
}

ggpairs(
  as.data.frame(na.omit(dframe1[,-1])), lower = list(continuous = wrap(lowerFn, method = "lm",se=TRUE)),
  diag = list(continuous = wrap("barDiag", colour = "black")),
  upper = list(continuous = wrap("cor", size = 5))
)

# Parameters

freq<-c(4,rep(12,ncol(dframe1)-1))
delay1<-c(46,31,49,49,49,49,0,4)

# Forecast Evaluation

fcst_dates <- seq.Date(from = as.Date("2019-03-30"),to = as.Date("2022-09-30"),
                       by = "quarter")
fcst_results <- NULL
for(date in fcst_dates){
  vingdp<-PRTDB(dframe1,delay = delay1,vintage = date)
  NAs<-matrix(rep(NA,20),nrow = 20,ncol=ncol(dframe1))
  vingdp1<-ts(rbind(vingdp,NAs),frequency = 12,start =c(2006,1)) 
  print(vingdp[,"ISE"])
  nowgdp<-nowcast(formula = GDP~., frequency = freq,data = vingdp1,r =4, q = 2 , p=1, method = "2s_agg")
  print(na.omit(nowgdp$yfcst[,3])[1])
  fcst_results<-c(fcst_results,na.omit(nowgdp$yfcst[,3])[1])
}

farima<-NULL
for (i in 1:(15+1)) {
  farima[[i]]<-forecast::forecast(auto.arima(subset(GDPgrowth,end=(51+i))),h=1)$mean[1]
}

frw<-NULL
for (i in 1:(15+1)) {
  frw[[i]]<-(subset(GDPgrowth,start =(51+i) ,end=(51+i)))
}

forecast::accuracy(fcst_results,GDPgrowth[53:67])
forecast::accuracy(unlist(farima),GDPgrowth[53:67])
forecast::accuracy(unlist(frw),GDPgrowth[53:67])

autoplot(ts(fcst_results,start=c(2019,1),end=c(2022,3),frequency = 4),series="Nowcast")+
  autolayer(ts(unlist(farima),start=c(2019,1),end=c(2022,3),frequency = 4),series="ARIMA")+
  autolayer(ts(unlist(frw),start=c(2019,1),end=c(2022,3),frequency = 4),series="RW")+
  autolayer(GDPgrowth)+labs(title = "Forecast Comparison",x="Date",y="%")+
  guides(colour = guide_legend(title = "Forecast"))
  
summary(nowgdp$reg)

# Nowcasts for current quarter

NAs<-matrix(rep(NA,20),nrow = 20,ncol=ncol(dframe1))
df2<-ts(rbind(dframe1,NAs),start = c(2006,1),frequency = 12)
now<-nowcast(formula = GDP~., frequency = freq,data = df2,r =4, q = 2 , p=1, method = "2s_agg")
now$yfcst
nowcast.plot(now, type = "fcst")
nowcast.plot(now, type = "factors")

##################################################