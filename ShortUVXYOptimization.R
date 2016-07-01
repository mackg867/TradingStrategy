########
# VIX/VXV Ratio Filter for UVXY Parameter Optimization
#########
library(quantmod)
getSymbols("^VIX",src="yahoo")
getSymbols("^VXV",src="yahoo")
getSymbols("VXX",src="yahoo")
getSymbols("UVXY",src="yahoo")
VXV = VXV[-1794,] #For some reason the data includes President's Day in 2014

close = as.numeric(UVXY[,6])
open = ((as.numeric(UVXY[,1])-as.numeric(UVXY[,4]))/as.numeric(UVXY[,4]))*close + close
high = ((as.numeric(UVXY[,2])-as.numeric(UVXY[,4]))/as.numeric(UVXY[,4]))*close + close
low = ((as.numeric(UVXY[,3])-as.numeric(UVXY[,4]))/as.numeric(UVXY[,4]))*close + close
vxv = as.numeric(get("VXV")[1199:nrow(VXV),6])
vix = as.numeric(get("VIX")[1199:nrow(VIX),6])

slip = 0
sl = seq(.2,.7,.02)
pt = seq(.2,.7,.02)
rpt = .3

output = matrix(rep(NA,length(sl)*length(pt)),nrow=length(pt),ncol=length(sl))

for(a in 1:length(sl)){
  for(b in 1:length(pt)){

inTrade = 0
enterPrice = 0
exitPrice = 0
enter = 0
stopPrice = 0
profitPrice = 0
returns = c()
block = 0

runEquity = rep(-1000,length(close))
runEquity[1] = 100000
saveEquity = 0
nShares = 0

enterPrices = c()
enterDays = c()
exitPrices = c()
exitDays = c()
stopPrices = c()
stopDays = c()

for(i in 2:length(close)){
  
  block = 0
  #Enter
  if(inTrade == 0 && vix[i-1]/vxv[i-1] < .98){
    enterPrice = open[i] - open[i]*slip
    stopPrice = enterPrice + enterPrice*sl[a]
    profitPrice = enterPrice - enterPrice*pt[b]
    inTrade = 1
    enter = 0
    block = 1
    
    nShares = (runEquity[i-1]*rpt)/(stopPrice-enterPrice)
    saveEquity = runEquity[i-1]-nShares*enterPrice
    enterPrices = c(enterPrices,enterPrice)
    enterDays = c(enterDays,i)
    stopPrices = c(stopPrices,stopPrice)
    stopDays = c(stopDays,i)
  }
  

  #Exit Stop Loss
  if(inTrade == 1 && open[i] > stopPrice && block == 0){
    exitPrice = open[i] + open[i]*slip
    inTrade = 0
    returns = c(returns,(enterPrice-exitPrice)/enterPrice)
    
    runEquity[i-1] = nShares*(2*enterPrice - exitPrice) + saveEquity
    exitPrices = c(exitPrices,exitPrice)
    exitDays = c(exitDays,i)
  }else if(inTrade == 1 && high[i] > stopPrice && block == 0){
    exitPrice = stopPrice + stopPrice*slip
    inTrade = 0
    returns = c(returns,(enterPrice-exitPrice)/enterPrice)
    
    runEquity[i-1] = nShares*(2*enterPrice - exitPrice) + saveEquity
    exitPrices = c(exitPrices,exitPrice)
    exitDays = c(exitDays,i)
  }
  
  #Exit Profit Target
  if(inTrade == 1 && open[i] < profitPrice && block == 0){
    exitPrice = open[i] + open[i]*slip
    inTrade = 0
    returns = c(returns,(enterPrice-exitPrice)/enterPrice)
    
    runEquity[i-1] = nShares*(2*enterPrice-exitPrice) + saveEquity
    exitPrices = c(exitPrices,exitPrice)
    exitDays= c(exitDays,i)
  }else if(inTrade == 1 && low[i] < profitPrice && block == 0){
    exitPrice = profitPrice + profitPrice*slip
    inTrade = 0
    returns = c(returns,(enterPrice-exitPrice)/enterPrice)
    
    runEquity[i-1] = nShares*(2*enterPrice - exitPrice) + saveEquity
    exitPrices = c(exitPrices,exitPrice)
    exitDays= c(exitDays,i)
  }
  
  #Update Stop
  if(inTrade == 1 && (close[i]+close[i]*sl[a]) < stopPrice){
    stopPrice = close[i]+close[i]*sl[a]
    stopPrices = c(stopPrices,stopPrice)
    stopDays = c(stopDays,i)
  }
  
  #Update Equity
  if(inTrade == 0){
    runEquity[i] = runEquity[i-1]
  }else{
    runEquity[i] = nShares*(2*enterPrice-close[i]) + saveEquity
  }
  
}

output[b,a] = (runEquity[length(runEquity)]/runEquity[1])^(1/(length(runEquity)/250))

}}
