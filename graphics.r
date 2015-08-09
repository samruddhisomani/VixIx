library(ggplot2)
library(reshape2)
library(cowplot)
library(foreach)
library(mosaic)

data <- read.csv("~/McCombs/Summer/Programming/VixIx/dropna.csv", header=TRUE)

data$Date<-as.Date(data$Date)


plotdata=data[,c(1:8)]

plotdatamelt=melt(plotdata, id=c('Date','SPY','RUT','DJI'))

gspy<-ggplot(aes(x=SPY, y=value), data=plotdatamelt)+geom_point(aes(color=variable, alpha=.005)) +geom_smooth(aes(color=variable), method='lm', se=FALSE, size=1.3) + scale_alpha(guide=FALSE)+labs(x="SPY Returns",y="Volatility Index Returns", title="SPY")+scale_color_discrete("Index")

grut<-ggplot(aes(x=RUT, y=value), data=plotdatamelt)+geom_point(aes(color=variable, alpha=.005)) +geom_smooth(aes(color=variable), method='lm', se=FALSE, size=1.3) + scale_alpha(guide=FALSE)+labs(x="RUT Returns",y="Volatility Index Returns", title="RUT")+scale_color_discrete("Index")

gdji<-ggplot(aes(x=DJI, y=value), data=plotdatamelt)+geom_point(aes(color=variable, alpha=.005)) +geom_smooth(aes(color=variable), method='lm', se=FALSE, size=1.3) + scale_alpha(guide=FALSE)+ggtitle("DJI")+labs(x="DJI Returns",y="Volatility Index Returns", title="DJI")+scale_color_discrete("Index")

plot_grid(gspy,grut,gdji, ncol=3)


plotdata=plotdata[,-1]

betas<-apply(plotdata,2, function (x) coef(summary(lm(x~plotdata[,7])))[2])

set.seed(0)

t=1000
totalwealth=10000
ndays=20

sA='VIX'
sB='SPY'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVIXSPY = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

sA='RVX'
sB='SPY'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simRVXSPY = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}


sA='VXD'
sB='SPY'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVXDSPY = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

sA='VIX'
sB='RUT'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVIXRUT = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

sA='RVX'
sB='RUT'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simRVXRUT = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}


sA='VXD'
sB='RUT'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVXDRUT = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

sA='VIX'
sB='DJI'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVIXDJI = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

sA='RVX'
sB='DJI'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simRVXDJI = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}


sA='VXD'
sB='DJI'
returns=plotdata[,c(sA,sB)]
wA=betas[sB]/(betas[sB]-betas[sA])
wB=betas[sA]/(betas[sA]-betas[sB])

simVXDDJI = foreach(i=1:t, .combine='rbind') %do% {
  totalwealth = 10000
  weights = c(wA,wB)
  holdings = weights * totalwealth
  wealthtracker = rep(0, ndays) 
  for(today in 1:ndays) {
    return.today = resample(returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth}
  wealthtracker
}

VXDDJI=as.data.frame(simVXDDJI) #8500-13000
RVXDJI=as.data.frame(simRVXDJI) #9000-12000
VIXDJI=as.data.frame(simVIXDJI) #8500-12000
VXDSPY=as.data.frame(simVXDSPY) #8500-13000
RVXSPY=as.data.frame(simRVXSPY) #9000-12000
VIXSPY=as.data.frame(simVIXSPY) #8500-12000
VXDRUT=as.data.frame(simVXDRUT) #8500-13000
RVXRUT=as.data.frame(simRVXRUT) #9000-12000
VIXRUT=as.data.frame(simVIXRUT) #8500-12000


gVXDDJI<-ggplot(aes(x=V20), data=VXDDJI)+geom_histogram(binwidth=100, alpha=.25) +geom_vline(aes(xintercept=mean(V20, na.rm=T)), color='red',size=1)+coord_cartesian(ylim=c(0,120),xlim=c(8500,13000))+labs(x="Wealth after Twenty Days",y="Count",title="VXD-DJI")

gRVXDJI<-ggplot(aes(x=V20), data=RVXDJI)+geom_histogram(binwidth=100, alpha=.25) +geom_vline(aes(xintercept=mean(V20, na.rm=T)), color='red',size=1)+coord_cartesian(ylim=c(0,120),xlim=c(8500,13000))+labs(x="Wealth after Twenty Days",y="Count",title="RVX-DJI")

gVIXDJI<-ggplot(aes(x=V20), data=VIXDJI)+geom_histogram(binwidth=100, alpha=.25) +geom_vline(aes(xintercept=mean(V20, na.rm=T)), color='red',size=1)+coord_cartesian(ylim=c(0,120),xlim=c(8500,13000))+labs(x="Wealth after Twenty Days",y="Count",title="VIX-DJI")

plot_grid(gVXDDJI,gRVXDJI,gVIXDJI,ncol=3)

sumstats<-function(x){
  q<-c(quantile(x[,20], 0.05),quantile(x[,20], 0.95))
  append(summary(x[,20]),q)
}

sumVXDDJI<-sumstats(simVXDDJI)
sumVIXDJI<-sumstats(simVIXDJI)
sumRVXDJI<-sumstats(simRVXDJI)
sumVXDRUT<-sumstats(simVXDRUT)
sumVIXRUT<-sumstats(simVIXRUT)
sumRVXRUT<-sumstats(simRVXRUT)
sumVXDSPY<-sumstats(simVXDSPY)
sumVIXSPY<-sumstats(simVIXSPY)
sumRVXSPY<-sumstats(simRVXSPY)

cbind(sumVXDDJI,sumVIXDJI,sumRVXDJI, sumVXDRUT,sumVIXRUT,sumRVXRUT,sumVXDSPY,sumVIXSPY,sumRVXDJI)

DJI<-c(simVXDDJI[,20],simVIXDJI[,20],simRVXDJI[,20])
q<-c(quantile(DJI, 0.05),quantile(DJI, 0.95))

SPY<-c(simVXDSPY[,20],simVIXSPY[,20],simRVXSPY[,20])
p<-c(quantile(SPY, 0.05),quantile(SPY, 0.95))

RUT<-c(simVXDRUT[,20],simVIXRUT[,20],simRVXRUT[,20])
n<-c(quantile(RUT, 0.05),quantile(RUT, 0.95))

DJIq<-append(summary(DJI),q)
SPYp<-append(summary(SPY),p)
RUTn<-append(summary(RUT),n)

cbind(DJIq,SPYp,RUTn)

summary(simVXDDJI[20])
