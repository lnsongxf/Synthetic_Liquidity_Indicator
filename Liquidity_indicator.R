#obveznice
#izvor podataka i ucitavanje
setwd("G:/")
getwd()
install.packages("readxl")
library(readxl)
px_last=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 1)
return=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 2)
return=sapply(return, as.numeric) 
return[is.na(return)] <- 0
return=data.frame(return)
return
px_low=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 3)
px_high=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 4)
turnover=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 6)
turnover=sapply(turnover, as.numeric) 
turnover[is.na(turnover)] <- 0
turnover=data.frame(turnover)
turnover
bid=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 7)
bid=sapply(bid, as.numeric) 
bid[is.na(bid)] <- 0
bid=data.frame(bid)
bid
ask=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 8)
ask=sapply(ask, as.numeric) 
ask[is.na(ask)] <- 0
ask=data.frame(ask)
ask
last_trade=read_excel("Obveznice_bloomberg_ticker.xlsx", sheet = 10)

#izracun amihud koeficijenta
amihud=matrix(, nrow = nrow(return), ncol = ncol(return))
for (i in 1:nrow(return))
  for (j in 2:ncol(return))
  {amihud[i,j] = abs(return[i,j])/turnover[i,j]}
amihud
amihud[!is.finite(amihud)] <- 0
amihud<- matrix(unlist(amihud), nrow(amihud), dimnames = dimnames(amihud))
amihud=rowMeans(amihud)
amihud

#izracune bid-ask spread
spread=matrix(, nrow = nrow(bid), ncol = ncol(bid))
for (i in 2:nrow(bid))
  for (j in 2:ncol(bid))
  {spread[i,j] = bid[i,j]-ask[i,j]}
spread
spread[!is.finite(spread)] <- 0
spread<- matrix(unlist(spread), nrow(spread), dimnames = dimnames(spread))
spread=rowMeans(spread)
spread

#izracun promet
turnover=rowMeans(turnover)
turnover

#izracun sharpe ratio
return=return[,-1]
return
ratio=matrix(, nrow = nrow(return), ncol = ncol(return))
ratio
for (i in 1:nrow(return))
  for (j in 1:ncol(return))
  {ratio[i,j] = return[i,j]/sd(return[i,])}
ratio
ratio[!is.finite(ratio)] <- 0
ratio<- matrix(unlist(ratio), nrow(ratio), dimnames = dimnames(ratio))
ratio=rowMeans(ratio)
ratio

#dionice
#izvor podataka i ucitavanje
setwd("G:/")
getwd()
install.packages("readxl")
library(readxl)
px_last=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 1)
px_last=sapply(px_last, as.numeric) 
px_last[is.na(px_last)] <- 0
px_last=data.frame(px_last)
px_last
return=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 2)
return=sapply(return, as.numeric) 
return[is.na(return)] <- 0
return=data.frame(return)
return
px_low=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 3)
px_low=sapply(px_low, as.numeric) 
px_low[is.na(px_low)] <- 0
px_low=data.frame(px_low)
px_low
px_high=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 4)
px_high=sapply(px_high, as.numeric) 
px_high[is.na(px_high)] <- 0
px_high=data.frame(px_high)
px_high
turnover=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 5)
turnover=sapply(turnover, as.numeric) 
turnover[is.na(turnover)] <- 0
turnover=data.frame(turnover)
turnover
spread=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 6)
spread=sapply(spread, as.numeric) 
spread[is.na(spread)] <- 0
spread=data.frame(spread)
spread
volume=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 9)
volume=sapply(volume, as.numeric) 
volume[is.na(volume)] <- 0
volume=data.frame(volume)
volume
shares_out=read_excel("Dionice_bloomberg_ticker.xlsx", sheet = 10)
shares_out=sapply(shares_out, as.numeric) 
shares_out[is.na(shares_out)] <- 0
shares_out=data.frame(shares_out)
shares_out

#izracun amihud koeficijenta
amihud=matrix(, nrow = nrow(return), ncol = ncol(return))
for (i in 1:nrow(return))
  for (j in 2:ncol(return))
  {amihud[i,j] = abs(return[i,j])/turnover[i,j]}
amihud
amihud[!is.finite(amihud)] <- 0
amihud<- matrix(unlist(amihud), nrow(amihud), dimnames = dimnames(amihud))
amihud=rowMeans(amihud)
amihud

#izracune bid-ask spread
spread=rowMeans(spread)
spread

#izracun hui-huebel ratio
hh=matrix(, nrow = nrow(return), ncol = ncol(return))
for (i in 2:nrow(return))
  for (j in 2:ncol(return))
  {hh[i,j] = ((px_high[i,j]-px_low[i,j])/px_low[i,j])/(volume[i,j]/(shares_out[i,j]*px_last[i,j]))}
hh
hh[!is.finite(hh)] <- 0
hh<- matrix(unlist(hh), nrow(hh), dimnames = dimnames(hh))
hh=rowMeans(hh)
hh

#izracun promet
turnover=rowMeans(turnover)
turnover

#izracun sharpe ratio
return=return[,-1]
return
ratio=matrix(, nrow = nrow(return), ncol = ncol(return))
ratio
for (i in 1:nrow(return))
  for (j in 1:ncol(return))
  {ratio[i,j] = return[i,j]/sd(return[i,])}
ratio
ratio[!is.finite(ratio)] <- 0
ratio<- matrix(unlist(ratio), nrow(ratio), dimnames = dimnames(ratio))
ratio=rowMeans(ratio)
ratio

#pca obveznice
spread=spread[-(1:140)]
data <- cbind(data.frame(amihud,spread,turnover,ratio))
data
install.packages("pkgbuild")
library(pkgbuild)
install.packages("ps")
library(ps)
install.packages("rlang")
library(rlang)
install.packages("factoextra")
library(factoextra)
res.pca=prcomp(data, scale=TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#tu dobijem koeficijente za indikator
res.pca 

#standardiziram varijable
amihud=data[,1]
amihud=(amihud-mean(amihud))/sd(amihud)
spread=data[,2]
spread=(spread-mean(spread))/sd(spread)
turnover=data[,3]
turnover=(turnover-mean(turnover))/sd(turnover)
ratio=data[,4]
ratio=(ratio-mean(ratio))/sd(ratio)
PC1=0.172*amihud+0.555*spread+0.564*turnover+0.585*ratio
PC1
PC2=0.951*amihud+0.070*spread-0.291*turnover-0.066*ratio
PC2

#crtam PC1 i PC2
install.packages("ggplot2")
library("ggplot2")
dates=seq(as.Date("2015-12-31"), as.Date("2019-12-31"), by="days")
dates=data.frame(dates)
dates
PC1=data.frame(PC1)
PC2=data.frame(PC2)
podaci=data.frame(dates,PC1, PC2)
podaci
ggplot(podaci,aes(dates))+geom_line(aes(y=PC1, colour="PC1")) + geom_line(aes(y=PC2, colour="PC2"))

#posebno crtam PC1=indikator_likvidnosti
indikator_likvidnosti=0.172*amihud+0.555*spread+0.564*turnover+0.585*ratio
indikator_likvidnosti=data.frame(indikator_likvidnosti)
indikator=data.frame(dates,indikator_likvidnosti)
indikator
ggplot(indikator,aes(dates))+geom_line(aes(y=indikator_likvidnosti, colour="indikator_likvidnosti"))

#pca dionice
amihud=amihud[-(1:139)]
hh=hh[-(1:139)]
turnover=turnover[-(1:140)]
ratio=ratio[-(1:139)]
data <- cbind(data.frame(amihud,spread,hh,turnover,ratio))
data
install.packages("pkgbuild")
library(pkgbuild)
install.packages("ps")
library(ps)
install.packages("rlang")
library(rlang)
install.packages("factoextra")
library(factoextra)
res.pca=prcomp(data, scale=TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#tu dobijem koeficijente za indikator
res.pca 

#standardiziram varijable
amihud=data[,1]
amihud=(amihud-mean(amihud))/sd(amihud)
spread=data[,2]
spread=(spread-mean(spread))/sd(spread)
hh=data[,3]
hh=(hh-mean(hh))/sd(hh)
turnover=data[,4]
turnover=(turnover-mean(turnover))/sd(turnover)
ratio=data[,5]
ratio=(ratio-mean(ratio))/sd(ratio)
PC1=0.046*amihud+0.644*spread+0.053*hh+0.645*turnover+0.403*ratio
PC1
PC2=-0.602*amihud+0.009*spread+0.786*hh+0.046*turnover-0.126*ratio
PC2

#crtam PC1 i PC2
install.packages("ggplot2")
library("ggplot2")
dates=seq(as.Date("2007-12-31"), as.Date("2019-12-31"), by="days")
dates=dates[-(1:312)]
dates=data.frame(dates)
dates
PC1=data.frame(PC1)
PC2=data.frame(PC2)
podaci=data.frame(dates,PC1, PC2)
podaci
ggplot(podaci,aes(dates))+geom_line(aes(y=PC1, colour="PC1")) + geom_line(aes(y=PC2, colour="PC2"))

#posebno crtam PC1=indikator_likvidnosti
indikator_likvidnosti=0.046*amihud+0.644*spread+0.053*hh+0.645*turnover+0.403*ratio
indikator_likvidnosti=data.frame(indikator_likvidnosti)
indikator=data.frame(dates,indikator_likvidnosti)
indikator
ggplot(indikator,aes(dates))+geom_line(aes(y=indikator_likvidnosti, colour="indikator_likvidnosti"))
