

###################################### Monte Carlo Simulation ####################


set.seed(1)
model.list <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
fit.list <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
pt.list <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.yee <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.ye <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.yae <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.ya <-list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.yt <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.yo <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
error.xa <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
xa.list <-  list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

yee.m <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
ye.m <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
yae.m <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
ya.m<- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
yt.m <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
yo.m <-list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
sd.yo <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
diff <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
table.mc <- data.frame(original=(rep(NA,10)),
                       change.xa=(rep(NA,10)),
                       change.ee=(rep(NA,10)),
                       change.e=(rep(NA,10)),
                       change.t=(rep(NA,10)),
                       change.a=(rep(NA,10)),
                       change.ae=(rep(NA,10)),
                       change.ee.e=(rep(NA,10)),
                       change.ee.ae=(rep(NA,10)))


model.list[[1]] <- modeldata
fit.list[[1]] <- sem(model,model.list[[1]],missing ="ML",estimator="ML",certer=T)
summary(fit.list[[1]],fit.measures=T,standardized=T,rsquare=T,modindices=F)
pt.list[[1]] <- parameterTable(fit.list[[1]])[,c("lhs","op","rhs","est")]



for (i in 2:10) {
  model.list[[i]] <- modeldata[sample(1:nrow(modeldata),nrow(modeldata), replace =T),]
  fit.list[[i]] <- sem(model,model.list[[i]],missing ="ML",estimator="ML",certer=T)
  summary(fit.list[[i]],fit.measures=T,standardized=T,rsquare=T,modindices=F)
  pt.list[[i]] <- parameterTable(fit.list[[i]])[,c("lhs","op","rhs","est")]
}

pt.list

################## original
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"original"] <- sd.yo[[i]]
  }
}
table.mc
diff
sd.yo
################## change.xa
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- rep(0,j) ## change here
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.xa"] <- sd.yo[[i]]
  }
}
table.mc
diff
sd.yo

################## change.ee
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rep(0,j)
    #error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.ee"] <- sd.yo[[i]]
  }
}
table.mc
diff
sd.yo
################## change.e
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    
    
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rep(0,j)
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.e"] <- sd.yo[[i]]
  }
}
table.mc


################## change.t
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rep(0,j)
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.t"] <- sd.yo[[i]]
  }
}
table.mc

################## change.a
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rep(0,j)
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.a"] <- sd.yo[[i]]
  }
}
table.mc

################## change.ae
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][18,4]))
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rep(0,j)
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.ae"] <- sd.yo[[i]]
  }
}
table.mc

################## change.ee.e
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rep(0,j)
    error.ye[[i]] <- rep(0,j)
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.ee.e"] <- sd.yo[[i]]
  }
}
table.mc

################## change.ee.ae
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rep(0,j)
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rep(0,j)
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][22,4]))
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.ee.ae"] <- sd.yo[[i]]
  }
}
table.mc
diff
sd.yo
################## change.ee.t
set.seed(1)
for (i in 1:10) {
  j <- 180
  sd.yo[[i]] <- 100
  diff[[i]] <- 100
  while(abs(diff[[i]]) > 0.005){
    j=j+100
    error.yee[[i]] <- rep(0,j)
    error.ye[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][19,4]))
    error.yae[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][21,4]))
    error.ya[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][20,4]))
    error.yt[[i]] <- rep(0,j)
    error.yo[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][23,4]))
    error.xa[[i]] <- rnorm(j,0,sqrt(pt.list[[i]][24,4]))
    xa.list[[i]] <- modeldata$xa[sample(1:nrow(modeldata),j, replace =T)]
    
    yee.m[[i]] <- pt.list[[i]][1,4]*xa.list[[i]]+ error.yee[[i]]
    yae.m[[i]] <- pt.list[[i]][5,4]*xa.list[[i]]+error.yae[[i]]
    ya.m[[i]]<- pt.list[[i]][4,4]*yae.m[[i]] +error.ya[[i]]
    ye.m[[i]] <- pt.list[[i]][2,4]*yee.m[[i]]+pt.list[[i]][3,4]*ya.m[[i]]+error.ye[[i]]
    yt.m[[i]] <- (pt.list[[i]][6,4]*ye.m[[i]]+ pt.list[[i]][7,4]*ya.m[[i]])+error.yt[[i]]
    yo.m[[i]] <- (pt.list[[i]][8,4]*ye.m[[i]] + pt.list[[i]][9,4]*ya.m[[i]] +pt.list[[i]][10,4]*yt.m[[i]]+pt.list[[i]][11,4]*xa.list[[i]])+error.yo[[i]]
    
    diff[[i]] <- abs(sd(yo.m[[i]])-sd.yo[[i]])
    sd.yo[[i]] <- sd(yo.m[[i]])
    print(diff[[i]])
    table.mc[i,"change.ee.t"] <- sd.yo[[i]]
  }
}
table.mc
diff
sd.yo
#### 
for (i in 1:10) {
  table.mc["mean",i] <- mean(table.mc[1:10,i])
}
for (i in 1:10) {
  table.mc["diff",i] <- table.mc[11,1]-table.mc["mean",i]
}
for (i in 1:10) {
  table.mc["% decrease",i] <- (table.mc["diff",i]/table.mc[11,1])*100
}

row.names(table.mc) <- c("Original Data","Resampling 1","Resampling 2","Resampling 3","Resampling 4",
                         "Resampling 5","Resampling 6","Resampling 7","Resampling 8","Resampling 9",
                         "mean","diff","% decrease")
table.mc
#write.csv(table.mc,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/table.mc2.csv',na="")