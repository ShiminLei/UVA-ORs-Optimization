

############### SEM ################################


xa <- tapply(X=data.main[which(data.main$start==1),]$prime_duration,INDEX=data.main[which(data.main$start==1),]$Actual.Surgery.Date,FUN=sum) # allocated hours during prime time for the ith day
ye_p <- tapply(X=data.main.elective$acthours,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum) #  Actual hours of elective caes
ya_p <- tapply(X=data.main.addson$acthours,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum) # actual hours of add on cases
yee_p <- tapply(X=data.main.elective$esthours,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum) #  estimated hours of elective cases
yae_p <- tapply(X=data.main.addson$esthours,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum) # estimated hours of add on cases

ye_o <- tapply(X=data.main.elective$act_over_time,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum) 
ya_o <- tapply(X=data.main.addson$act_over_time,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum)
yee_o <- tapply(X=data.main.elective$est_over_time,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum) 
yae_o <- tapply(X=data.main.addson$est_over_time,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum) 

yee <- tapply(X=data.main.elective$est_duration,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum)
yae <- tapply(X=data.main.addson$est_duration,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum)
ye <- tapply(X=data.main.elective$act_duration,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum)
ya <- tapply(X=data.main.addson$act_duration,INDEX=data.main.addson$Actual.Surgery.Date,FUN=sum)

yt <- tapply(X=data.main$Turnover_new,INDEX=data.main$Actual.Surgery.Date,FUN=sum) 


#ye_total <- tapply(X=data.main.elective$Total.Case.Minutes,INDEX=data.main.elective$Actual.Surgery.Date,FUN=sum) #  Actual hours of elective caes
#temp <- data.frame(c( data.frame(ye_p),  data.frame(ye_o) , data.frame(ye_total/60)))

yu <- xa - ye_p - ya_p
yue <- xa - yee_p-yae_p
yo <- ye_o + ya_o
yoe <- yee_o + yae_o

underutilization <- yu/xa
underutilization_est <- yue/xa
overutilization <- yo/xa
overutilization_est <- yoe/xa

utili_frame <- data.frame(underutilization,underutilization_est,overutilization,overutilization_est)
utili_frame$actual.Surgery.Date <- as.Date(rownames(utili_frame),format='%m/%d/%Y')

modeldata <- data.frame(xa,yee_p,ye_p,ya_p,yae_p,yee_o,ye_o,ya_o,yu,yo,yt,yee,yae,ya,ye)
#modeldata <- data.frame(yee,ye,ya ,yae,yt ,yo ,xa,yoe)
modeldata$actual.Surgery.Date <- as.Date(rownames(modeldata),format='%m/%d/%Y')
modeldata$week <- weekdays(modeldata$actual.Surgery.Date)
modeldata$week <- factor(modeldata$week,ordered = T,levels =c("Monday","Tuesday","Wednesday","Thursday","Friday"))
modeldata <- modeldata[order(modeldata$actual.Surgery.Date),]
#write.csv(modeldata,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/modeldata.csv',na="")
modeldata <- na.omit(modeldata)
varTable(modeldata) 

# modeldata$xa <- scale(as.numeric(modeldata$xa),center=T, scale=F)
# modeldata$yee <- scale(as.numeric(modeldata$yee),center=T, scale=F)
# modeldata$ye <- scale(as.numeric(modeldata$ye),center=T, scale=F)
# modeldata$ya <- scale(as.numeric(modeldata$ya),center=T, scale=F)
# modeldata$yae <- scale(as.numeric(modeldata$yae),center=T, scale=F)
# modeldata$yt <- scale(as.numeric(modeldata$yt),center=T, scale=F)
# modeldata$yo <- scale(as.numeric(modeldata$yo),center=T, scale=F)

modeldata$xa <- as.numeric(modeldata$xa)
modeldata$yee <- as.numeric(modeldata$yee)
modeldata$ye <- as.numeric(modeldata$ye)
modeldata$ya <- as.numeric(modeldata$ya)
modeldata$yae <- as.numeric(modeldata$yae)
modeldata$yt <- as.numeric(modeldata$yt)
modeldata$yo <- as.numeric(modeldata$yo)

library(mvnormtest)
multitest.matrix <- as.matrix(modeldata)[,1:7]
multitest.matrix <- matrix(as.numeric(multitest.matrix), dim(multitest.matrix), dimnames = dimnames(multitest.matrix))
mshapiro.test(multitest.matrix)


save(modeldata, file="modeldata_complete_2018")

setwd("C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/Data")
load("modeldata_complete_2018")

library(Hmisc)
library(lavaan)
library(semPlot)

cov(modeldata[,1:7])
covariate <- cov(modeldata[,1:7])
covariate<- covariate[c("xa","yee","ye","ya" ,"yae","yt" ,"yo"),c("xa","yee","ye","ya" ,"yae","yt" ,"yo")]
covariate
#write.csv(covariate,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/covariate.csv',na="")

correlation <- cor(modeldata[,1:7])
correlation<- correlation[,c("yee","ye","ya" ,"yae","yt" ,"yo" ,"xa")]
correlation

#write.csv(correlation,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/correlation.csv',na="")

# #### over time plot
require(ggplot2)
require(reshape2)
# lineplot_day <- data.frame(date=modeldata$actual.Surgery.Date,
#                            EST_over=as.vector(modeldata$yoe),
#                            over=as.vector(modeldata$yo))
#
# lineplot_day2 <- melt(lineplot_day,id.vars="date",variable.name="Class",value.name="TotalHours")
#
# ggplot(lineplot_day2, aes(x=date, y=TotalHours, colour=Class, group=Class,shape=Class)) +
#   geom_line()+
#   xlab("Date") +
#   ylab("Total Hours")+
#   #ggtitle("Time Plot") +
#   theme_bw() +
#   scale_color_manual(values = c("#666666","red"),
#                      name="Type",    # Legend label, use darker colors
#                      breaks=c("EST_over", "over"),
#                      labels=c("Estimated Overutilized Hours", "Actual Overutilized Hours")
#   )
#
#
# #### time plot####
# 
lineplot_day <- data.frame(date=modeldata$actual.Surgery.Date,
                           EST_ELECTIVE_TIME=as.vector(modeldata$yee),
                           EST_ADDSON_TIME=as.vector(modeldata$yae),
                           ACT_ELECTIVE_TIME=as.vector(modeldata$ye),
                           ACT_ADDSON_TIME=as.vector(modeldata$ya))
lineplot_day2 <- melt(lineplot_day,id.vars="date",variable.name="Class",value.name="TotalHours")
lineplot_day$date[which.min(lineplot_day$ACT_ELECTIVE_TIME)]


ggplot(lineplot_day2, aes(x=date, y=TotalHours, colour=Class, group=Class,shape=Class)) +
  geom_line()+
  xlab("Date") +
  ylab("Total Hours")+
  #ggtitle("Time Plot") +
  theme_bw() +
  scale_color_manual(values = c("#3399FF","#00CC66","#FF3366","#FF9900"),
                     name="Type",    # Legend label, use darker colors
                     breaks=c("EST_ELECTIVE_TIME", "EST_ADDSON_TIME","ACT_ELECTIVE_TIME","ACT_ADDSON_TIME"),
                     labels=c("Estimated Elective Time", "Estimated Add-on Time","Actual Elective Time","Actual Add-on Time")
  )
# #### utilization plot####
# 
lineplot_uti <- data.frame(date=utili_frame$actual.Surgery.Date,
                           underutilization=-as.vector(utili_frame$underutilization),
                           underutilization_est=-as.vector(utili_frame$underutilization_est),
                           overutilization=as.vector(utili_frame$overutilization),
                           overutilization_est=as.vector(utili_frame$overutilization_est))
lineplot_uti2 <- melt(lineplot_uti,id.vars="date",variable.name="Class",value.name="Utilization")


ggplot(lineplot_uti2, aes(x=date, y=Utilization, colour=Class, group=Class,shape=Class)) +
  geom_line()+
  xlab("Date") +
  ylab("Utilization")+
  #ggtitle("Time Plot") +
  ylim(-0.4, 0.4)+
  theme_bw() +
  scale_color_manual(values = c("#3399FF","#00CC66","#FF3366","#FF9900"),
                     name="Type",    # Legend label, use darker colors
                     breaks=c("underutilization", "underutilization_est","overutilization","overutilization_est"),
                     labels=c("Actual Underutilization", "Estimated Underutilization","Actual Overutilization","Estimated Overutilization")
  )
####### markout 
# ggplot(lineplot_day2, aes(x=date, y=TotalHours, colour=Class, group=Class)) + 
#   geom_point(x=as.Date('2018-01-17'),y=136.4000,size=3,color="brown") +
#   geom_point(x=as.Date('2017-07-20'),y=164.9333,size=3) +
#   geom_point(x=as.Date('2017-07-28'),y=159.1333,size=3) +
#   geom_line()+
#   xlab("Date") +
#   ylab("Total Hours")+
#   #ggtitle("Time Plot") +
#   theme_bw() +
#   scale_color_manual(values = c("#3399FF","#00CC66","#FF3366","#FF9900"),
#                      name="Type",    # Legend label, use darker colors
#                      breaks=c("EST_ELECTIVE_TIME", "EST_ADDSON_TIME","ACT_ELECTIVE_TIME","ACT_ADDSON_TIME"),
#                      labels=c("Estimated Elective Time", "Estimated Add-on Time","Actual Elective Time","Actual Add-on Time")
#   )
################ under_utilized model########
model <- '
yee_p ~ xa
ye_p ~ yee_p
ya_p ~ yae_p
yae_p ~ yee_p
yt ~ ye_p+ya_p
#yo ~ ye+ya+yt+xa

# yee~0*1
# ye~0*1
# ya~0*1
# yae~0*1
# yt~0*1
# yo~0*1

'


fit <- sem(model,modeldata,estimator="ML")
summary(fit,fit.measures=T,standardized=T,rsquare=T,modindices=F)


###################################### SEM


model <- '
yee ~ xa
ye ~ yee +ya
ya ~ yae
yae ~ xa
yt ~ ye+ya
yo ~ ye+ya+yt+xa

yee~0*1
ye~0*1
ya~0*1
yae~0*1
yt~0*1
yo~0*1

'


fit <- sem(model,modeldata,estimator="ML")
summary(fit,fit.measures=T,standardized=T,rsquare=T,modindices=F)
modindices(fit, sort=1,maximum.number = 40 )
pt <- parameterTable(fit)[,c("lhs","op","rhs","est")]
pt 
################# ######################################### residual.matrix
lavInspect(fit,"cov.ov")
re.covariate <- lavInspect(fit,"cov.ov")# model-implied observed-variable covariances
#lavInspect(fit,"mean.ov")# model-implied observed-variable means
# ?lavInspect # see other available options

residual.matrix <-covariate -re.covariate
residual.matrix
###################################################### Hadi's nultivariate outlier detiction ####

# yee=0.625xa+458.144  
# ye= 0.779 yee+  163.822
# ya=   0.993yae+ -0.028 ye +21.971
# yae=-0.011 yee+ 0.011  xa + 70.032 
# yt=0.193  ye+    0.367ya +   65.810 
# yo=  0.366 ye  + 0.489  ya +  0.237 yt    + -0.329xa  + 26.493

E_yee <- modeldata$yee-pt[1,4]*modeldata$xa   #error term calculated
E_ye <- modeldata$ye-(pt[2,4] *modeldata$yee+pt[3,4]*modeldata$ya)
E_ya<- modeldata$ya-pt[4,4]*modeldata$yae
E_yae <- modeldata$yae-pt[5,4]*modeldata$xa
E_yt <- modeldata$yt-(pt[6,4]*modeldata$ye+ pt[7,4]*modeldata$ya)
E_yo <- modeldata$yo-(pt[8,4]*modeldata$ye + pt[9,4]*modeldata$ya +pt[10,4]*modeldata$yt+pt[11,4]*modeldata$xa)

error.frame <- data.frame(E_yee,E_ye,E_ya,E_yae,E_yt,E_yo)
row.names(error.frame) <- row.names(modeldata)
error.frame
cor(error.frame)

library(robustX)
distance <- mvBACON(error.frame)
#plot(error.frame)
points(error.frame[!distance$subset, ], col = "red", pch = 19)
rownames(error.frame[!distance$subset,])
error.frame[!distance$subset, ]

modeldata[rownames(error.frame[!distance$subset,]),]
modeldata <- modeldata[rownames(error.frame[distance$subset,]),]
cov(modeldata[,1:7])
covariate <- cov(modeldata[,1:7])
covariate<- covariate[c("xa","yee","ye","ya" ,"yae","yt" ,"yo"),c("xa","yee","ye","ya" ,"yae","yt" ,"yo")]
covariate
#write.csv(covariate,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/covariate2.csv',na="")
correlation <- cor(modeldata[,1:7])
correlation<- correlation[c("xa","yee","ye","ya" ,"yae","yt" ,"yo"),c("xa","yee","ye","ya" ,"yae","yt" ,"yo")]
correlation
#write.csv(correlation,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/correlation.csv',na="")


fit <- sem(model,modeldata,estimator="ML",certer=T)
summary(fit,fit.measures=T,standardized=T,rsquare=T,modindices=F)
pt <- parameterTable(fit)[,c("lhs","op","rhs","est")]

AIC(fit) 

#missing patterns
lavInspect(fit,"patterns")
varTable(fit)

lavInspect(fit,"cov.ov")
re.covariate <- lavInspect(fit,"cov.ov")# model-implied observed-variable covariances
#lavInspect(fit,"mean.ov")# model-implied observed-variable means
# ?lavInspect # see other available options
re.correlation <- cov2cor(re.covariate)
re.correlation <- re.correlation[c("xa","yee","ye","ya" ,"yae","yt" ,"yo"),c("xa","yee","ye","ya" ,"yae","yt" ,"yo")]

residual.matrix <-covariate -re.covariate
residual.matrix
#write.csv(residual.matrix,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/residual.matrix.csv',na="")

cor.residual <- correlation-re.correlation
cor.residual 
#write.csv(cor.residual,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/cor.residual.csv',na="")