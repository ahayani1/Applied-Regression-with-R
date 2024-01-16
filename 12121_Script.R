#Loading dataset and removing missing values
data=read.csv(file.choose(),header=TRUE,na.strings=-9)
datanew<-na.omit(data)

#Scatterplot
plot(datanew)


#Variables
afiyr<-datanew$afiyr
sex<-datanew$sex
aage<-datanew$aage
AJBHRS<-datanew$AJBHRS
ancars<-datanew$ancars
ahhsize<-datanew$ahhsize
aqfedhi<-datanew$aqfedhi
arace<-datanew$arace
aregion<-datanew$aregion
atenure<-datanew$atenure
avote<-datanew$avote

#Change of baseline
baseaqfedhi<-as.factor(aqfedhi)
Baselineaqfedhi<-factor(baseaqfedhi,c("12", "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9"  ,"10" ,"11" , "13"))


#Explanatory analysis
summary(afiyr)
#Individual Regressions

lm.sex<-lm(afiyr~as.factor(sex),data=datanew)
lm.aage<-lm(afiyr~aage,data=datanew)
lm.AJBHRS<-lm(afiyr~AJBHRS,data=datanew)
lm.ancars<-lm(afiyr~as.factor(ancars),data=datanew)
lm.ahhsize<-lm(afiyr~ahhsize,data=datanew)
lm.Baselineaqfedhi<-lm(afiyr~Baselineaqfedhi,data=datanew)
lm.arace<-lm(afiyr~as.factor(arace),data=datanew)
lm.aregion<-lm(afiyr~as.factor(aregion),data=datanew)
lm.atenure<-lm(afiyr~as.factor(atenure),data=datanew)
lm.avote<-lm(afiyr~as.factor(avote),data=datanew)
summary(lm.sex)
summary(lm.aage)
summary(lm.AJBHRS)
summary(lm.ancars)
summary(lm.ahhsize)
summary(lm.Baselineaqfedhi)
summary(lm.arace)
summary(lm.aregion)
summary(lm.atenure)
summary(lm.avote)



 #Regression models
#With categorical
lm.full<-lm(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(arace)+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew)
summary(lm.full)


#Without categorical
lm.full2<-lm(afiyr~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+arace+aregion+atenure+avote,data=datanew)
summary(lm.full2)

#Remove arace since it is not significant
#With categorical after removing arace
lm.full3<-lm(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew)
summary(lm.full3)

#arace significance in ANOVA: arace is not f statistic significant
anova(lm.full, lm.full3, test="F")

#Without categorical after removing arace
lm.full4<-lm(afiyr~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+aregion+atenure+avote,data=datanew)

#Remove ancars since it is not significant
#With categorical after removing ancars
lm.full5<-lm(afiyr~as.factor(sex)+aage+AJBHRS+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew)
summary(lm.full5)


#ancars significance in ANOVA : ancars is f statistic significant
anova(lm.full3, lm.full5, test="F")


#Step function
lm.step<-step(lm(afiyr~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+arace+aregion+atenure+avote,data=datanew),direction="both")
summary(lm.step)

#Forward function
lm.forward<-baseballforward.lm<-step(lm(afiyr~1,data=datanew),direction="forward",scope=~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+arace+aregion+atenure+avote)
summary(lm.forward)



#Residual plots

resplots<-function(name.lm){
par(mfrow=c(2,2))
plot(name.lm$fit,rstandard(name.lm),ylab="Residuals",xlab="Fitted",main="Residuals vs Fitted")
hist(rstandard(name.lm),ylab="Frequency",xlab="Standardised Residuals",main="Histogram of Residuals")
qqnorm(rstandard(name.lm),ylab="Sample Quantiles",xlab="Theoretical Quantiles",main="QQ plot")
qqline(rstandard(name.lm))
}
resplots(lm.full3)


#Function loading

library("calibrate")

library("moments")

library("car")

#Multicollinearity with VIF
vif(lm.full3)


#Transformation : data cleansing to remove zeroes
datanew1<-datanew[!( datanew$afiyr==0),]
View(datanew1)


#Variable change with dataset
afiyr<-datanew1$afiyr
sex<-datanew1$sex
aage<-datanew1$aage
AJBHRS<-datanew1$AJBHRS
ancars<-datanew1$ancars
ahhsize<-datanew1$ahhsize
aqfedhi<-datanew1$aqfedhi
arace<-datanew1$arace
aregion<-datanew1$aregion
atenure<-datanew1$atenure
avote<-datanew1$avote

baseaqfedhi<-as.factor(aqfedhi)
Baselineaqfedhi<-factor(baseaqfedhi,c("12", "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9"  ,"10" ,"11" , "13"))

#Boxcox transformation
b=boxcox(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew1)
lambda=b$x
lik=b$y
bc=cbind(lambda,lik)
bc
bc[order(-lik),]
#Minimising value=14/33


#Log tranform
lm.log<-lm(log(afiyr)~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew1)
summary(lm.log)

#Square root transform
lm.sqrt<-lm((afiyr)^(1/2)~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew1)
summary(lm.sqrt)

#Quadratic transform
lm.quad<-lm((afiyr^(2))~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew1)
summary(lm.quad)

#Reciprocal transform
lm.reci<-lm((1/afiyr)~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew1)
summary(lm.reci)


#Residual plots for log and square root transforms
resplots(lm.log)
resplots(lm.sqrt)



#Outliers/high leverage values/influential points

lm.influence(lm.sqrt)$hat>0.010096
summary(lm.influence(lm.sqrt)$hat>0.010096)

rstandard(lm.sqrt)>2
summary( rstandard(lm.sqrt)>2)
dffits(lm.sqrt)>0.14210
summary(dffits(lm.sqrt)>0.14210)

cooks.distance(lm.sqrt)>1
summary(cooks.distance(lm.sqrt)>1)



#Outlier plot
plot.outliers.1<-function(lm.name){
    library("calibrate")
par(mfrow=c(2,2))
    #Std res and student res
plot(lm.name$fit,rstudent(lm.name),ylab="Student/Standard Residuals",xlab="Fitted",main="Student Resids vs Fitted",pch=".")#pattern
textxy(lm.name$fit,rstudent(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
abline(h=c(2,-2))
 points(lm.name$fit,rstandard(lm.name),col="red",pch=".")
textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7,col="red")

    #hat values
    plot(rstandard(lm.name),lm.influence(lm.name)$hat,pch=".",main="Hat values vs standardised residuals",xlab="Stand Res",ylab="Leverage values")
    textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
     textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
    hatval <- 2*(length(lm.name$coefficients)-1)/length(rstandard(lm.name))
    abline(h=hatval,v=c(2,-2))
     textxy(rstandard(lm.name),lm.influence(lm.name)$hat,labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)

    #cook's distance
    plot(rstandard(lm.name),cooks.distance(lm.name),pch=".",main="Cook's distance vs standardised residuals",xlab="Stand Res",ylab="Cook's distance")
    abline(h=c(1,4/(length(rstandard(lm.name)))),v=c(2,-2))
     textxy(rstandard(lm.name),cooks.distance(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)

    #dffits
 plot(rstandard(lm.name),dffits(lm.name),pch=".",main="DFFITS vs standardised residuals",xlab="Stand Res",ylab="DFFITS")
    dffitsval <- 2*sqrt((length(lm.name$coefficients)-1)/length(rstandard(lm.name)))
    abline(h=c(dffitsval, -1*dffitsval),v=c(2,-2))
     textxy(rstandard(lm.name),dffits(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
    
}


plot.outliers.1(lm.sqrt)


#Removing outliers
datanew2<-datanew1[-c(313,358,1798,1954),]

#Rerunning regression

#Variables
afiyr<-datanew2$afiyr
sex<-datanew2$sex
aage<-datanew2$aage
AJBHRS<-datanew2$AJBHRS
ancars<-datanew2$ancars
ahhsize<-datanew2$ahhsize
aqfedhi<-datanew2$aqfedhi
arace<-datanew2$arace
aregion<-datanew2$aregion
atenure<-datanew2$atenure
avote<-datanew2$avote

#Change of baseline
baseaqfedhi<-as.factor(aqfedhi)
Baselineaqfedhi<-factor(baseaqfedhi,c("12", "1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9"  ,"10" ,"11" , "13"))



#Regression models
#With categorical
lm.final<-lm(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(arace)+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew2)
summary(lm.final)


#Without categorical
lm.final2<-lm(afiyr~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+arace+aregion+atenure+avote,data=datanew2)
summary(lm.final2)

#Remove arace since it is not significant
#With categorical after removing arace
lm.final3<-lm(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew2)
summary(lm.final3)

#arace significance in ANOVA: arace is not f statistic significant
anova(lm.final, lm.final3, test="F")

#Without categorical after removing arace
lm.final4<-lm(afiyr~sex+aage+AJBHRS+ancars+ahhsize+aqfedhi+aregion+atenure+avote,data=datanew2)
summary(lm.final4)

#Remove ancars since it is not significant
#With categorical after removing ancars
lm.final5<-lm(afiyr~as.factor(sex)+aage+AJBHRS+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew2)
summary(lm.final5)


#ancars significance in ANOVA : ancars is f statistic significant
anova(lm.final3, lm.final5, test="F")


#Boxcox transformation
b=boxcox(afiyr~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew2)
lambda=b$x
lik=b$y
bc=cbind(lambda,lik)
bc
bc[order(-lik),]
#Minimising value=14/33


#Square root transform
lmfinal.sqrt<-lm((afiyr)^(1/2)~as.factor(sex)+aage+AJBHRS+as.factor(ancars)+ahhsize+Baselineaqfedhi+as.factor(aregion)+as.factor(atenure)+as.factor(avote),data=datanew2)
summary(lmfinal.sqrt)



#Residual plots for log and square root transforms
resplots(lmfinal.sqrt)



#Moonmadness
setwd("E:/")
moon<-read.csv("MoonMadness.csv",header=TRUE)
head(moon)
dim(moon)

#boxplots
boxplot(Admission~as.factor(Month),data=moon)
boxplot(Admission~as.factor(Moon),data=moon)

#linear Models
moon$Moon <- relevel(moon$Moon, "During")
moonmadness.lm<-lm(Admission~as.factor(Moon)+as.factor(Month),data=moon)
summary(moonmadness.lm)
moonmadness.lm.2<-lm(Admission~as.factor(Moon),data=moon)
summary(moonmadness.lm.2)

#resplots
resplots<-function(name.lm){
par(mfrow=c(2,2))
plot(rstandard(name.lm),ylab="Residuals",main="Index plot")
plot(name.lm$fit,rstandard(name.lm),ylab="Residuals",xlab="Fitted",main="Residuals vs Fitted")
hist(rstandard(name.lm),ylab="Residuals",main="Histogram of Residuals")
qqnorm(rstandard(name.lm),main="QQ plot")
qqline(rstandard(name.lm))
}
resplots(moonmadness.lm)
resplots(moonmadness.lm.2)

#pairwise t test
pairwise.t.test(moon$Admission,as.factor(moon$Moon),p.adjust="bonferroni")


#full models
full.lm<-lm(Admission~as.factor(Moon)+as.factor(Month),data=moon)
summary(full.lm)
anova(full.lm)
full.1.lm<-lm(Admission~as.factor(Month),data=moon)
full.2.lm<-lm(Admission~as.factor(Moon),data=moon)
anova(full.lm,full.1.lm)
anova(full.lm,full.2.lm)

#interacion plots
interaction.plot(moon$Month,moon$Moon, moon$Admission)
interaction.lm<-lm(Admission~as.factor(Moon)+as.factor(Month)+Moon*Month,data=moon)
summary(interaction.lm)
anova(interaction.lm)
resplots(interaction.lm)

#tranformations
log.lm<-lm(log(Admission)~as.factor(Moon)+as.factor(Month),data=moon)
summary(log.lm)
resplots(log.lm)
(log.lm)
sqrt.lm<-lm(sqrt(Admission)~as.factor(Moon)+as.factor(Month),data=moon)
summary(sqrt.lm)
resplots(sqrt.lm)
reci.lm<-lm(Admission^-1~as.factor(Moon)+as.factor(Month),data=moon)
summary(reci.lm)
resplots(reci.lm)

moon$Moon <- relevel(moon$Moon, "During")
full.lm.1<-lm(Admission~as.factor(Moon)+as.factor(Month),data=moon)
summary(full.lm.1)

#Outlier Plots
plot.outliers.1<-function(lm.name){
    library("calibrate")
par(mfrow=c(2,2))
    #Std res and student res
plot(lm.name$fit,rstudent(lm.name),ylab="Student/Standard Residuals",xlab="Fitted",main="Student Resids vs Fitted",pch=".")#pattern
textxy(lm.name$fit,rstudent(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
abline(h=c(2,-2))
 points(lm.name$fit,rstandard(lm.name),col="red",pch=".")
textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7,col="red")

    #hat values
    plot(rstandard(lm.name),lm.influence(lm.name)$hat,pch=".",main="Hat values vs standardised residuals",xlab="Stand Res",ylab="Leverage values")
    textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
     textxy(lm.name$fit,rstandard(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
    hatval <- 2*(length(lm.name$coefficients)-1)/length(rstandard(lm.name))
    abline(h=hatval,v=c(2,-2))
     textxy(rstandard(lm.name),lm.influence(lm.name)$hat,labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)

    #cook's distance
    plot(rstandard(lm.name),cooks.distance(lm.name),pch=".",main="Cook's distance vs standardised residuals",xlab="Stand Res",ylab="Cook's distance")
    abline(h=c(1,4/(length(rstandard(lm.name)))),v=c(2,-2))
     textxy(rstandard(lm.name),cooks.distance(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)

    #dffits
 plot(rstandard(lm.name),dffits(lm.name),pch=".",main="DFFITS vs standardised residuals",xlab="Stand Res",ylab="DFFITS")
    dffitsval <- 2*sqrt((length(lm.name$coefficients)-1)/length(rstandard(lm.name)))
    abline(h=c(dffitsval, -1*dffitsval),v=c(2,-2))
     textxy(rstandard(lm.name),dffits(lm.name),labs=seq(1,length(rstandard(lm.name))),offset=0.2,cex=0.7)
    
}
install.packages("calibrate")
library("MASS")
plot.outliers.1(full.lm.1)
moon.1<- moon[-c(101,93,21,40,53,81),] 

full.lm.2<-lm(Admission~as.factor(Moon)+as.factor(Month),data=moon.1)
summary(full.lm.2)
resplots(full.lm.2)
