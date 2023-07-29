#Causal mediation analysis#
library(Hmisc)
mydata=spss.get("D:/submission/CHARLS/Causal mediation analysis/single.sav")
mydata$edu <-factor (mydata$edu)
mydata$dr1<-factor (mydata$dr1)
mydata$smoke <-factor (mydata$smoke)
mydata$dia <-factor (mydata$dia)
mydata$hyp <-factor (mydata$hyp)
mydata$heart <-factor (mydata$heart)
mydata$cancer <-factor (mydata$cancer)
mydata$stroke <-factor (mydata$stroke)

install.packages('mediation')
library(mediation)
medModel<-glm(drink~mar+age+sex+edu+smoke+dia+stroke+hyp+bmi+
                cancer+heart,family=binomial(link = "probit"),
              data=mydata,weights = weight,control=list(maxit=1000))
outModel<-glm(depression~mar*drink+age+sex+edu+smoke+dia+stroke+hyp+bmi+
                cancer+heart,family=binomial(link = "probit"), 
              data=mydata,weights = weight,control=list(maxit=500))

library(mediation)
med<-mediate(model.m =medModel,model.y=outModel,treat = 'mar',
             mediator = 'drink',weights = weight,data=mydata)
summary(med)

#To assess the robustness of the results of the causal mediation analysis#
#bootstrap resampling was employed (50% of samples, n = 100 times)#
sample=sample(nrow(mydata),3629, replace=F)#3629=n/2(7258/2)#

newmydata=mydata[sample,]  
library(mediation)
medModel<-glm(drink~mar+age+sex+edu+smoke+dia+stroke+hyp+bmi+
                cancer+heart,family=binomial(link = "probit"),
              data=newmydata,weights = weight,control=list(maxit=1000))
outModel<-glm(depression~mar*drink+age+sex+edu+smoke+dia+stroke+hyp+bmi+
                cancer+heart,family=binomial(link = "probit"), 
              data=newmydata,weights = weight,control=list(maxit=1000))

library(mediation)
med<-mediate(model.m =medModel,model.y=outModel,treat = 'mar',
             mediator = 'drink',weights = weight,data=newmydata)
summary(med)