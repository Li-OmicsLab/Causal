library(Hmisc)
mydata=spss.get("C:/depression/single.sav")
mydata$edu<-factor (mydata$edu)

mydata$diabetes <-factor (mydata$diabetes)
mydata$hyp <-factor (mydata$hyp)
mydata$heart <-factor (mydata$heart)
mydata$cancer <-factor (mydata$cancer)
mydata$stroke <-factor (mydata$stroke)
sample1=sample(nrow(mydata),5187,replace=F)
mydata=mydata[sample1,]
library(mediation)
medModel<-glm(smoke~mar+age2+gender+edu+bmi+hyp+heart+cancer+stroke,
              family=binomial(link = "probit"),
              weights = weight,control=list(maxit=1500),
              data=mydata)
outModel<-glm(depression~mar*smoke+age2+gender+edu+bmi+hyp+heart+cancer+stroke,
              family=binomial(link = "probit"),
              weights = weight,control=list(maxit=1500),
              data=mydata)

library(mediation)
med<-mediate(model.m =medModel,model.y=outModel,treat = 'mar',
             mediator = 'smoke', weights = weight, data=mydata)
summary(med)

