## Preamble
#Paul Braverman
#University of Chicago
#Applied multivariate Analysis

###h=0###
rm(list = ls())
cat("\014")
getwd()
setwd("C:/Users/pdbra/Dropbox/University of Chicago/Class/Advanced Econometrics/hw2-master/hw2-master")


states = read.csv('statelevel.csv')
national = read.csv('national.csv')
library(tidyverse)
library(lubridate)
library(glmnet)
library(dplyr)
gr <- function(x, horizon=3) x/lag(x, horizon)
national = national %>%
  mutate(termspread = GS10-TB3MS, DATE = ymd(DATE),
         SP500 = gr(SP500), PAYEMS = lag(gr(PAYEMS)),
         INDPRO = lag(gr(INDPRO)), GS10=NULL,
         TB3MS = NULL, RECPROUSM156N = NULL)
gri <- function(x, horizon=2) x*lag(x, horizon)
states = states %>% mutate(DATE = mdy(X), X=NULL) %>%
  mutate_at(vars(Alabama:Wyoming), funs(lag(gri(.))))
alldat = full_join(national,states,by='DATE') %>% na.omit()

attach(alldat)

#0# Baseline
plot(DATE,USREC,type = "s",ylab="USREC",main="USREC Baseline")
dev.copy(png,'myplot_0_h0.png')
dev.off()
#1# Logistic regression
fit <- glm(as.factor(USREC)~.-DATE,family = "binomial",data=alldat)
summary(fit) # display results
plot(DATE,predict(fit,type ="response"),type = "s",ylab="USREC",main="USREC Logistic regression h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_1_h0.png')
dev.off()

#2# Lasso-logistic regression
X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(alldat, select = -c(USREC,DATE) )), y=as.factor(USREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(DATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_2_h0.png')
dev.off()

#3# Elnet-logistic regression (alpha = 0.5)
X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(alldat, select = -c(USREC,DATE) )), y=as.factor(USREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(DATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_3_h0.png')
dev.off()

#4# Lasso-logistic regression + squared predictors
X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(USREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(DATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_4_h0.png')
dev.off()

#5# Elnet-logistic regression (alpha = 0.5) + squared predictors 
X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(USREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(DATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_5_h0.png')
dev.off()

#6# Lasso-logistic regression + squared predictors + cubic

X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(USREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(DATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors + cubic h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_6_h0.png')
dev.off()

#7# Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic
X=as.matrix(subset(alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(USREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(DATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic h=0")
lines(DATE,USREC,type = "s",col="red")
dev.copy(png,'myplot_7_h0.png')
dev.off()


###h=1###
rm(list = ls())
cat("\014")
getwd()
setwd("C:/Users/pdbra/Dropbox/University of Chicago/Class/Advanced Econometrics/hw2-master/hw2-master")


states = read.csv('statelevel.csv')
national = read.csv('national.csv')
library(tidyverse)
library(lubridate)
library(glmnet)
library(dplyr)
gr <- function(x, horizon=3) x/lag(x, horizon)
national = national %>%
  mutate(termspread = GS10-TB3MS, DATE = ymd(DATE),
         SP500 = gr(SP500), PAYEMS = lag(gr(PAYEMS)),
         INDPRO = lag(gr(INDPRO)), GS10=NULL,
         TB3MS = NULL, RECPROUSM156N = NULL)
gri <- function(x, horizon=2) x*lag(x, horizon)
states = states %>% mutate(DATE = mdy(X), X=NULL) %>%
  mutate_at(vars(Alabama:Wyoming), funs(lag(gri(.))))
alldat = full_join(national,states,by='DATE') %>% na.omit()

attach(alldat)
leadUSREC<-USREC[-1]
leadDATE<-DATE[-1]
cut_alldat=alldat[-614,]
detach(alldat)
rm(alldat)
attach(cut_alldat)

#0# Baseline
plot(leadDATE,leadUSREC,type = "s",ylab="USREC",main="USREC Baseline")
dev.copy(png,'myplot_0_h1.png')
dev.off()

#1# Logistic regression
fit <- glm(as.factor(leadUSREC)~.-DATE-USREC,family = "binomial",data=cut_alldat)
summary(fit) # display results
plot(leadDATE,predict(fit,type ="response"),type = "s",ylab="USREC",main="USREC Logistic regression h=1")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_1_h1.png')
dev.off()

#2# Lasso-logistic regression
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cutalldat, select = -c(USREC,DATE) )), y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression h=1")
lines(leadDATE,lagUSREC,type = "s",col="red")
dev.copy(png,'myplot_2_h1.png')
dev.off()

#3# Elnet-logistic regression (alpha = 0.5)
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cutalldat, select = -c(USREC,DATE) )), y=as.factor(USREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) h=1")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_3_h1.png')
dev.off()

#4# Lasso-logistic regression + squared predictors
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors h=1")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_4_h1.png')
dev.off()

#5# Elnet-logistic regression (alpha = 0.5) + squared predictors 
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors h=1")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_5_h1.png')
dev.off()

#6# Lasso-logistic regression + squared predictors + cubic

X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(USREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors + cubic h=1")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_6_h1.png')
dev.off()

#7# Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic h=0")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_7_h0.png')
dev.off()



###h=2###
rm(list = ls())
cat("\014")
getwd()
setwd("C:/Users/pdbra/Dropbox/University of Chicago/Class/Advanced Econometrics/hw2-master/hw2-master")


states = read.csv('statelevel.csv')
national = read.csv('national.csv')
library(tidyverse)
library(lubridate)
library(glmnet)
library(dplyr)
gr <- function(x, horizon=3) x/lag(x, horizon)
national = national %>%
  mutate(termspread = GS10-TB3MS, DATE = ymd(DATE),
         SP500 = gr(SP500), PAYEMS = lag(gr(PAYEMS)),
         INDPRO = lag(gr(INDPRO)), GS10=NULL,
         TB3MS = NULL, RECPROUSM156N = NULL)
gri <- function(x, horizon=2) x*lag(x, horizon)
states = states %>% mutate(DATE = mdy(X), X=NULL) %>%
  mutate_at(vars(Alabama:Wyoming), funs(lag(gri(.))))
alldat = full_join(national,states,by='DATE') %>% na.omit()


attach(alldat)
leadUSREC<-USREC[-1]
leadUSREC<-leadUSREC[-1]
leadDATE<-DATE[-1]
leadDATE<-leadDATE[-1]
cut_alldat=alldat[c(-614,-613),]
detach(alldat)
rm(alldat)
attach(cut_alldat)
#0# Baseline
plot(DATE,USREC,type = "s",ylab="USREC",main="USREC Baseline")
dev.copy(png,'myplot_0_h2.png')
dev.off()
#1# Logistic regression
fit <- glm(as.factor(leadUSREC)~.-DATE,family = "binomial",data=cut_alldat)
summary(fit) # display results
plot(leadDATE,predict(fit,type ="response"),type = "s",ylab="USREC",main="USREC Logistic regression h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_1_h2.png')
dev.off()

#2# Lasso-logistic regression
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) )), y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_2_h2.png')
dev.off()

#3# Elnet-logistic regression (alpha = 0.5)
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) )), y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_3_h2.png')
dev.off()

#4# Lasso-logistic regression + squared predictors
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_4_h2.png')
dev.off()

#5# Elnet-logistic regression (alpha = 0.5) + squared predictors 
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_5_h2.png')
dev.off()

#6# Lasso-logistic regression + squared predictors + cubic

X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors + cubic h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_6_h2.png')
dev.off()

#7# Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic h=2")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_7_h2.png')
dev.off()

###h=3###
rm(list = ls())
cat("\014")
getwd()
setwd("C:/Users/pdbra/Dropbox/University of Chicago/Class/Advanced Econometrics/hw2-master/hw2-master")


states = read.csv('statelevel.csv')
national = read.csv('national.csv')
library(tidyverse)
library(lubridate)
library(glmnet)
library(dplyr)
gr <- function(x, horizon=3) x/lag(x, horizon)
national = national %>%
  mutate(termspread = GS10-TB3MS, DATE = ymd(DATE),
         SP500 = gr(SP500), PAYEMS = lag(gr(PAYEMS)),
         INDPRO = lag(gr(INDPRO)), GS10=NULL,
         TB3MS = NULL, RECPROUSM156N = NULL)
gri <- function(x, horizon=2) x*lag(x, horizon)
states = states %>% mutate(DATE = mdy(X), X=NULL) %>%
  mutate_at(vars(Alabama:Wyoming), funs(lag(gri(.))))
alldat = full_join(national,states,by='DATE') %>% na.omit()

attach(alldat)
leadUSREC<-USREC[-1]
leadUSREC<-leadUSREC[-1]
leadUSREC<-leadUSREC[-1]
leadDATE<-DATE[-1]
leadDATE<-leadDATE[-1]
leadDATE<-leadDATE[-1]
cut_alldat=alldat[c(-614,-613,-612),]
detach(alldat)
rm(alldat)
attach(cut_alldat)

#0# Baseline
plot(DATE,USREC,type = "s",ylab="USREC",main="USREC Baseline")
dev.copy(png,'myplot_0_h3.png')
dev.off()
#1# Logistic regression
fit <- glm(as.factor(leadUSREC)~.-DATE,family = "binomial",data=cut_alldat)
summary(fit) # display results
plot(leadDATE,predict(fit,type ="response"),type = "s",ylab="USREC",main="USREC Logistic regression h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_1_h3.png')
dev.off()

#2# Lasso-logistic regression
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) )), y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_2_h3.png')
dev.off()

#3# Elnet-logistic regression (alpha = 0.5)
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
cvfit = cv.glmnet(x=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) )), y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = X, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_3_h3.png')
dev.off()

#4# Lasso-logistic regression + squared predictors
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_4_h3.png')
dev.off()

#5# Elnet-logistic regression (alpha = 0.5) + squared predictors 
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
cvfit = cv.glmnet(x=XX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_5_h3.png')
dev.off()

#6# Lasso-logistic regression + squared predictors + cubic

X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(leadUSREC), alpha=1, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Lasso-logistic regression + squared predictors + cubic h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_6_h3.png')
dev.off()

#7# Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic
X=as.matrix(subset(cut_alldat, select = -c(USREC,DATE) ))
X2=X^2
XX=cbind(X,X2)
X3=X2^2
XXX=cbind(XX,X3)
cvfit = cv.glmnet(x=XXX, y=as.factor(leadUSREC), alpha=0.5, family="binomial")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = XXX, s = "lambda.min")
plot(leadDATE,predict(cvfit, newx = XXX, s = "lambda.min", type ="response"),type = "s",ylab="USREC",main="USREC Elnet-logistic regression (alpha = 0.5) + squared predictors +cubic h=3")
lines(leadDATE,leadUSREC,type = "s",col="red")
dev.copy(png,'myplot_7_h3.png')
dev.off()