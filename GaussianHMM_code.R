setwd("C:/Users/ksh/Desktop/data")

#=================================================#
#=================================================#

setwd("C:/Users/ksh/Desktop/data")
data<-read.csv("c_data.csv")
data2<-read.csv("c2.csv")
data2<-na.omit(data2)

library(dplyr)

par(mfrow=c(1,2))
plot(data$y,type='b',ylab='1st')
plot(data2$y,type='b',ylab='2nd')

acf(data$y, main="1st")
acf(data2$y, main="2nd")

hist(data$y,breaks=50,main="1st") 
hist(data2$y,breaks=50,main="2nd") 


data %>% group_by(year) %>% summarise(sum(y))
data2 %>% group_by(year) %>% summarise(sum(y))


data$ynew <- (data$y-mean(data$y))/sd(data$y)
data2$ynew <- (data2$y-mean(data2$y))/sd(data2$y)

plot(data$ynew,type='b',ylab='1st')
plot(data2$ynew,type='b',ylab='2nd')


mean(data$y)
var(data$y)
mean(data2$y)
var(data2$y)

data$y<-data$y*2.4527
data$y<-as.integer(data$y)

dataS<-rbind(data, data2)
dataS

plot(dataS$y,type='b',ylab='1st')
hist(dataS$ynew,breaks=50,main="1st",freq=F) 
lines(density(dataS$ynew))
plot(density(dataS$ynew))


x<-seq(-3,3,length.out=1001)
y1<-dnorm(x,mean=0,sd=1)
plot(density(dataS$ynew))
lines(x,y1,col='red')

write.csv(dataS, file="c_new.csv")

data<-read.csv("c_new.csv")

#=================================================#
#=================================================#
setwd("C:/Users/ksh/Desktop/data")
data<-read.csv("c_new.csv")

y<-data$ynew

library(depmixS4)
library(RcppHMM)
library(hmmr)

plot(y,type='b')

m2<-hmm(y,nstates=2)
summary(m2)
plot(m2@posterior$state)

m3<-hmm(y,nstates=3)
summary(m3)  
plot(m3@posterior$state)

m4<-hmm(y,nstates=4)
summary(m4)
plot(m4@posterior$state)

m5<-hmm(y,nstates=5)
summary(m5)
plot(m5@posterior$state)

m6<-hmm(y,nstates=6)
summary(m6)

m7<-hmm(y,nstates=7)

plot(y);abline(h=0.332,col='red');abline(h=-1.163,col='red')
plot(y);abline(h=-0.014,col='red');abline(h=-1.160,col='red');abline(h=0.812,col='red')
plot(y);abline(h=0.009,col='red');abline(h=-1.172,col='red');abline(h=0.881,col='red');abline(h=-0.886,col='red')
plot(y);abline(h=-0.467,col='red');abline(h=-0.034,col='red');abline(h=-1.1576,col='red');abline(h=0.852,col='red');abline(h=-1.103,col='red')




data$s2<-m2@posterior$state
data$s2value<-ifelse(data$s2==1,-1.163,0.332)
data$s3<-m3@posterior$state
data$s3value<-ifelse(data$s3==1,0.812, 
                     ifelse(data$s3==2,-1.160,-0.014))
data$s4<-m4@posterior$state
data$s4value<-ifelse(data$s4==1,0.009,
                     ifelse(data$s4==2,-0.886,
                            ifelse(data$s4==3,-1.172,0.881)))
data$s5<-m5@posterior$state
data$s5value<-ifelse(data$s5==1,-0.449,
                     ifelse(data$s5==2,0.861,
                            ifelse(data$s5==3,-0.359,
                                   ifelse(data$s5==4,0.091,-1.174))))

plot(y,type='l');points(1:1141,data$s2value,col='red',pch=20)
abline(h=0.332);abline(h=-1.163)

plot(y,type='l');points(1:1141,data$s3value,col='red',pch=20)
abline(h=-0.014);abline(h=-1.160);abline(h=0.812)

plot(y,type='l');points(1:1141,data$s4value,col='red',pch=20)
abline(h=0.009);abline(h=-1.172);abline(h=0.881);abline(h=-0.886)

plot(y,type='l');points(1:1141,data$s5value,col='red',pch=20)
abline(h=-0.449);abline(h=0.861);abline(h=-0.359);abline(h=0.091);abline(h=-1.174)



hmm_bic<-data.frame(state=c(2,3,4,5,6,7),
                    BIC=c(2645.349,2371.787,2325.430,2336.638,
                      2376.316,2506.267))

plot(x=hmm_bic$state, y=hmm_bic$BIC,type='b',lty='dotted',xlab='states',ylab='BIC')

hmm_aic<-data.frame(state=c(2,3,4,5,6,7),
                    AIC=c(2610.072,2301.231,2209.518,2165.289,
                          2139.452,2193.809))

plot(x=hmm_aic$state, y=hmm_aic$AIC,type='b',lty='dotted',xlab='states',ylab='AIC')



library(HiddenMarkov)

Pi<-matrix(c(.8,.1,.1,
             .1,.8,.1,
             .1,.1,.8),byrow=T,nrow=3)

mod<-dthmm(y,Pi=Pi,delta=c(1,0,0),'norm',
           list(mean=c(0,0,0),sd=c(1,1,1)))
fitted.mod.HM<-BaumWelch(mod)
summary(fitted.mod.HM)

res<-residuals(fitted.mod.HM)
plot(res,pch=20)
acf(res)
qqline(res)



Pi<-matrix(c(.7,.1,.1,.1,
             .1,.7,.1,.1,
             .1,.1,.7,.1,
             .1,.1,.1,.7),byrow=T,nrow=4)

mod<-dthmm(y,Pi=Pi,delta=c(1,0,0,0),'norm',
           list(mean=c(0,0,0,0),sd=c(1,1,1,1)))
fitted.mod.HM<-BaumWelch(mod)
summary(fitted.mod.HM)

res<-residuals(fitted.mod.HM)
plot(res,pch=20)
acf(res)
qqnorm(res)
qqline(res)



Pi<-matrix(c(.6,.1,.1,.1,.1,
             .1,.6,.1,.1,.1,
             .1,.1,.6,.1,.1,
             .1,.1,.1,.6,.1,
             .1,.1,.1,.1,.6),byrow=T,nrow=5)

mod<-dthmm(y,Pi=Pi,delta=c(1,0,0,0,0),'norm',
           list(mean=c(0,0,0,0,0),sd=c(1,1,1,1,1)))
fitted.mod.HM<-BaumWelch(mod)
summary(fitted.mod.HM)

res<-residuals(fitted.mod.HM)
plot(res,pch=20)
acf(res)
qqnorm(res)
qqline(res)



#
#
##############ARIMA

tsy<-ts(y,start=c(2001,1),frequency = 54)
decompose.ts<-decompose(tsy)
plot(decompose.ts)

decompose.ts.adj<-tsy-decompose.ts$seasonal
plot(decompose.ts.adj)

tsy.diff_1<-diff(tsy,difference=1)
plot(tsy.diff_1)


tsy.diff_2<-diff(tsy,difference=2)
plot(tsy.diff_2)

library(forecast)
auto.arima(tsy)

tsy.arima<-arima(tsy,order=c(0,1,1))
tsy.forecast<-forecast(tsy.arima, h=30)
plot(tsy.forecast)

tsy.arima<-arima(tsy,order=c(1,0,0))
tsy.forecast<-forecast(tsy.arima, h=30)
plot(tsy.forecast)
###################################################

N<-c('1','2','3')
A<-matrix(c(0.8,0.1,0.1,
            0.1,0.8,0.1,
            0.1,0.1,0.8),
          ncol=length(N),byrow=T)
Mu<-matrix(c(-1,0,1),ncol=length(N))
Sigma<-array(c(1,1,1),dim=c(1,1,length(N)))
Pi<-rep(1/length(N),length(N))

HMM<-verifyModel(list("Model"="GHMM",
                                      "StateNames"=N,
                                      "A"=A,
                                      "Mu"=Mu,
                                      "Sigma"=Sigma,
                                      "Pi"=Pi))

length<-1141
observationSequence<-generateObservations(HMM,length)
hiddenStates<-forwardBackward(HMM,observationSequence$Y)
print(hiddenStates)


n<-3
m<-6
model<-initGHMM(n,m)
print(model)

newModel<-learnEM(newModel,
                  observationSequence,iter=50,delta=1E-5,print=F)



hiddenStates<-viterbi(HMM.cont.univariate,y)

#=================================================#
#=================================================#
#Transforming natural parameters to working
pois.HMM.pn2pw <-function(m,lambda,gamma,delta=NULL,stationary=T){
  tlambda<-log(lambda)
  if(m==1) return(tlambda)
  foo<-log(gamma/diag(gamma))
  tgamma<-as.vector(foo[!diag(m)])
  if(stationary){tdelta<-NULL}
  else{tdelta<-log(delta[-1]/delta[1])}
  parvect<-c(tlambda,tgamma,tdelta)
  return(parvect)
}

#Transforming working parameters to natural
pois.HMM.pw2pn<-function(m,parvect,stationary=T){
  lambda<-exp(parvect[1:m])
  gamma<-diag(m)
  if(m==1) return(list(lambda=lambda,gamma=gamma,delta=1))
  gamma[!gamma]<-exp(parvect[(m+1):(m*m)])
  gamma<-gamma/apply(gamma,1,sum)
  if(stationary){delta<-solve(t(diag(m)-gamma+1),rep(1,m))}
  else{foo<-c(1,exp(parvect[(m*m+1):(m*m+m-1)]))
  delta<-foo/sum(foo)}
  return(list(lambda=lambda, gamma=gamma,delta=delta))
}

#Computing minus the LL from the working parameters
pois.HMM.mllk<-function(parvect,x,m,stationary=T,...){
  if(m==1) return(-sum(dpois(x,exp(parvect),log=T)))
  n<-length(x)
  pn<-pois.HMM.pw2pn(m,parvect,stationary=stationary)
  foo<-pn$delta*dpois(x[1],pn$lambda)
  sumfoo<-sum(foo)
  lscale<-log(sumfoo)
  foo<-foo/sumfoo
  for(i in 2:n){
    if(!is.na(x[i])){P<-dpois(x[i],pn$lambda)}
    else{P<-rep(1,m)}
    foo<-foo%*%pn$gamma*P
    sumfoo<-sum(foo)
    lscale<-lscale+log(sumfoo)
    foo<-foo/sumfoo
  }
  mllk<- -lscale
  return(mllk)
}

#Computing the MLEs, given starting values for the natural parameters
pois.HMM.mle<-function(x,m,lambda0,gamma0,delta0=NULL,stationary=T,...){
  parvect0<-pois.HMM.pn2pw(m,lambda0,gamma0,delta0,stationary = stationary)
  
  mod<-nlm(pois.HMM.mllk,parvect0,x=x,m=m,stationary=stationary)
  
  pn<-pois.HMM.pw2pn(m=m,mod$estimate,stationary=stationary)
  mllk<-mod$minimum
  np<-length(parvect0)
  AIC<-2*(mllk+np)
  n<-sum(!is.na(x))
  BIC<-2*mllk+np*log(n)
  list(m=m,lambda=pn$lambda,gamma=pn$gamma,delta=pn$delta,
       code=mod$code,mllk=mllk,AIC=AIC,BIC=BIC)
}

#Generating a sample
pois.HMM.generate_sample<-function(ns,mod){
  mvect<-1:mod$m
  state<-numeric(ns)
  state[1]<-sample(mvect,1,prob=mod$delta)
  for(i in 2:ns) state[i]<-sample(mvect,1,
                                  prob=mod$gamma[state[i-1],])
  x<-rpois(ns,lambda=mod$lambda[state])
  return(x)
}

#Global decoding by the Viterbi algorithm
pois.HMM.viterbi<-function(x,mod){
  n<-length(x)
  xi<-matrix(0,n,mod$m)
  foo<-mod$delta*dpois(x[1],mod$lambda)
  xi[1,]<-foo/sum(foo)
  for(i in 2:n){
    foo<-apply(xi[i-1,]*mod$gamma,2,max)*dpois(x[i],mod$lambda)
    xi[i,]<-foo/sum(foo)
  }
  iv<-numeric(n)
  iv[n]<-which.max(xi[n,])
  for(i in (n-1):1)
    iv[i]<-which.max(mod$gamma[,iv[i+1]*xi[i,]])
  return(iv)
}

#Computing log(forward probabilities)



#=================================================#
#=================================================#

#Fitting Poisson-HMMs 
y<-data$y
n<-length(y)
#============================ 2 state
m<-2
lambda0<-c(100,200)
gamma0<-matrix(
  c(
    0.9,0.1,
    0.1,0.9
  )
  ,m,m,byrow=T
)
mod2s<-pois.HMM.mle(y,m,lambda0,gamma0,stationary=T)
delta0<-c(1,1)/2
mod2h<-pois.HMM.mle(y,m,lambda0,gamma0,delta=delta0,stationary=F)
mod2s;mod2h
