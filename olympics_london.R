# Import Data
setwd("~/статистийг лахглата/BAYESIAN MODELLING/Olympics - London")
olympics<-read.csv("13_Olympics.csv",header=TRUE,dec = ".")

### GENERALLY ####
par(mfrow=c(3,5))
for (i in 6:18){
  qqnorm(olympics[,i])
  qqline(olympics[,i],col="red")
}

### correlations ###
cor(TotalMedals,BordaPoints)
cor(TotalMedals,Ln.GDP.)
cor(TotalMedals,Ln.Income.)
cor(TotalMedals,Ln.PopnSize.)
cor(TotalMedals,GDP)
cor(TotalMedals,Income)
cor(TotalMedals,PopnSize)

cor(GoldMedals,BordaPoints)
cor(GoldMedals,Ln.GDP.)
cor(GoldMedals,Ln.Income.)
cor(GoldMedals,Ln.PopnSize.)
cor(GoldMedals,GDP)
cor(GoldMedals,Income)
cor(GoldMedals,PopnSize)

cor(Silver,BordaPoints)
cor(Silver,Ln.GDP.)
cor(Silver,Ln.Income.)
cor(Silver,Ln.PopnSize.)
cor(Silver,GDP)
cor(Silver,Income)
cor(Silver,PopnSize)

cor(Bronze,BordaPoints)
cor(Bronze,Ln.GDP.)
cor(Bronze,Ln.Income.)
cor(Bronze,Ln.PopnSize.)
cor(Bronze,GDP)
cor(Bronze,Income)
cor(Bronze,PopnSize)

#########

olympics1<-as.list(na.omit(olympics[,c(2,3,4,5,6,16,17,18)]))
attach(olympics1)
y<-cbind(GoldMedals,Silver,Bronze)
data.names1<-c(names(olympics1),'y')

parameter.names1<-c("a","b","c","d","e","alpha","beta","Alpha","Beta")

library(R2OpenBUGS)
library(coda)

winbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

inits1<-list( list(a=c(0,0,0),b=c(0,0,0),c=c(0,0,0),d=c(0,0,0),e=c(0,0,0),alpha=0 ,beta=c(0,0,0)))

model1.sim <- bugs(data=data.names1, 
                   inits1, 
                   model.file = "C:/Users/VASO TSOULI/Desktop/multilevel .txt", 
                   parameters = parameter.names1,
                   n.chains = 1, 
                   n.iter = 5000, 
                   n.burnin =1000, 
                   n.thin=1,debug=F,OpenBUGS.pgm=winbugs.dir)

print(model1.sim,3) 

#install.packages("BRugs")
library(BRugs)

p0 <- function( bugs.object, digits=3){ 
  mcmc.output<-bugs.object$sims.matrix 
  n.iter <- nrow(mcmc.output)
  n.par  <- ncol(mcmc.output)
  mcmc.output<-mcmc.output[ , -n.par] 
  temp<-apply( mcmc.output < 0, 2, mean)
  res <- pmin( temp, 1-temp)
  return( round(res,digits) )
}
# calculating the probability of zero to be central in the posterior densities
p0(model1.sim)

plot.trace <- function( bugs.object, nrow=5, ncol=NULL, ergodic=FALSE){ 
  mcmc.output<-bugs.object$sims.matrix 
  n.iter <- nrow(mcmc.output)
  n.par  <- ncol(mcmc.output)
  if (is.null(ncol)) ncol <- (n.par %/% nrow)+1*( (n.par %% nrow)!=0 )
  
  par(mfcol=c(nrow,ncol) )
  if (ergodic){ 
    for (k in 1:n.par){ 
      plot( cumsum(mcmc.output[,k])/1:n.iter, type='l', main=colnames(mcmc.output)[k]) }
    
  }else{
    for (k in 1:n.par){ plot( mcmc.output[,k], type='l', main=colnames(mcmc.output)[k]) }
  }
}

# trace plots - in a window with 5 rows and 4 columns
plot.trace( model1.sim,3,2)
# ergodic mean plots - in a window with 5 rows and 4 columns
plot.trace( model1.sim,5,4, ergodic=T)


geweke.plot(model1.sim) #The Geweke diagnostic takes two nonoverlapping parts (usually
#the first 0.1 and last 0.5 proportions) of the Markov chain and
#compares the means of both parts, using a difference of means test
#to see if the two parts of the chain are from the same distribution
#(null hypothesis)

#plot(model1.sim, display.parallel = FALSE)

#print(exp(coef)$model1.sim)



##### other models #####

olympics<-read.csv("C:\\Users\\VASO TSOULI\\Desktop\\статистийг лахглата\\BAYESIAN MODELLING\\Final Assignment\\13_Olympics.csv",header=TRUE,dec = ".")


olympics2<-as.list(na.omit(olympics[,c(2,3,4,6,16,17,18)]))
attach(olympics2)
y<-cbind(GoldMedals,Silver,Bronze)
data.names2<-c(names(olympics2),'y')

parameter.names2<-c("a","b","c","d","e","A","B")


inits2<-list( list(a=c(0,0,0),b=c(0,0,0),c=c(0,0,0),d=c(0,0,0),e=c(0,0,0)))

model2.sim <- bugs(data=data.names2, 
                   inits2, 
                   model.file = "C:/Users/VASO TSOULI/Desktop/poissonmodel1.txt", 
                   parameters = parameter.names2,
                   n.chains = 1, 
                   n.iter = 5000, 
                   n.burnin =1000, 
                   n.thin=1,debug=F,OpenBUGS.pgm=winbugs.dir)

print(model2.sim,3) 



#### model without income

olympics3<-as.list(na.omit(olympics[,c(2,3,4,5,6,17,18)]))
attach(olympics3)
y<-cbind(GoldMedals,Silver,Bronze)
data.names3<-c(names(olympics3),'y')

parameter.names3<-c("a","b","d","e","alpha","beta","Alpha","Beta")

inits3<-list( list(a=c(0,0,0),b=c(0,0,0),d=c(0,0,0),e=c(0,0,0),alpha=0 ,beta=c(0,0,0)))

model3.sim <- bugs(data=data.names3, 
                   inits3, 
                   model.file = "C:/Users/VASO TSOULI/Desktop/multilevel(-Ln.Income.).txt", 
                   parameters = parameter.names3,
                   n.chains = 1, 
                   n.iter = 5000, 
                   n.burnin =1000, 
                   n.thin=1,debug=F,OpenBUGS.pgm=winbugs.dir)

print(model3.sim,3) 

p0(model3.sim)

####
par(mfrow=c(2,2))
geweke.plot(model3.sim)
traceplot(model3.sim)
######
olympics2<-subset(na.omit(olympics[,c(2,3,4,5,6,16,17,18)]))

library(BAS)
summary(bas.glm(TotalMedals~.,data = olympics2,family = poisson()))

######
olympics4<-subset(na.omit(olympics[,c(2,3,4,5,6,7,8,9)]))

summary(bas.lm(TotalMedals~.,data = olympics4))
####

olympics5<-as.list(na.omit(olympics[,c(5,6,7,8,9)]))
attach(olympics5)
data.names5<-c(names(olympics5))

parameter.names5<-c("alpha","beta")

library(R2OpenBUGS)
library(coda)

winbugs.dir <- "C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

inits5<-list( list(alpha=0 ,beta=c(0,0,0,0),tau=1))

model5.sim <- bugs(data=data.names5, 
                   inits5, 
                   model.file = "C:/Users/VASO TSOULI/Desktop/Normalmodel.txt", 
                   parameters = parameter.names5,
                   n.chains = 1, 
                   n.iter = 5000, 
                   n.burnin =1000, 
                   n.thin=1,debug=T,OpenBUGS.pgm=winbugs.dir)

print(model5.sim,3) 


