setwd("C:/Users/Mahla/Documents/R/R-2.15.2")
GZLM<-read.table("Dataset_GZLM_SeedGermination.txt",header=T,sep="\t",quote="")
names(GZLM)
attach(GZLM)
levels(storage)

g1<-log(GZLM$g+0.5)
model1<-glm(g~storage*temp*treat*var,family=poisson,data=GZLM)
summary(model1)
anova(model1,test="Chi")


#Control vs. Hydro
contrasts(GZLM$treat)<-c(1,-1,0,0,0)
contrasts(GZLM$treat)
model_all<-glm(g1~treat)
summary(model_all)


	#100 vs Control
contrasts(GZLM$treat)<-c(1,0,0,-1,0)
contrasts(GZLM$treat)
model_all<-glm(g1~treat)
summary(model_all)

	#100 vs hydro
contrasts(GZLM$treat)<-c(0,1,0,-1,0)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)


	#100 vs 150
contrasts(GZLM$treat)<-c(0,0,0,-1,1)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)


	#100 vs 50
contrasts(GZLM$treat)<-c(0,0,1,-1,0)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)


	#control vs 50
contrasts(GZLM$treat)<-c(-1,0,1,0,0)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)


	#150 vs 50
contrasts(GZLM$treat)<-c(0,0,1,0,-1)
contrasts(GZLM$treat)
model_all<-glm(g11~GZLM$treat)
summary(model_all)

	#150 vs hydro
contrasts(GZLM$treat)<-c(0,1,0,0,-1)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)

	#hydro vs 50
contrasts(GZLM$treat)<-c(0,-1,1,0,0)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)

   #150 vs Control
contrasts(GZLM$treat)<-c(1,0,0,0,-1)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)

   #50,100,150 vs hydro
contrasts(GZLM$treat)<-c(0,3,-1,-1,-1)
contrasts(GZLM$treat)
model_all<-glm(g1~GZLM$treat)
summary(model_all)

names(GZLM)
levels(storage)

   #det_prime vs no_det
contrasts(GZLM$storage)<-c(1,-1,0)
contrasts(GZLM$storage)
model_all<-glm(g1~GZLM$storage)
summary(model_all) 

  #det_prime vs prime_det
contrasts(GZLM$storage)<-c(1,0,-1)
contrasts(GZLM$storage)
model_all<-glm(g1~GZLM$storage)
summary(model_all)

   # prime_det  vs no_det
contrasts(GZLM$storage)<-c(0,1,-1)
contrasts(GZLM$storage)
model_all<-glm(g1~GZLM$storage)
summary(model_all)


levels(var)
     #RGS003" vs "Zarfam"
contrasts(GZLM$var)<-c(1,-1)
contrasts(GZLM$var)
model_all<-glm(g1~GZLM$var)
summary(model_all)

attach(GZLM)
levels(temp)
names(GZLM)
   # 7Cvs 3C
contrasts(GZLM$temp)<-c(0,1,-1)
contrasts(GZLM$temp)
model_all<-glm(g1~GZLM$temp)
summary(model_all)

   # 7C vs 20C
contrasts(GZLM$temp)<-c(1,0,-1)
contrasts(GZLM$temp)
model_all<-glm(g1~GZLM$temp)
summary(model_all)

   # 20Cvs 3C
contrasts(GZLM$temp)<-c(-1,1,0)
contrasts(GZLM$temp)
model_all<-glm(g1~GZLM$temp)
summary(model_all)





Combinedstorage<-as.factor(ifelse(GZLM$storage=="det_prime","detprime",
	    ifelse(GZLM$storage=="no_det","no_det",ifelse(GZLM$storage=="prime_det","detprime",GZLM$storage))))

levels(temp)
Combinedtemp<-as.factor(ifelse(GZLM$temp=="Opt","Opt",ifelse(GZLM$temp=="cold ","cold_cool",ifelse(GZLM$temp=="cool","cold_cool",GZLM$temp))))


levels(var)
Combinedvar<-as.factor(ifelse(GZLM$var=="Zarfam","rgs_Zarfam",
	    ifelse(GZLM$var=="rgs","rgs_Zarfam", GZLM$var)))

levels(treat)
Combinedtreat<-as.factor(ifelse(GZLM$treat=="A","A", ifelse(GZLM$treat=="B","B_C_D_E", ifelse(GZLM$treat=="C","B_C_D_E", ifelse(GZLM$treat=="D","B_C_D_E",   ifelse(GZLM$treat=="E","B_C_D_E", GZLM$treat))))))

model2<-glm(g~Combinedstorage*Combinedtemp*Combinedtreat,family=poisson,data=GZLM)
summary(model2)
anova(model2,test="Chi")
null_model<-glm(g~1) 
AIC(null_model,model1)

model2a<-glm(g~Combinedstorage+Combinedtemp+Combinedtreat+Combinedtreat*Combinedtemp+Combinedstorage*Combinedtreat+Combinedstorage*Combinedtemp,family=poisson,data=GZLM)
summary(model2a)
anova(model2a,test="Chi")
AIC(null_model,model2a)

plot(g~Combinedstorage*Combinedtemp+Combinedstorage*Combinedtreat+Combinedtreat*Combinedtemp)
points((g~Combinedstorage*Combinedtemp+Combinedstorage*Combinedtreat+Combinedtreat*Combinedtemp), fitted(model2))
plot(resid(model2a)~fitted(model2a))
plot(resid(model1)~fitted(model1))
par(mfrow=c(2,2))
plot(model2a, pch=16, lwd=2)
par(mfrow=c(1,1))


levels(Combinedstorage)

library(gplots) 
xx<-tapply(g,list(Combinedstorage,Combinedtemp),mean)
yy<-tapply(g,list(Combinedstorage,Combinedtemp),sd)
zz<-tapply(g,list(Combinedstorage,Combinedtemp),length)
er<-yy/sqrt(zz)
w<-length(levels(Combinedstorage))
barx<-barplot(xx, col=c(1:w), beside=T, ylab="number of germinated seeds", xlab="temperature", 
ylim=c(0, max(xx)+max(yy)),xpd=FALSE)
box(if(detprime=="black"))
arrows(barx,xx+er, barx, xx, angle=90, code=1, length=0.05)
legend(locator(1),c(levels(Combinedtreat)),fill=c(1:w),bty="n",cex=0.8)
}
anova.plot<-function(Combinedstorage, g, treat, ylab="number of germinated seeds", xlab="storage", 
ylim=c(0, max(xx)+max(yy)), length=0.05){


palette(c("grey25","grey50","grey75","white"))

anova.plot(Combinedstorage, g, Combinedtreat)


library(lattice)
bwplot(g ~ Combinedtreat | Combinedstorage  , data = GZLM, 
   strip = strip.custom(bg = 'yellow'), 
   cex = .5, layout = c(3, 1),
   xlab = "treatment", ylab = "number of germinated seeds",
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = 0.5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same"),rot=90))






########################################################################
model1<-glm(g~storage*temp*treat*var,family=poisson,data=GZLM)
summary(model1)
anova(model1,test="Chi")
drop1(model1)

model2<-update(model1,~.-storage:temp:treat:var)
AIC(model1,model2)
drop1(model2)

model3<-update(model2,~.-storage:temp:treat)
AIC(model2,model3)
drop1(model3)


model4<-update(model3,~.-storage:temp:var)
AIC(model3,model4)
drop1(model4)


levels(GZLM$storage)
sums1<-matrix(tapply(GZLM$g,GZLM$storage,mean),nc=3,
dimnames = list(c("r1"),c(levels(GZLM$storage))))


pairwise.difference <- function(m){
   npairs  <- choose( ncol(m), 2 )
   results <- matrix( NA, nc=npairs, nr=nrow(m) )
   cnames  <- rep(NA, npairs)
   if(is.null(colnames(m))) colnames(m) <- paste("col", 1:ncol(m), sep="")
   k <- 1
   for(i in 1:ncol(m)){
    for(j in 1:ncol(m)){
    if(j <= i) next;
    results[ ,k] <- m[ ,i] - m[ ,j]
    cnames[k]    <- paste(colnames(m)[ c(i, j) ], collapse=".vs.")
    k <- k + 1
     }
   }
 colnames(results) <- cnames
 rownames(results) <- rownames(m)
 return(results)
 }

pairwise.difference(sums1)
min(abs(pairwise.difference(sums1)))

Combinedstorage<-as.factor(ifelse(GZLM$storage=="det_prime","detprime",
	    ifelse(GZLM$storage=="no_det","no_det",ifelse(GZLM$storage=="prime_det","detprime",GZLM$storage))))

model_reduced1<-glm(g ~ Combinedstorage + temp + treat + var + Combinedstorage*temp + Combinedstorage*treat + 
    temp*treat + Combinedstorage*var + temp*var + treat*var + Combinedstorage*treat*var + 
    temp*treat*var)
summary(model_reduced1)
anova(model_reduced1,test="Chi")
anova(model1,model_reduced1,test="Chi")
drop1(model_reduced1)


sums2<-matrix(tapply(GZLM$g,GZLM$temp,mean),nc=3,
dimnames = list(c("r1"),c(levels(GZLM$temp))))

pairwise.difference(sums2)
min(abs(pairwise.difference(sums2)))
Combinedtemp<-as.factor(ifelse(GZLM$temp=="Opt","Opt",ifelse(GZLM$temp=="cold ","cold_cool",ifelse(GZLM$temp=="cool","cold_cool",GZLM$temp))))
model_reduced2<-glm(g ~ Combinedstorage + Combinedtemp + treat + var + Combinedstorage*Combinedtemp + Combinedstorage*treat + 
    Combinedtemp*treat + Combinedstorage*var + Combinedtemp*var + treat*var + Combinedstorage*treat*var + 
    Combinedtemp*treat*var)
summary(model_reduced2)
anova(model_reduced2,test="Chi")
anova(model1,model_reduced2,test="Chi")
drop1(model_reduced2)


sums3<-matrix(tapply(GZLM$g,GZLM$treat,mean),nc=5,
dimnames = list(c("r1"),c(levels(GZLM$treat))))
pairwise.difference(sums3)
min(abs(pairwise.difference(sums3)))
Combinedtreat<-as.factor(ifelse(GZLM$treat=="A","A", ifelse(GZLM$treat=="B","B_C", ifelse(GZLM$treat=="C","B_C", ifelse(GZLM$treat=="D","D",   ifelse(GZLM$treat=="E","E", GZLM$treat))))))
model_reduced3<-glm(g ~ Combinedstorage + Combinedtemp + Combinedtreat + var + Combinedstorage*Combinedtemp + Combinedstorage*Combinedtreat + 
    Combinedtemp*Combinedtreat + Combinedstorage*var + Combinedtemp*var + Combinedtreat*var + Combinedstorage*Combinedtreat*var + 
    Combinedtemp*Combinedtreat*var)
summary(model_reduced3)
anova(model_reduced3,model1,test="Chi")

sums4<-matrix(tapply(GZLM$g,Combinedtreat,mean),nc=4,
dimnames = list(c("r1"),c(levels(Combinedtreat))))
pairwise.difference(sums4)
min(abs(pairwise.difference(sums4)))
Combinedtreat2<-as.factor(ifelse(GZLM$treat=="A","A", ifelse(GZLM$treat=="B","B_C_E", ifelse(GZLM$treat=="C","B_C_E", ifelse(GZLM$treat=="D","D",   ifelse(GZLM$treat=="E","B_C_E", GZLM$treat))))))
model_reduced4<-glm(g ~ Combinedstorage + Combinedtemp + Combinedtreat2 + var + Combinedstorage*Combinedtemp + Combinedstorage*Combinedtreat2 + 
    Combinedtemp*Combinedtreat2 + Combinedstorage*var + Combinedtemp*var + Combinedtreat2*var + Combinedstorage*Combinedtreat2*var + 
    Combinedtemp*Combinedtreat2*var)
summary(model_reduced4)
anova(model_reduced3,model_reduced4,test="Chi")
AIC(model_reduced3,model_reduced4)
anova(model_reduced3,test="Chi")


sums5<-matrix(tapply(GZLM$g,Combinedtreat2,mean),nc=3,
dimnames = list(c("r1"),c(levels(Combinedtreat2))))
pairwise.difference(sums5)
min(abs(pairwise.difference(sums5)))
Combinedtreat3<-as.factor(ifelse(GZLM$treat=="A","A", ifelse(GZLM$treat=="B","B_C_D_E", ifelse(GZLM$treat=="C","B_C_D_E", ifelse(GZLM$treat=="D","B_C_D_E",   ifelse(GZLM$treat=="E","B_C_D_E", GZLM$treat))))))
model_reduced5<-glm(g ~ Combinedstorage + Combinedtemp + Combinedtreat3 + var + Combinedstorage*Combinedtemp + Combinedstorage*Combinedtreat3 + 
    Combinedtemp*Combinedtreat3 + Combinedstorage*var + Combinedtemp*var + Combinedtreat3*var + Combinedstorage*Combinedtreat3*var + 
    Combinedtemp*Combinedtreat3*var)
summary(model_reduced5)
anova(model_reduced3,model_reduced4,model_reduced5,test="Chi")
anova(model_reduced5,test="Chi")
AIC(model_reduced3,model_reduced4,model_reduced5)
anova(model_reduced5,test="Chi")
drop1(model_reduced5)

model5<-update(model_reduced5,~.-Combinedtemp:Combinedtreat3:var)
AIC(model_reduced5,model5)
anova(model_reduced5,model5,test="Chi")
drop1(model5)
model6<-update(model5,~.-Combinedstorage:Combinedtreat:var)
AIC(model6,model5)
drop1(model6)
model7<-update(model6,~.-Combinedtemp:Combinedtreat)
AIC(model6,model7)
drop1(model7)
model8<-update(model7,~.-Combinedtreat:var)
AIC(model7,model8)
drop1(model8)
anova(model8,test="Chi")


model9<-glm(g ~ Combinedstorage + Combinedtemp + Combinedtreat + 
    var + Combinedstorage:Combinedtemp + Combinedstorage:Combinedtreat + 
    Combinedstorage:var + Combinedtemp:var,family=poisson,data=GZLM)
summary(model9)
anova(model9,test="Chi")


xv<-seq(0,50,0.001)
yv<-predict(model8,list(g=xv),type="response")
plot(g ~ Combinedstorage + Combinedtemp + Combinedtreat + 
    var + Combinedstorage:Combinedtemp + Combinedstorage:Combinedtreat + 
    Combinedstorage:var + Combinedtemp:var,pch=16,cex=3,col="blue",cex.axis=2,cex.lab=2)
lines(xv,yv,lwd=4,col="brown")
par(mar=c(5,4,2,2),mfrow=c(1,1))


	
par(mfrow=c(1,2))
plot(fitted(model8),resid(model8))
qqnorm(resid(model8))
qqline(resid(model8))
par(mfrow=c(1,1))





