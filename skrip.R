setwd("D:\\[46] Workshop\\Linear regression")

ISIT <- read.delim("ISIT.txt", header = TRUE, sep = "\t", dec = ".")
ISIT$fStation<-factor(ISIT$Station)
library(lattice)
xyplot(Sources~SampleDepth|fStation,
       data=ISIT,
       xlab="Sample Depth",ylab="Sources",
       strip = function(bg='white', ...)
         strip.default(bg='white', ...),
       panel = function(x, y) {
         panel.grid(h=-1, v= 2)
         I<-order(x)
         llines(x[I], y[I],col=1)})

op <- par(mfrow=c(2,2),mar=c(5,4,1,2))
Sources16<-ISIT$Sources[ISIT$Station==16]
Depth16<-ISIT$SampleDepth[ISIT$Station==16]
plot(Depth16,Sources16,type="p")

library(gam)
M2<-gam(Sources16~lo(Depth16,span=0.5))
plot(M2,se=T)					#Figure 3.1B

#It may be better to predict along an equidistant gradient
P2 <- predict(M2, se = TRUE)
plot(Depth16, Sources16, type = "p")
I1 <- order(Depth16)
lines(Depth16[I1], P2$fit[I1], lty = 1)
lines(Depth16[I1], P2$fit[I1] + 2 * P2$se[I1], lty = 2)
lines(Depth16[I1], P2$fit[I1] - 2 * P2$se[I1], lty = 2)
par(op)

library(lattice)
Data<-read.table("ISIT.txt",header=T)
Sources<-Data$Sources[Data$Station==16]
Depth<-Data$SampleDepth[Data$Station==16]

plot(Depth,Sources,type="n")
TargetVal<-1500
Bin<-500
B1<-TargetVal-Bin
B2<-TargetVal+Bin

abline(v=B1,lty=2)
abline(v=B2,lty=2)
points(TargetVal,-1,pch=17,cex=2)
BinInterval<-vector(length=length(Depth))
BinInterval[1:length(Depth)]<-1
BinInterval[Depth>=B1 & Depth <=B2]<-16
points(Depth,Sources,pch=BinInterval)

S1<-Sources[BinInterval==16]
D1<-Depth[BinInterval==16]
tmp1<-lm(S1~D1)

DD1<-seq(B1,B2,length=100)
NewData<-data.frame(D1=DD1)
pred1<-predict(tmp1,newdata=NewData)

lines(DD1,pred1,lwd=2)

NewData<-data.frame(D1=1500)
pred1<-predict(tmp1,newdata=NewData)

library(lattice)

Sources<-Data$Sources[Data$Station==16]
Depth<-Data$SampleDepth[Data$Station==16]
I=order(Depth)
Sources1<-Sources[I]
Depth1=Depth[I]

library(gam)


M1=gam(Sources1~lo(Depth1,span=0.1))
M2=gam(Sources1~lo(Depth1,span=0.15))
M3=gam(Sources1~lo(Depth1,span=0.2))
M4=gam(Sources1~lo(Depth1,span=0.25))
M5=gam(Sources1~lo(Depth1,span=0.3))
M6=gam(Sources1~lo(Depth1,span=0.5))
M7=gam(Sources1~lo(Depth1,span=0.7))
M8=gam(Sources1~lo(Depth1,span=0.8))
M9=gam(Sources1~lo(Depth1,span=0.9))
M10=gam(Sources1~lo(Depth1,span=1.0))


Mp1=predict(M1,se=T)
Mp2=predict(M2,se=T)
Mp3=predict(M3,se=T)
Mp4=predict(M4,se=T)
Mp5=predict(M5,se=T)
Mp6=predict(M6,se=T)
Mp7=predict(M7,se=T)
Mp8=predict(M8,se=T)
Mp9=predict(M9,se=T)
Mp10=predict(M10,se=T)

xall=rep(Depth1,10)
yall=rep(Sources1,10)
id=rep(c("span = 0.1","span = 0.15","span = 0.2",
         "span = 0.25","span = 0.3","span = 0.5",
         "span = 0.7","span = 0.8","span = 0.9",
         "span = 1.0"),
       each=length(Depth1))

Pall=c(Mp1$fit,Mp2$fit,Mp3$fit,Mp4$fit,Mp5$fit,
       Mp6$fit,Mp7$fit,Mp8$fit,Mp9$fit,Mp10$fit)
SEall=c(Mp1$se.fit,Mp2$se.fit,Mp3$se.fit,Mp4$se.fit,Mp5$se.fit,
        Mp6$se.fit,Mp7$se.fit,Mp8$se.fit,Mp9$se.fit,Mp10$se.fit)
library(lattice)

xyplot(yall~xall|id,xlab="Depth",ylab="Sources",
       panel=function(x,y,subscripts,...){
         panel.points(x,y,col=1,cex=0.5)
         panel.lines(xall[subscripts],Pall[subscripts],col=1,lwd=1)
         panel.lines(xall[subscripts],Pall[subscripts]+2*SEall[subscripts],col=1,lwd=1,lty=2)
         panel.lines(xall[subscripts],Pall[subscripts]-2*SEall[subscripts],col=1,lwd=1,lty=2)
       }
)

AIC(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10)

M2<-gam(Sources16~lo(Depth16,span=0.5))
E2 <- resid(M2)
F2 <- fitted(M2)
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(x = F2, y=E2, xlab="Fitted values", ylab="Residuals")
plot(x = Depth16, y=E2, xlab="Depth", ylab="Residuals")
hist(E2,main="", xlab="Residuals")

library(mgcv)
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
Sources16 <- ISIT$Sources[ISIT$Station == 16]
Depth16 <- ISIT$SampleDepth[ISIT$Station == 16]

plot(Depth16, Sources16, type = "p")
M3 <- gam(Sources16 ~ s(Depth16, fx = FALSE, k=-1,bs = "cr"))
plot(M3, se = TRUE)
M3pred <- predict(M3, se = TRUE, type = "response")
plot(Depth16, Sources16, type = "p")
I1 <- order(Depth16)
lines(Depth16[I1], M3pred$fit[I1], lty=1)
lines(Depth16[I1], M3pred$fit[I1]+2*M3pred$se[I1],lty=2)
lines(Depth16[I1], M3pred$fit[I1]-2*M3pred$se[I1],lty=2)

E2 <- resid(M3)
F2 <- fitted(M3)
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(x = F2, y=E2, xlab="Fitted values", ylab="Residuals")
plot(x = Depth16, y=E2, xlab="Depth", ylab="Residuals")
hist(E2,main="", xlab="Residuals")

Squid <- read.delim("Squid.txt", header = TRUE)
Squid$fMONTH=factor(Squid$MONTH)
M1 <- lm(Testisweight ~ DML * fMONTH,data=Squid)
Squid$Testisweight

