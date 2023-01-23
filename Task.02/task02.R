setwd('~/Desktop/Evolution/Tasks/Task.02')
Data1<- read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
Data2<- read.csv ('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
write.csv(Data1, 'rawdata.csv', quote=F)
Data1
head(Data1)
GlargleBrgle<- Data1
head(GlargleBrgle)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data2[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Feeds <- which(Data1 [,9]=='bottle')
berenMilk <- Data1[Feeds ,]
head(berenMilk)
Feeds <-which(Data1 [,'event']== 'bottle')
Feeds <-which(Data1$event == 'bottle')
dayID <- apply(Data1, 1, function(x) paste (x[1:3], collapse='-'))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin ="2019-04-18")
beren2 <- Data1
Data1$age <-dateID-dateID[which (Data1$event == 'birth')]
beren2 <- Data1
beren3 <- beren2 [order (beren2$age),]
write.csv(beren3, 'beren_new.csv', quote = F, row.names = FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value [Feeds])
avgFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], mean)
varFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], var)
totalFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], sum)
numFeeds <- tapply (beren3$value [Feeds], beren3$age [Feeds], length)
cor(beren3$value [Feeds], beren3$age[Feeds])
cor.test (beren3$value [Feeds], beren3$age[Feeds])
berenCor<- cor.test (beren3$value [Feeds], beren3$age [Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value [Feeds]~ beren3$caregiver [Feeds])
boxplot( beren3$value [Feeds]~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
CMass <-which(Data1[,9]== 'trait_mass')
berenMass <-Data1[Mass,]
head(berenMass)
Mass <- which(Data1[,'event']== 'trait_mass')
Mass <- which(Data1$event== 'trait_mass')
dayID <-apply(Data1, 1, function(x) paste(x[1:3], collapse = '-'))
dayID <-sapply(dayID, as.Date, format="Y-%m%d", orgin= "2019-04-18")
pdf("r02b-cumulativeMilkByTime.pdf")
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv',stringsAsFactors=F)
write.csv(Data2, 'rawdata.csv', quote=F)
length(Data2)
nrow(Data2)
ncol(Data2)
colnames(Data2)
head(Data2)
Data2[1,]
Data2[2,]
Data2[1:3,4]
Data2[1:5,1:3]
Data2[1:5, 1:3]
CMass <-(Data2[,9]=='trait_mass')
CyrusMilk <-Data2[Mass,]
head(CyrusMilk)
CMass <- which(Data2$event == 'trait_mass')
dayID <- apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data2$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data2)
Cyrus2 <- Data2
Cyrus3 <- Cyrus2[order(Cyrus2$age),]
write.csv(Cyrus3, 'Cyrus_new.csv', quote=F, row.names=FALSE)
CMass<- which(Cyrus3$event == "trait_mass")
avgCMassCyrus <- mean(Cyrus3$value[CMass])
avgCMass <- tapply(Cyrus3$value[CMass], Cyrus3$age[CMass], mean)
varCMass <- tapply(Cyrus3$value[CMass], Cyrus3$age[CMass], var)
totalCMass <- tapply(Cyrus3$value[CMass], Cyrus3$age[CMass], sum)
totalCMass <- tapply(Cyrus3$value[Mass], Cyrus3$age[Mass], sum)
numCMass <- tapply(Cyrus3$value[CMass], Cyrus3$age[CMass], length)
cor(Cyrus3$value [CMass], Cyrus3$age [CMass])
cor.test(Cyrus3$value[CMass], Cyrus3$age[CMass])
cyrusCor <- cor.test(Cyrus3$value[CMass], Cyrus3$age[CMass])
summary(cyrusCor)
cyrusANOVA <- aov(Cyrus3$value[CMass]~ Cyrus3$caregiver[CMass])
boxplot( Cyrus3$value[Mass]~ Cyrus3$caregiver[Mass], xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass , type ="b" , pch=16, xlab=" age in days" , ylab="ounces of milk")
abline(h=mean(totalCMass), lty=2, col='red')   
pdf('r02b-totalMilkByDay.pdf', height = 4, width = 4)
par(las=1, mar=c(5,5,1,1) , mgp=c(2, 0.5, 0), tck=-0.1)
pdf('r02b-totalMilkByDay.pdf', height = 4, width = 4)
par(las=1, mar=c(5,5,1,1) , mgp=c(2, 0.5, 0), tck=-0.1)
plot(as.numeric(names(totalMass)))
abline(h=mean(totalMass), lty=2, col='red')
dev.off()
boxplot(Cyrus3$value[CMass]~ Cyrus3$age[CMass], xlab= "age in days" , ylab = "mass in grams")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalCMass)), totalCMass , type ="b" , pch=16, xlab=" age in days" , ylab="ounces of milk")
boxplot( beren3$value[Feeds]~ beren3$caregiver[Feeds], xlab= "age in days" , ylab = "mass in grams")
source("http://jonsmitchell.com/code/plotFxn02b.R")
#berenMass <-which(beren3$event=='trait_mass')
#cyrusMass <-which(Data2$event=='trait-mass')
#boxplot(beren3$age[berenMass]~ beren3$value[berenMass], xlabs= "age in days", ylabs="mass in kg")

dayID <- apply(Data2,1, function(x) paste(x[1:3],collapse='-'))
dateID <- sapply(dayID, as.Date, format= "%Y-%m-%d", origin= "2019-04-18")
Data2$age <- dateID -dateID[which(Data2$event == 'birth')]
Cyrus2 <- Data2
Cyrus3 <-Cyrus2[order(Cyrus2$age),]

par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0),tck=-0.01)
plot(berenMass$age, berenMass$value, type="b", pch=16, xlabs="age in days", ylabs="mass in kg", ylim=c(4,18))
points(Cyrus3$age, Cyrus3$value / 1000, pch=16, col="blue")


