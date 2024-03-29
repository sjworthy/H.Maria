library(picante)
library(abind)

# Null Model 1 for Figure 2

# CDM has 25 rows, 1 for each 1mx1m plot

plot.300.2017=read.csv("Plot.300.2017.csv", header=T)
abund.cdm.300.25.pre=table(plot.300.2017$Subplot, plot.300.2017$SpCode)

plot.300.2018=read.csv("Plot.300.2018.csv", header=T)
abund.cdm.300.25.post=table(plot.300.2018$St., plot.300.2018$Sp.Code)

plot.400.2017=read.csv("Plot.400.2017.csv", header=T)
abund.cdm.400.25.pre=table(plot.400.2017$Subplot, plot.400.2017$SpCode)

plot.400.2018=read.csv("Plot.400.2018.csv", header=T)
abund.cdm.400.25.post=table(plot.400.2018$St, plot.400.2018$Sp.Code)

plot.500.2017=read.csv("Plot.500.2017.csv", header=T)
abund.cdm.500.25.pre=table(plot.500.2017$Subplot, plot.500.2017$SpCode)

plot.500.2018=read.csv("Plot.500.2018.csv", header=T)
abund.cdm.500.25.post=table(plot.500.2018$St, plot.500.2018$Sp.Code)


# Observed distribution of Bray-Curtis

bray.output.300.pre = vegdist(abund.cdm.300.25.17, method="bray")
bray.output.300.post = vegdist(abund.cdm.300.25.post, method="bray")
bray.output.400.pre = vegdist(abund.cdm.400.25.pre, method="bray")
bray.output.400.post = vegdist(abund.cdm.400.25.post, method="bray")
bray.output.500.pre = vegdist(abund.cdm.500.25.pre, method="bray")
bray.output.500.post = vegdist(abund.cdm.500.25.post, method="bray")


# Null models for Bray-Curtis
# Give the function the CDM

beta.bray <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="bray"))
	}

nulls <- replicate(999, beta.bray(abund.cdm.400.25.pre))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.bray <- as.matrix(vegdist(abund.cdm.400.25.pre, method="bray"))

ses <- (obs.bray - nulls.means) / nulls.sds

obs.nulls = abind(obs.bray,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

# Observed distribution of Raup

raup.output.300.pre = vegdist(abund.cdm.300.25.17, method="raup")
raup.output.300.post = vegdist(abund.cdm.300.25.post, method="raup")
raup.output.400.pre = vegdist(abund.cdm.400.25.pre, method="raup")
raup.output.400.post = vegdist(abund.cdm.400.25.post, method="raup")
raup.output.500.pre = vegdist(abund.cdm.500.25.pre, method="raup")
raup.output.500.post = vegdist(abund.cdm.500.25.post, method="raup")

# Null models for Raup
# Give the function the CDM

beta.raup <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="raup"))
	}

nulls <- replicate(999, beta.raup(abund.cdm.300.25.post))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.raup <- as.matrix(vegdist(abund.cdm.300.25.post, method="raup"))

ses <- (obs.raup - nulls.means) / nulls.sds

obs.nulls = abind(obs.raup,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

# Plot Figure 2

colnames(bray.results.1)=c("300-Pre", "300-Post", "400-Pre", "400-Post", "500-Pre", "500-Post")
ses.bray.results.1=bray.results.1[,7:12]
colnames(ses.bray.results.1)=c("300-Pre", "300-Post", "400-Pre", "400-Post", "500-Pre", "500-Post")
colnames(raup.results.1)=c("300-Pre", "300-Post", "400-Pre", "400-Post", "500-Pre", "500-Post")
ses.raup.results.1=raup.results.1[,7:12]
colnames(ses.raup.results.1)=c("300-Pre", "300-Post", "400-Pre", "400-Post", "500-Pre", "500-Post")

par(mfrow=c(2,2))
boxplot(bray.results.1[,1:6], col=c("white", "gray53"), ylab="Bray-Curtis Dissimilarity", las=2)
boxplot(ses.bray.results.1[,1:6], col=c("white", "gray53"), ylab="SES.Bray-Curtis Dissimilarity", las=2)
boxplot(raup.results.1[,1:6], col=c("white", "gray53"), ylab="Raup-Crick Dissimilarity", las=2)
boxplot(ses.raup.results.1[,1:6], col=c("white", "gray53"), ylab="SES.Raup-Crick Dissimilarity", las=2)


# Non-parametric Wilcoxon Test (Mann-Whitney-Wilcoxon Test)

bray.300m=wilcox.test(bray.results.1[,1], bray.results.1[,2])
# p < 2.2e-16
# W = 23229

bray.400m=wilcox.test(bray.results.1[,3], bray.results.1[,4])
# p = 0.0536
# W = 34476

bray.500m=wilcox.test(bray.results.1[,5], bray.results.1[,6])
# p = 0.00175
# W = 41471

bray.ses.300m=wilcox.test(ses.bray.results.1[,1], ses.bray.results.1[,2])
# p = 0.01312
# W = 39733

bray.ses.400m=wilcox.test(ses.bray.results.1[,3], ses.bray.results.1[,4])
# p = 0.8046
# W = 37624

bray.ses.500m=wilcox.test(ses.bray.results.1[,5], ses.bray.results.1[,6])
# p = 0.8436
# W = 34567

raup.300m=wilcox.test(raup.results.1[,1], raup.results.1[,2])
# p = 0.002172
# W = 38491

raup.400m=wilcox.test(raup.results.1[,3], raup.results.1[,4])
# p = 0.7766

raup.500m=wilcox.test(raup.results.1[,5], raup.results.1[,6])
# p = 1.927e-05
W = 42381

raup.ses.300m=wilcox.test(ses.raup.results.1[,1], ses.raup.results.1[,2])
# p = 0.2398

raup.ses.400m=wilcox.test(ses.raup.results.1[,3], ses.raup.results.1[,4])
# p = 0.9017

raup.ses.500m=wilcox.test(ses.raup.results.1[,5], ses.raup.results.1[,6])
# p = 0.8356

# Figure 3

# Observed Bray-Curtis

abund.cdm.2017=read.csv("abund.cdm.2017.csv", header=T, row.names=1)
abund.cdm.2018=read.csv("abund.cdm.2018.csv", header=T, row.names=1)

bray.output.2017 = vegdist(abund.cdm.2017, method="bray")
bray.output.2018 = vegdist(abund.cdm.2018, method="bray")


# Null models for Bray-Curtis
# Give the function the CDM

beta.bray <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="bray"))
	}

nulls <- replicate(999, beta.bray(abund.cdm.2018))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.bray <- as.matrix(vegdist(abund.cdm.2018, method="bray"))

ses <- (obs.bray - nulls.means) / nulls.sds

obs.nulls = abind(obs.bray,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]


# Observed distribution of Raup

raup.output.2017 = vegdist(abund.cdm.2017, method="raup")
raup.output.2018 = vegdist(abund.cdm.2018, method="raup")

# Null models for Raup
# Give the function the CDM

beta.raup <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="raup"))
	}

nulls <- replicate(999, beta.raup(abund.cdm.2018))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.raup <- as.matrix(vegdist(abund.cdm.2018, method="raup"))

ses <- (obs.raup - nulls.means) / nulls.sds

obs.nulls = abind(obs.raup,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

# Figure 3 Plot

bray.results=read.csv("bray.results.csv",header=T, row.names=1)

plot(bray.results[,1]~row.names(bray.results), pch=19, col="black",ylim=c(0.4,0.8), 
     type="b", cex=2, xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
par(new=TRUE)
plot(bray.results[,2]~row.names(bray.results), pch=19, col="red",ylim=c(0.4,0.8), 
     type="b", cex=2, xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"), cex.axis=1.2)
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1, pch=19)

plot(bray.results[,3]~row.names(bray.results), pch=19, col="black",ylim=c(-1.2,1), 
     type="b",cex=2,xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
par(new=TRUE)
plot(bray.results[,4]~row.names(bray.results), pch=19, col="red",ylim=c(-1.2,1), 
     type="b",cex=2,xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"), cex.axis=1.2)
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"),lty=1, pch=19)

raup.results=read.csv("raup.results.csv", header=T, row.names=1)
plot(raup.results[,1]~row.names(raup.results), pch=19, col="black",ylim=c(0,1), 
     type="b", cex=2, xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
par(new=TRUE)
plot(raup.results[,2]~row.names(raup.results), pch=19, col="red",ylim=c(0,1), 
     type="b",cex=2, xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"), cex.axis=1.2)
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1,pch=19)

plot(raup.results[,3]~row.names(raup.results), pch=19, col="black",ylim=c(-2.5,2.5), 
     type="b", cex=2, xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
par(new=TRUE)
plot(raup.results[,4]~row.names(raup.results), pch=19, col="red",ylim=c(-2.5,2.5),
     type="b", cex=2, xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)",cex.axis=1.2, cex.lab=1.2)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"), cex.axis=1.2)
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1, pch=19)

# Figure 4

# Observed Bray-Curtis

abund.cdm=read.csv("abund.cdm.csv", header=T, row.names=1)

bray.output = vegdist(abund.cdm, method="bray")

# Null models for Bray-Curtis
# Give the function the CDM

beta.bray <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="bray"))
	}

nulls <- replicate(999, beta.bray(abund.cdm))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.bray <- as.matrix(vegdist(abund.cdm, method="bray"))

ses <- (obs.bray - nulls.means) / nulls.sds

obs.nulls = abind(obs.bray,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]


# Observed Raup

raup.output = vegdist(abund.cdm, method="raup")

# Null models for Raup
# Give the function the CDM

beta.raup <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="raup"))
	}

nulls <- replicate(999, beta.raup(abund.cdm))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.raup <- as.matrix(vegdist(abund.cdm, method="raup"))

ses <- (obs.raup - nulls.means) / nulls.sds

obs.nulls = abind(obs.raup,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

# Figure 4 Plot

par(mfrow=c(2,2))
plot(bray.results[,1]~row.names(bray.results), pch=19, cex=2, col="black", ylim=c(0.4,0.8), xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick)
plot(bray.results[,2]~row.names(bray.results), pch=19, cex=2, col="black", ylim=c(-2.1,0.25), xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick)
plot(raup.results[,1]~row.names(raup.results), pch=19, cex=2, col="black", ylim=c(0,0.04), xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick)
plot(raup.results[,2]~row.names(raup.results), pch=19, cex=2, col="black", ylim=c(-0.85,-0.70), xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick)


# Adult beta diversity between elevations
# Figure 5

# Observed Bray-Curtis

adult.cdm=read.csv("adult.cdm.no.liana.csv", header=T, row.names=1)

bray.output.adult = vegdist(adult.cdm, method="bray")

# Null models for Bray-Curtis
# Give the function the CDM

beta.bray <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="bray"))
	}

nulls <- replicate(999, beta.bray(adult.cdm))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.bray <- as.matrix(vegdist(adult.cdm, method="bray"))

ses <- (obs.bray - nulls.means) / nulls.sds

obs.nulls = abind(obs.bray,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]


# Observed distribution of Raup

raup.output.adult = vegdist(adult.cdm, method="raup")


# Null models for Raup
# Give the function the CDM

beta.raup <- function(x){

as.matrix(vegdist(randomizeMatrix(x, null.model = "independentswap"), method="raup"))
	}

nulls <- replicate(999, beta.raup(adult.cdm))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.raup <- as.matrix(vegdist(adult.cdm, method="raup"))

ses <- (obs.raup - nulls.means) / nulls.sds

obs.nulls = abind(obs.raup,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

plot(bray.results[,1]~row.names(bray.results), pch=19, type="b", cex=2, col="black",ylim=c(0.4,0.8), xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"))

plot(bray.results[,2]~row.names(bray.results), pch=19, type="b", cex=2,col="black",ylim=c(-1.2,1), xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "500m-500m"))

plot(raup.results[,1]~row.names(raup.results), pch=19, type="b", cex=2,col="black",ylim=c(0,1), xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"))

plot(raup.results[,2]~row.names(raup.results), pch=19, type="b", cex=2,col="black",ylim=c(-2.5,2.5), xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300m-400m", "400m-500m", "300m-500m"))



# Not in published manuscript

# Given adult community structure pre-hurricane, how similar are seedling communities post-hurricane
# Figure 5 

# Cdm has adults and seedlings pre- and post-hurricane.

# Only seedlings are randomized so just rows 1-6 of cdm

# Observed Bray-Curtis

as.cdm=read.csv("abund.cdm.csv", header=T, row.names=1)
as.no.liana=read.csv("abund.cdm.no.liana.csv", header=T, row.names=1)

bray.output.as = vegdist(as.cdm, method="bray")
bray.output.as.no.liana = vegdist(as.no.liana, method="bray")

# Null models for Bray-Curtis
# Give the function the CDM

beta.bray <- function(x){

rando=randomizeMatrix(x[1:6,], null.model="independentswap")
new.cdm=rbind(rando, x[7:9,])
as.matrix(vegdist(new.cdm, method="bray"))
}

nulls <- replicate(999, beta.bray(as.no.liana))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.bray <- as.matrix(vegdist(as.no.liana, method="bray"))

ses <- (obs.bray - nulls.means) / nulls.sds

obs.nulls = abind(obs.bray,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]


# Observed distribution of Raup

raup.output.as = vegdist(as.cdm, method="raup")
raup.output.as.no.liana = vegdist(as.no.liana, method="raup")

# Null models for Raup
# Give the function the CDM

beta.raup <- function(x){

rando=randomizeMatrix(x[1:6,], null.model="independentswap")
new.cdm=rbind(rando, x[7:9,])
as.matrix(vegdist(new.cdm, method="raup"))
}

nulls <- replicate(999, beta.raup(as.no.liana))

nulls.means <- apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds <- apply(nulls, c(1:2), sd, na.rm = T)

obs.raup <- as.matrix(vegdist(as.no.liana, method="raup"))

ses <- (obs.raup - nulls.means) / nulls.sds

obs.nulls = abind(obs.raup,nulls)

rank.out <- array(dim=dim(obs.nulls), t(apply(apply(obs.nulls,c(1,2),rank),3,t)))

rank.out[,,1]

plot(bray.results.no.liana[,1]~row.names(bray.results.no.liana), pch=19, col="black",ylim=c(0.6,0.9), type="b", cex=2, xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
plot(bray.results.no.liana[,2]~row.names(bray.results.no.liana), pch=19, col="red",ylim=c(0.6,0.9), type="b", cex=2, xaxt="n", ylab="Bray-Curtis Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300", "400", "500"))
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1, pch=19)

plot(bray.results.no.liana[,3]~row.names(bray.results.no.liana), pch=19, col="black",ylim=c(-2.5,0.3), type="b",cex=2,xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
plot(bray.results.no.liana[,4]~row.names(bray.results.no.liana), pch=19, col="red",ylim=c(-2.5,0.3), type="b",cex=2,xaxt="n", ylab="SES.Bray-Curtis Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300", "400", "500"))
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"),lty=1, pch=19)

plot(raup.results.no.liana[,1]~row.names(raup.results.no.liana), pch=19, col="black",ylim=c(0,0.5), type="b", cex=2, xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
plot(raup.results.no.liana[,2]~row.names(raup.results.no.liana), pch=19, col="red",ylim=c(0,0.5), type="b",cex=2, xaxt="n", ylab="Raup-Crick Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300", "400", "500"))
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1,pch=19)

plot(raup.results.no.liana[,3]~row.names(raup.results.no.liana), pch=19, col="black",ylim=c(-3,0.6), type="b", cex=2, xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)")
par(new=TRUE)
plot(raup.results.no.liana[,4]~row.names(raup.results.no.liana), pch=19, col="red",ylim=c(-3,0.6),type="b", cex=2, xaxt="n", ylab="SES.Raup-Crick Dissimilarity", xlab="Elevation (m)")
xtick=seq(300,500, by=100)
axis(side=1, at=xtick, labels=c("300", "400", "500"))
legend("topleft", legend=c("pre H. Maria", "post H. Maria"), col=c("black", "red"), lty=1, pch=19)