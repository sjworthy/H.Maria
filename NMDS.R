library(vegan)
# NMDS only seedling plots
setwd("/Users/samanthaworthy/Desktop/Impact.Hurr.Maria/NMDS")
seed.NMDS.cdm=read.csv("seedlings.NMDS.csv", header=T, row.names=1)

# insufficient data
# Bray-Curtis
seed.NMDS=metaMDS(seed.NMDS.cdm, distance="bray", autotransform=FALSE)
seed.spp.fit=envfit(seed.NMDS, seed.NMDS.cdm, permutations=999)
plot(seed.NMDS, type="n")
points(seed.NMDS, display="sites", pch=16)
text(seed.NMDS, labels=c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m",
                         "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), pos=2)
plot(seed.spp.fit, p.max=0.05, cex=0.5)

# insufficient data
# Raup-Crick
seed.NMDS.raup=metaMDS(seed.NMDS.cdm, distance="raup", autotransform=FALSE)
seed.spp.fit.raup=envfit(seed.NMDS.raup, seed.NMDS.cdm, permutations=999)
plot(seed.NMDS.raup, type="n")
points(seed.NMDS.raup, display="sites", pch=16)
text(seed.NMDS.raup, labels=c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m",
                              "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), cex=0.8, pos=1)
plot(seed.spp.fit.raup, p.max=0.05, cex=0.5)

# NMDS seedlings compared to adults without lianas
# Bray-Curtis
seed.adults.cdm=read.csv("abund.NMDS.no.liana.csv", header=T, row.names=1)

seed.adult.NMDS=metaMDS(seed.adults.cdm, distance="bray", autotransform=FALSE, try=100)
seed.adult.spp.fit=envfit(seed.adult.NMDS, seed.adults.cdm, permutations=999)
plot(seed.adult.NMDS, type="n")
points(seed.adult.NMDS, display="sites", pch=16)
text(seed.adult.NMDS, labels=c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m",
                               "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), pos=2)
plot(seed.adult.spp.fit, p.max=0.05, cex=0.5)

# Raup-Crick
seed.adult.NMDS.raup=metaMDS(seed.adults.cdm, distance="raup", autotransform=FALSE, try=100)
seed.adult.spp.fit.raup=envfit(seed.adult.NMDS.raup, seed.adults.cdm, permutations=999)
plot(seed.adult.NMDS.raup, type="n")
points(seed.adult.NMDS.raup, display="sites", pch=16)
text(seed.adult.NMDS.raup, labels=c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m",
                                    "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), cex=0.8, pos=1)

