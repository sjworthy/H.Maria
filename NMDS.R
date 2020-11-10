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
                               "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), pos=2, cex=0.7)
plot(seed.adult.spp.fit, p.max=0.05, cex=0.8)

# new graphing code

site.scrs <- as.data.frame(scores(seed.adult.NMDS, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Sites = c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m","Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"))
head(site.scrs)


spp.scrs <- as.data.frame(scores(seed.adult.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = seed.adult.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)
head(sig.spp.scrs)

nmds.plot.dune <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2))+ ggrepel::geom_text_repel(aes(label=Sites),angle=45)+
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))
 
BC.plot=nmds.plot.dune+
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


# Raup-Crick
seed.adult.NMDS.raup=metaMDS(seed.adults.cdm, distance="raup", autotransform=FALSE, try=100)
seed.adult.spp.fit.raup=envfit(seed.adult.NMDS.raup, seed.adults.cdm, permutations=999)
plot(seed.adult.NMDS.raup, type="n")
points(seed.adult.NMDS.raup, display="sites", pch=16)
text(seed.adult.NMDS.raup, labels=c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m",
                                    "Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"), cex=0.8, pos=1)
# New plot
RC.site.scrs <- as.data.frame(scores(seed.adult.NMDS.raup, display = "sites")) #save NMDS results into dataframe
RC.site.scrs <- cbind(RC.site.scrs, Sites = c("Pre-300m", "Post-300m", "Pre-400m", "Post-400m", "Pre-500m","Post-500m", "Adults-300m", "Adults-400m", "Adults-500m"))
head(site.scrs)


spp.scrs <- as.data.frame(scores(seed.adult.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = seed.adult.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)
head(sig.spp.scrs)

RC.nmds.plot.dune <- ggplot(RC.site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2))+ ggrepel::geom_text_repel(aes(label=Sites))+
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))


