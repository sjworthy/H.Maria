library(vegan)

setwd("/Users/samanthaworthy/Desktop/Impact.Hurr.Maria")

# Read in Data

p300.17=read.csv("Plot.300.2017.csv", header=T)
p400.17=read.csv("Plot.400.2017.csv", header=T)
p500.17=read.csv("Plot.500.2017.csv", header=T)
p300.18=read.csv("Plot.300.2018.csv", header=T)
p400.18=read.csv("Plot.400.2018.csv", header=T)
p500.18=read.csv("Plot.500.2018.csv", header=T) 
All=read.csv("All.seedlings.csv", header=T)
All.2017=read.csv("All.2017.csv", header=T) # missing PSYBER species
All.2018=read.csv("All.2018.csv", header=T)

# Number of individuals per plot

nrow(p300.17)
nrow(p400.17)
nrow(p500.17)
nrow(p300.18)
nrow(p400.18)
nrow(p500.18)

# Determining the number of families per plot

fam.300.17=table(p300.17$Family)
write.csv(fam.300.17, file="fam.300.17.csv")
unique(p300.17$Family)
fam.300.18=table(p300.18$Family)
write.csv(fam.300.18, file="fam.300.18.csv")
unique(p300.18$Family)
fam.400.17=table(p400.17$Family)
write.csv(fam.400.17, file="fam.400.17.csv")
unique(p400.17$Family)
fam.400.18=table(p400.18$Family)
write.csv(fam.400.18, file="fam.400.18.csv")
unique(p400.18$Family)
fam.500.17=table(p500.17$Family)
write.csv(fam.500.17, file="fam.500.17.csv")
unique(p500.17$Family)
fam.500.18=table(p500.18$Family)
write.csv(fam.500.18, file="fam.500.18.csv")
unique(p500.18$Family)

# Determining the number of genera per plot

gen.300.17=table(p300.17$Genus)
write.csv(gen.300.17, file="gen.300.17.csv")
unique(p300.17$Genus)
gen.300.18=table(p300.18$Genus)
write.csv(gen.300.18, file="gen.300.18.csv")
unique(p300.18$Genus)
gen.400.17=table(p400.17$Genus)
write.csv(gen.400.17, file="gen.400.17.csv")
unique(p400.17$Genus)
gen.400.18=table(p400.18$Genus)
write.csv(gen.400.18, file="gen.400.18.csv")
unique(p400.18$Genus)
gen.500.17=table(p500.17$Genus)
write.csv(gen.500.17, file="gen.500.17.csv")
unique(p500.17$Genus)
gen.500.18=table(p500.18$Genus)
write.csv(gen.500.18, file="gen.500.18.csv")
unique(p500.18$Genus)

# Determining the number of species per plot

sp.300.17=table(p300.17$Taxa)
write.csv(sp.300.17, file="sp.300.17.csv")
unique(p300.17$Taxa)
sp.300.18=table(p300.18$Taxa)
write.csv(sp.300.18, file="sp.300.18.csv")
unique(p300.18$Taxa)
sp.400.17=table(p400.17$Taxa)
write.csv(sp.400.17, file="sp.400.17.csv")
unique(p400.17$Taxa)
sp.400.18=table(p400.18$Taxa)
write.csv(sp.400.18, file="sp.400.18.csv")
unique(p400.18$Taxa)
sp.500.17=table(p500.17$Taxa)
write.csv(sp.500.17, file="sp.500.17.csv")
unique(p500.17$Taxa)
sp.500.18=table(p500.18$Taxa)
write.csv(sp.500.18, file="sp.500.18.csv")
unique(p500.18$Taxa)

# Making community data matrices

abund.cdm = matrix(data=NA, nrow = 6, ncol = 63)
row.names(abund.cdm) = c("p.300.17", "p.300.18", "p.400.17", "p.400.18", "p.500.17", "p.500.18")
colnames(abund.cdm) = as.factor(unique(All$Taxa))

abund.cdm=read.csv("abund.cdm.csv", header=T, row.names=1)

all.abund.cdm=read.csv("abund.cdm.all.csv", header=T, row.names = 1)
# Calculate species richness
richness=specnumber(abund.cdm)
all.richness=specnumber(all.abund.cdm)

# Calculating Fisher's Alpha

alpha=fisher.alpha(abund.cdm)
alpha.all=fisher.alpha(all.abund.cdm)

# Calculate Inverse Simpson's Diversity

invsimp=diversity(abund.cdm, index="invsimpson")
all.invsimp=diversity(all.abund.cdm, index = "invsimpson")

# Evenness, not correlated with shannon

even=invsimp/richness
all.even=all.invsimp/all.richness

#bray curtis (abundance weighted metric()
bray.output = vegdist(abund.cdm, method="bray")

