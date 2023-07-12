#MALES



## load libraries
library(phytools)
## Loading required package: ape
## Loading required package: maps
## read tree from file - Tiene que ser Newick
mtDNA.tree<-read.tree("Dloop-COI-Cytb.nwk")
## plot tree
plotTree(mtDNA.tree,type="phylogram",ftype="i")


#Rotate nodes
tree<-rotateNodes(mtDNA.tree,c("23","25","29","30", "31","37"))
plotTree(tree)
nodelabels()

# Import dataframe
RPCS_males<-read.csv("Males.csv",row.names=1)
## change this into a vector
# RPCS_males<-as.matrix(RPCS_males)[,1]

RPCS_males<-as.data.frame(RPCS_males)

x1_males<-setNames(RPCS_males[,1],rownames(RPCS_males))
x2_males<-x1_males[!is.na(RPCS_males)]

## Now we can estimate ancestral states. We will also compute variances & 95% confidence intervals for each node:

fit_males<-anc.ML(tree,x2_males,vars=TRUE,CI=TRUE)
fit_males


## projection of the reconstruction onto the edges of the tree
obj<-contMap(tree,x2_males,plot=TRUE,method="anc.ML")

#invert colors
invert.colors_males<-setMap(obj,invert=TRUE)
plot(invert.colors_males,type="phylogram",legend=0.6*max(nodeHeights(tree)),
     fsize=c(0.7,0.9), sig=0)


#Correct tiplabels to make them coincide with terminals of the tree
tips <- tree$tip.label
tips
RPCS_m2 <- vector("list")
RPCS_m2
for (i in tips) {
  RPCS_m2[i] <- round(x2_males,0)[i]
}
RPCS_m2



# nodelabels with interger values
nodelabels(round(fit_males$ace,0),adj=c(1.3,-1.1),frame="none", cex=0.7)

#Tips
tiplabels(RPCS_m2, adj=c(0.5,2),frame="none", cex=0.55)


###################################################
###################################################
###################################################
#                    Females                     #
###################################################
###################################################
###################################################




## load matrix
RPCS_females<-read.csv("Females.csv",row.names=1)

RPCS_females<-as.data.frame(RPCS_females)

x1_females<-setNames(RPCS_females[,1],rownames(RPCS_females))
x2_females<-x1_females[!is.na(RPCS_females)]

## Now we can estimate ancestral states. We will also compute variances & 95% confidence intervals for each node:

fit_females<-anc.ML(tree,x2_females,vars=TRUE,CI=TRUE)
fit_females


## projection of the reconstruction onto the edges of the tree
obj<-contMap(tree,x2_females,plot=TRUE,method="anc.ML")

#invert colors
invert.colors_females<-setMap(obj,invert=TRUE)
plot(invert.colors_females,type="phylogram",legend=0.6*max(nodeHeights(tree)),
     fsize=c(0.7,0.9), sig=0)


#make tiplabels match with terminals
tips <- tree$tip.label
tips
RPCS_f2 <- vector("list")
RPCS_f2
for (i in tips) {
  RPCS_f2[i] <- round(x2_females,0)[i]
}
RPCS_f2



# nodelabels Sas integers
nodelabels(round(fit_females$ace,0),adj=c(1.3,-1.1),frame="none", cex=0.7)

#Tips
tiplabels(RPCS_f2, adj=c(0.5,2),frame="none", cex=0.55)



###############################
###############################
###############################
#          contMap            #
###############################
###############################
###############################


layout(matrix(1:3,1,3),widths=c(0.42,0.16,0.42))


plot(invert.colors_males,type="phylogram",legend=0.018,
     fsize=c(0.7,1.4), sig=0,ftype="off",main = "")

title(main = "RPCS males",cex.main = 2, font.main= 1,line = -3, adj=0.2)
nodelabels(round(fit_males$ace,0),adj=c(1.3,-1.1),frame="none", cex=0.9)
tiplabels(RPCS_m2, adj=c(-0.4,0.3),frame="none", cex=0.9)

plot.new()
plot.window(xlim=c(-0.1,0.1),ylim=c(1, length(tree$tip.label)))
par(cex=1) 

#text(rep(0,length(tree$tip.label)), 1:length(tree$tip.label),tree$tip.label)
text(rep(0,length(tree$tip.label)), 1:length(tree$tip.label),gsub("_"," ",tree$tip.label))

plot(invert.colors_females,type="phylogram",legend=0.6*max(nodeHeights(tree)),
     fsize=c(0.7,0.9), sig=0, ftype="off",direction="leftwards")
nodelabels(round(fit_females$ace,0),adj=c(-0.3,-0.8),frame="none", cex=0.6,main = "")
tiplabels(RPCS_f2, adj=c(1.5,0.2),frame="none", cex=0.55)
title(main = "RPCS females",cex.main = 1.3, font.main=1 ,line = -1.8, adj=0.9)

