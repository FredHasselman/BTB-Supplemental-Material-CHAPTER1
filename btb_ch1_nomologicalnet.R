
##' Functions to reproduce analyses, simulations and graphs in Chapter 1 of 'Beyond the Boundary'
##'
##' @title btb_ch1_nomologicalnet.R; 
##' @author Fred Hasselman (unless otherwise indicated);
##' Copyright (C) 2010-2014 Fred Hasselman;

# BTBfiles SETUP ---------------------------------------------------------------------------------------------------------------------------
#
# First the script will download the latest Beyond The Boundary Toolbox, 'BTBTB.R'
# It is stored in a GitHub repository from which we will source it using the 
# extremely useful function source_https() I found on Tony Breyal's blog:
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/

source_https <- function(url, ...) {
  # load the package 
  require(RCurl)

  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# Source the BTBTB.R toolbox!
source_https("https://raw2.github.com/FredHasselman/toolboxR/master/BTBTB.R")

# Set the working directory to "BTBfiles" by calling the `pathfinder` fuction [function defined in BTBTB.R]
# If you know the extracted folder is beyond the scope of the current `getwd()` path, be sure to manually assign 
# YOURPATH <- "/your/path/to/BTBfiles"
YOURPATH <- pathfinder(folder="^(BTBfiles)$")
setwd(YOURPATH)

# Load required libraries for Chapter 1
# This installs a package if it is not present on your system [function defined in BTBTB.R]
init(c("igraph","tm","ggplot2","grid"))

# Most Scientific Journals want Arial as a font for Figures [function defined in BTBTB.R]
setArial(afmPATH=paste(YOURPATH,"afm",sep="/"))


# ---------------------------------------------------------CHAPTER THE FIRST----------------------------------------------------------------
#
# This code will reproduce the analyses/tables/figures presented in Chapter 1 of BEYOND THE BOUNDARY based on data files contained in
# in the archive. 
# 
# In order to run the code that created the corpus on which the graph analyses are based uncomment and execute the lines after # CREATE CORPUS.
# Note that this step is not required to run the code in this file; the datafile with the filtered corpus is provided in the archive.


# CREATE CORPUS -----------------------------------------------------------------------------------------------------------------------
#
# In order to run the code that created the corpus on which the graph analyses are based uncomment and execute the lines below.
# Note that this step is not required to run the code in this file; the datafile with the filtered corpus is provided in the archive.
#
#  source("filtR.CORPUS.R") 
#  filtR.CORPUS() # [function defined in filtR.CORPUS.R]


# A CRUDE SKETCH ---------------------------------------------------------------------------------------------------------------------------

# Load the "raw" article abstract data (df) and the VCorpus in which many different terms have been replaced by a topic keyword (see Appendix A.1)
load("filtRDcorpus.Rdata")

ind1970 <- grep("197.",df$DatePub)
ind1980 <- grep("198.",df$DatePub)
ind1990 <- grep("199.",df$DatePub)
ind2000 <- grep("200.",df$DatePub)
ind2010 <- grep("201.",df$DatePub)

################
## Figure 1.2 ##
################

# This creates a graph object and an .svg file for each decade in the search query [function defined in BTBTB.R]
# Final fgure was created in Adobe Illustrator
g1970 <- graph2svg(TDM <- TermDocumentMatrix(TM[ind1970]),"ind1970.svg")
g1980 <- graph2svg(TDM <- TermDocumentMatrix(TM[ind1980]),"ind1980.svg")
g1990 <- graph2svg(TDM <- TermDocumentMatrix(TM[ind1990]),"ind1990.svg")
g2000 <- graph2svg(TDM <- TermDocumentMatrix(TM[ind2000]),"ind2000.svg")

# It is is similar to this plot:
par(mfrow=c(2,2))
plot(g1970,layout=layout.sphere, main="1970-1980")
plot(g1980,layout=layout.sphere, main="1980-1990")
plot(g1990,layout=layout.sphere, main="1990-2000")
plot(g2000,layout=layout.sphere, main="2000-2010")
par(mfrow=c(1,1))

################
## Figure 1.3 ##
################

# Calculate the degree for nodes in each decade.
inv <- unique(c(names(degree(g1970)),names(degree(g1980)),names(degree(g1990)),names(degree(g2000))))

# Assign variable in a datastructure for plottin
gdeg       <- data.frame(matrix(nrow=4*length(inv),ncol=5,dimnames=list(NULL,c("epoch","term","deg","bet","cat"))))
gdeg$epoch <- c(rep("1970-1980",length(inv)),rep("1980-1990",length(inv)),rep("1990-2000",length(inv)),rep("2000-2010",length(inv)))
gdeg$term  <- c(rep(sort(inv),4))
gdeg$cat   <- (1:4*length(inv))*0
gdeg$cat[grep("cns",gdeg$term)] <- "Brain"
gdeg$cat[grep("bio",gdeg$term)] <- "Biology"
gdeg$cat[grep("com",gdeg$term)] <- "Comorbid"
gdeg$cat[grep("theo",gdeg$term)]<- "Theory"
gdeg$cat[gdeg$cat==0] <- "Other"

# Degree associated with nodes
idx <- which( gdeg$term[gdeg$epoch=="1970-1980"] %in% names(degree(g1970,loops=F,normalized=T)), arr.ind=T )
gdeg$deg[idx] <- as.vector(degree(g1970,loops=F,normalized=T))

idx <- which( gdeg$term[gdeg$epoch=="1980-1990"] %in% names(degree(g1980,loops=F,normalized=T)), arr.ind=T )
gdeg$deg[length(inv)+idx] <- as.vector(degree(g1980,loops=F,normalized=T))

idx <- which( gdeg$term[gdeg$epoch=="1990-2000"] %in% names(degree(g1990,loops=F,normalized=T)), arr.ind=T )
gdeg$deg[2*length(inv)+idx] <- as.vector(degree(g1990,loops=F,normalized=T))

idx <- which( gdeg$term[gdeg$epoch=="2000-2010"] %in% names(degree(g2000,loops=F,normalized=T)), arr.ind=T )
gdeg$deg[3*length(inv)+idx] <- as.vector(degree(g2000,loops=F,normalized=T))

# Multipanel barplot, bar indicating degree (normalised)
  ggplot(gdeg, aes(x=term,y=deg,fill=cat)) + 
  geom_bar(stat="identity",colour="grey30") +
  geom_hline(yintercept = 0.5, colour="grey60") +
  facet_wrap(~ epoch) + scale_fill_grey(name="Type",start=0,end=1) +
  ylab("Normalised degree of term in graph") + xlab("Term associated with ætiology of developmental dyslexia") +
  theme_bw(base_size=12, base_family="Arial") +
  theme(plot.margin  = unit(c(1,1,1,1), "lines"),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        axis.line    = element_line(colour="black"),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(vjust=-0.05),
        strip.text.x = element_text(family="Arial",face="bold",size=18),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

w=50
h=20
ggsave(file="Figure1.3.pdf",width=w,height=h,units="cm",dpi=300)


# THEO CORPUS -----------------------------------------------------------------------------------------------------------------------
#
# In order to run the code that created the corpus on which the graph analyses are based uncomment and execute the lines below.
# Note that this step is not required to run the code in this file; the datafile with the filtered corpus is provided in the archive.
#
#  source("filtR.theoCORPUS.R") 
#  filtR.theoCORPUS() # [function defined in filtR.theoCORPUS.R]

load("filtRDcorpusTHEO.Rdata")


################
## Figure 1.3 ##
################

ind1970 <- grep("197.",dfTheo$DatePub)
ind1980 <- grep("198.",dfTheo$DatePub)
ind1990 <- grep("199.",dfTheo$DatePub)
ind2000 <- grep("200.",dfTheo$DatePub)
ind2010 <- grep("201.",dfTheo$DatePub)

# This creates a graph object and an .svg file for each decade in the search query [function defined in BTBTB.R]
# Final figure was created in Adobe Illustrator
ng1970 <- hoodGraph2svg(TDM <- TermDocumentMatrix(TMtheo[ind1970]),"~theo~","NGind1970.svg")
ng1980 <- hoodGraph2svg(TDM <- TermDocumentMatrix(TMtheo[ind1980]),"~theo~","NGind1980.svg")
ng1990 <- hoodGraph2svg(TDM <- TermDocumentMatrix(TMtheo[ind1990]),"~theo~","NGind1990.svg")
ng2000 <- hoodGraph2svg(TDM <- TermDocumentMatrix(TMtheo[ind2000]),"~theo~","NGind2000.svg")

# It is is similar to this plot:
par(mfrow=c(2,2))
plot(ng1970, layout=layout.star(ng1970, center=V(ng1970)[which(V(ng1970)$name=="~theo~")]), main="1970-1980")
plot(ng1980, layout=layout.star(ng1980, center=V(ng1980)[which(V(ng1980)$name=="~theo~")]), main="1980-1990")
plot(ng1990, layout=layout.star(ng1990, center=V(ng1990)[which(V(ng1990)$name=="~theo~")]), main="1990-2000")
plot(ng2000, layout=layout.star(ng2000, center=V(ng2000)[which(V(ng2000)$name=="~theo~")]), main="2000-2010")
par(mfrow=c(1,1))

# Here are the cocitation and other stats reported in the paragraph

idx  <- which(V(ng1970)$name=="~theo~")
ci1  <-(cocitation(ng1970,V(ng1970)[idx]))
ci1a <-graph.strength(ng1970,V(ng1970)[idx])
ci1M <-mean(ci1[setdiff(1:length(ci1),idx)])
ci1SD<-sd(ci1[setdiff(1:length(ci1),idx)])

idx  <- which(V(ng1980)$name=="~theo~")
ci2  <-(cocitation(ng1980,V(ng1980)[idx]))
ci2M <-mean(ci2[setdiff(1:length(ci2),idx)])
ci2SD<-sd(ci2[setdiff(1:length(ci2),idx)])
ci2a <-graph.strength(ng1980,V(ng1980)[idx])

idx  <- which(V(ng1990)$name=="~theo~")
ci3  <-(cocitation(ng1990,V(ng1990)[idx]))
ci3M <-mean(ci3[setdiff(1:length(ci3),idx)])
ci3SD<-sd(ci3[setdiff(1:length(ci3),idx)])
ci3a <-graph.strength(ng1990,V(ng1990)[idx])

idx  <- which(V(ng2000)$name=="~theo~")
ci4  <-cocitation(ng2000,V(ng2000)[idx])
ci4M <-mean(ci4[setdiff(1:length(ci4),idx)])
ci4SD<-sd(ci4[setdiff(1:length(ci4),idx)])
ci4a <-graph.strength(ng2000,V(ng2000)[idx])

cat("\n\nMean Cocitation (SD) and Graph Strength for the 1st order neighbourhood of vertex ~theo~\n\n 1970-1980:", ci1M,"(",ci1SD,") -- ",ci1a,"\n 1980-1990:", ci2M,"(",ci2SD,") -- ",ci2a,"\n 1990-2000:", ci3M,"(",ci3SD,") -- ",ci3a,"\n 2000-2010:", ci4M,"(",ci4SD,") -- ",ci4a,"\n")
