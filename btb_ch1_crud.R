
##' Functions to reproduce analyses, simulations and graphs in Chapter 1 of 'Beyond the Boundary'
##'
##' @title btb_ch1_crud.R; 
##' @author Fred Hasselman (unless otherwise indicated);
##' Copyright (C) 2010-2014 Fred Hasselman;

# BTBfiles SETUP --------------------------------------------------------------
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
init(c("plyr","MASS","ggplot2","grid"))

# Most Scientific Journals want Arial as a font for Figures [function defined in BTBTB.R]
setArial(afmPATH=paste(YOURPATH,"afm",sep="/"))


# ----------------------CHAPTER THE FIRST---------------------------------------
#
# This code will reproduce the analyses/tables/figures presented in Chapter 1 of BEYOND THE BOUNDARY based on simulated data.
# 

# Generate CRUD population and samples ------------------------------------

# Create two population sized correlated random variables
popgen <- function (np,r,sd1,sd2){
  e.cor <- matrix(c(1,r,r,1),2) 
  e.cov <- e.cor*as.matrix(c(sd1,sd2))%*%t(as.matrix(c(sd1,sd2)))
  return(e <- mvrnorm(np,c(0,0),e.cov))
}

set.seed(1111) #set the seed of the random number generator

nmin=10
nmax=100
nt=100
np=10^6

cors  <- c(0.1,0.2,0.3)
sd1   <- 1
# This can be a vector of SD values for the second variable, it doesn't make a lot of difference for the results
sd2   <- 1 #c(1,10,100)
cruds <- vector("list",length(cors)*length(sd2))

# These for.. loops for are used to display more clearly what happens: iterate over correlations and sds
tot = 0
for(c in seq(along=cors)){
  for(s in seq(along=sd2)){
    tot = tot+1
    
    e <- popgen(np,cors[c],sd1,sd2[s])
    
    qDYS    <- quantile(e[,1], probs = c(0.1, 0.25, 0.25, 0.75))
    idDYS10 <- which(e[,1]<=qDYS[1])
    idDYS25 <- which(e[,1]<=qDYS[2])
    idAVE25 <- which((e[,1]>qDYS[3]) & (e[,1]<qDYS[4]))
    DYS10   <- e[idDYS10,2]
    DYS25   <- e[idDYS25,2]
    AVE25   <- e[idAVE25,2]
    
    # Run t-tests on 100 samples for each restricted range group
    # Honk if you love vector programming!    
    data1 <- replicate(nt,mapply(t.test, x=mapply(sample,size=nmin:nmax,MoreArgs=list(x=AVE25)), y=mapply(sample,size=nmin:nmax,MoreArgs=list(x=DYS10)), MoreArgs=list(alternative="greater"))[3,])
    data2 <- replicate(nt,mapply(t.test, x=mapply(sample,size=nmin:nmax,MoreArgs=list(x=AVE25)), y=mapply(sample,size=nmin:nmax,MoreArgs=list(x=DYS25)), MoreArgs=list(alternative="greater"))[3,])
    
    # Count significant tests
    p1 <- (apply(data1<.05,1,sum,na.rm=T))
    p2 <- (apply(data2<.05,1,sum,na.rm=T))
    pr <- c(p1,p2)  
    
    ind <- c(rep(1,(nmax-nmin)+1),rep(2,(nmax-nmin)+1))
    NN  <- c(nmin:nmax,nmin:nmax)
    cor <- rep(cors[c],2*((nmax-nmin)+1))
    dsd <- rep(sd2[s],2*((nmax-nmin)+1))
    
    df     <- as.data.frame(cbind(ind,cor,dsd,NN,pr))
    df$ind <- factor(df$ind,labels=c("Sampled from 10th percentile","Sampled from 25th percentile"))
    
    cruds[[tot]] <- df
    
    rm(e,data1,data2,p1,p2,pr,ind,NN,cor,dsd,df)
  }
}


################
## Figure 1.5 ##
################

# create data.frame
df     <- ldply(cruds,rbind)
df$cor <- factor(df$cor, labels=c("|r| = 0.1","|r| = 0.2","|r| = 0.3"))
df$dsd <- factor(df$dsd)

ggplot(df) +  
  geom_hline(yintercept=0  ,colour="grey70") + 
  geom_hline(yintercept=25 ,colour="grey70") + 
  geom_hline(yintercept=50 ,colour="grey70") + 
  geom_hline(yintercept=75 ,colour="grey70") + 
  geom_hline(yintercept=100,colour="grey70") + 
  geom_point(aes(x=NN,y=pr,colour=dsd,shape=dsd), alpha=.8,show_guide=F) +  
  scale_colour_grey() +
  facet_wrap(ind ~ cor) +  
  xlab("Sample size of independent groups in t-test") + ylab("Number of significant tests") +
  theme_bw(base_size=12, 
           base_family="Arial") + 
  theme(aspect.ratio=1, 
        axis.line = element_line(colour="black"),
        axis.title.x = element_text(vjust=-1.1),
        axis.title.y = element_text(vjust=-0.1),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.background   = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank())


# Some results mentioned in the text --------------------------------------

df$NN[df$pr[df$cor==levels(df$cor)[1] & df$ind==levels(df$ind)[1]]>=25][1]
df$NN[df$pr[df$cor==levels(df$cor)[1] & df$ind==levels(df$ind)[2]]>=25][1]


df$NN[df$pr[df$cor==levels(df$cor)[2] & df$ind==levels(df$ind)[1]]>=25][1]
df$NN[df$pr[df$cor==levels(df$cor)[2] & df$ind==levels(df$ind)[2]]>=25][1]

df$NN[df$pr[df$cor==levels(df$cor)[3] & df$ind==levels(df$ind)[1]]>=25][1]
df$NN[df$pr[df$cor==levels(df$cor)[3] & df$ind==levels(df$ind)[2]]>=25][1]

df$NN[df$pr[df$cor==levels(df$cor)[3] & df$ind==levels(df$ind)[1]]>=90][1]
df$NN[df$pr[df$cor==levels(df$cor)[3] & df$ind==levels(df$ind)[2]]>=90][1]


# Save Figure1.4 ----------------------------------------------------------
w=30
h=20
ggsave(file= "Crud.pdf",width=w,height=h,units="cm",dpi=300)