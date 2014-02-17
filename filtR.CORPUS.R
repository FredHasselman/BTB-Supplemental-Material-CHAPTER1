# GET SEARCH TEXT INTO CORPUS -------------------------------------------------------------------------------------------------------------

filtR.CORPUS <- function(infile="pubmed_result0902.txt",outfile="filtRDcorpus.Rdata"){
  
  cat("\n** BEYOND THE BOUNDARY - CHAPTER THE FIRST **\n\nfiltR.CORPUS():\n  This function filters the text from the titles and abstracts of each article retrieved by the pubmed query ( infile =",infile,") into a VCorpus data structure (see package:tm). The terms used are listed in Appendix A.1 of Chapter 1. A data.frame (df) with raw data and a VCorpus (TM) are saved in outfile = ",outfile,")\nEstablishing the conversion rules is not efficient: Filter rules need to be created quite literal, this requires expert knowledge and introduces a degree of subjectivity... In Chapter 6, filtering is omitted.\n\n")
  
  
  # BTBfiles SETUP ---------------------------------------------------------------------------------------------------------------------------
  #
  # First download the latest Beyond The Boundary Toolbox, 'BTBTB.R'.
  # It is stored in a GitHub repository and we need to download it and source it.
  # Call the useful function source_https() I found on Tony Breyal's blog:
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
  
  # Load some libraries (installs the package if it is not on your system)
  init(c("igraph","tm","RTextTools"))
    
  # READ THE QUERY FIELDS --------------------------------------------------------------------------------------------------------------------
  
  # This was the MedLine search on: 9 Feb 2013
  l=readLines("pubmed_result0902.txt",n=-1)
  
  # How many searches produced an abstract?
  indAB<-grep("AB",l,fixed=TRUE)
  
  PMIDtxt<-vector("list",length(indAB))
  ABtxt  <-vector("list",length(indAB))
  TItxt  <-vector("list",length(indAB))
  DPtxt  <-vector("list",length(indAB))
  SOtxt  <-vector("list",length(indAB))
  DAtxt  <-vector("list",length(indAB))
  LIDtxt <-vector("list",length(indAB))
  AIBtxt <-vector("list",length(indAB))
  
  rm(indAB)
  
  # How many records were retrieved
  indPMID<-grep("PMID",l,fixed=TRUE)
  
  ind=0
  
  for(i in 1:(length(indPMID)-1)){
    
    # This is a search record with an abstract
    if (any(grepl("AB",l[indPMID[i]:indPMID[i+1]],fixed=TRUE))) {
      
      ind    <-ind+1    
      indAB  <-charmatch("AB",l[indPMID[i]:indPMID[i+1]])
      indTI  <-charmatch("TI",l[indPMID[i]:indPMID[i+1]])
      indDP  <-charmatch("DP",l[indPMID[i]:indPMID[i+1]])
      indDA  <-charmatch("DA",l[indPMID[i]:indPMID[i+1]])
      indSO  <-charmatch("SO",l[indPMID[i]:indPMID[i+1]])
      indLID <-charmatch("LID",l[indPMID[i]:indPMID[i+1]])
      indAIB <-charmatch("AIB",l[indPMID[i]:indPMID[i+1]])
      
      # Get ABstract text
      cntAB=0
      repeat{cntAB=cntAB+1
             ifelse(grepl("[A-Z][A-Z]",l[(indPMID[i]+indAB-1)+cntAB]), break, next)}
      ABtxt[ind] <- paste(l[(indPMID[i]+indAB-1):((indPMID[i]+indAB-1)+cntAB-1)],collapse='')
      
      # Get PMID
      PMIDtxt[ind]<-paste(l[indPMID[i]],collapse='')
      
      # Get TItle text (can be multiline)
      TItxt[ind]<- ifelse((length(indTI)>0),
{cntTI=0
 repeat{cntTI=cntTI+1
        ifelse(grepl("[A-Z][A-Z]",l[(indPMID[i]+indTI-1)+cntTI]), break, next)}         
 paste(l[(indPMID[i]+indTI-1):((indPMID[i]+indTI-1)+cntTI-1)],collapse='')},
"NA")

# Get other fields, if present
ifelse(length(indDP) >0,DPtxt[ind] <- paste(l[(indPMID[i]+indDP-1)],collapse=''),DPtxt[ind]   <- "NA")
ifelse(length(indSO) >0,SOtxt[ind] <- paste(l[(indPMID[i]+indSO-1)],collapse=''),SOtxt[ind]   <- "NA")
ifelse(length(indDA) >0,DAtxt[ind] <- paste(l[(indPMID[i]+indDA-1)],collapse=''),DAtxt[ind]   <- "NA")
ifelse(length(indLID)>0,LIDtxt[ind]<- paste(l[(indPMID[i]+indLID-1)],collapse=''),LIDtxt[ind] <- "NA")
ifelse(length(indAIB)>0,AIBtxt[ind]<- paste(l[(indPMID[i]+indAIB-1)],collapse=''),AIBtxt[ind] <- "NA")
    }
  }

rm(cntAB,cntTI,ind,i)
df <- data.frame(unlist(PMIDtxt),unlist(TItxt),unlist(ABtxt),unlist(DPtxt),unlist(DAtxt),unlist(SOtxt),unlist(LIDtxt),stringsAsFactors=FALSE)
names(df)<-list("PMID","TItle","ABstract","DatePub","DatecreAted","SOurce","LID")

oks<-complete.cases(df$TItle,df$ABstract)
df<-df[oks,]
data <-paste(df$TItle,df$ABstract)

r<-Corpus(VectorSource(data))
swamp<-prpIT(r)

rm(ABtxt,DAtxt,TItxt,PMIDtxt,SOtxt,DPtxt,LIDtxt,AIBtxt,indAB,indAIB,indDA,indDP,indLID,indPMID,indSO,indTI,oks)
save(r,df,data,swamp,file="searchdataTXT.Rdata")
rm(data)

# MODIFY SEARCH DATA ---------------------------------------------------------------------------------------------------------------------

TMcorpus <- swamp
stemTDM  <- TermDocumentMatrix(TMcorpus)
stems    <- Terms(stemTDM)

# DEVELOpMENTAL DYSLEXIA (DYS) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dyslexia=c( 
  list(dys_term="dyslexia"),
  list(dys_syns=c(regIT(src=stems,"^dysle"),"word\\sblind")),
  list(dev     =c(regIT(src=stems,"(congenit|constit|^develop(ment)*)"))),
  list(dys     =c("((specif\\s)|(specif\\s^(develop(ment)*)\\s))+(learn\\s|read\\s|spell\\s)+(disabl|disord|deficit|impair)"))
)

# developmental dyslexia/word blind ~> ~dys~
tags1 <-tagIT(gregexpr(wordXsYro(dyslexia[[3]],dyslexia[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,dyslexia[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,dyslexia[[2]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" ~dys~ ",cor=TMcorpus))  

# specific learning disability ~> dys~sld
tags1 <-tagIT(gregexpr(wordXsYro(dyslexia[[4]],dyslexia[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,dyslexia[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,dyslexia[[4]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" dys~sld ",cor=TMcorpus))  

rm(tags1,tags2,tags3,src)

tags1 <-regIT(src=stems,"(congenit|constit|^develop(ment)*)")
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~dev~ ",cor=TMcorpus))  
rm(tags1,src)



# COMORBIDITY (COM) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of named comorbid symptoms / diseases / afflictions
comorbid=c(
  list(com_term =c("comorbid")),
  list(psy    =c(regIT(src=stems,"adhd|^add$|depress|tourett|schiz|borderlin|hyperact|^epil|psychosom|autis|psychosi"),
                 "attent\\sdefici","anxieti\\sdisord","asperg\\ssyndrom","psychopatholog\\sdisord")),
  list(beh    =c(regIT(src=stems,"antisoci|aggress|emot$|^soci(al|o)$|psychosoci$"),"substanc\\sabus")),
  list(cog    =c(regIT(src=stems,"retard|aphas|dysphas|hyperlex|dyscal|anomia"),"intellig\\sdefici")),
  list(mot    =c(regIT(src=stems,"(dys|a)prax|dysgr"))),
  list(spr    =c(regIT(src=stems,"alal|dyslal|anarth"),"verbal\\sdyspraxia")),
  list(spp    =c(regIT(src=stems,"^sli$"),"specific\\slanguag\\simpair")),
  list(lng    =c(regIT(src=stems,"agram|(il|\\b)illiter|dyssynt"))),
  list(viz    =c(regIT(src=stems,"prosopa|blind"),"vision\\simpair")),
  list(aud    =c(regIT(src=stems,"deaf"),"hear\\simpair")),
  list(mem    =c(regIT(src=stems,"amnesia|anomia"))),    
  list(dev    =c("pervas\\sdevelopment")),
  list(imp    =c(regIT(src=stems,"syndrom|difficulti|disord|impair")))
)

# COMORBID SYNDROM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# comorbid syndroms ~> com~xxx
for(i in 2:(length(comorbid)-1)){
  tags1<-tagIT(gregexpr(wordXsYro(comorbid[[i]],comorbid$imp),TMcorpus,perl=TRUE),TMcorpus)
  tags2<-tagIT(gregexpr(wordXsYr(tags1,comorbid[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  tags3<-tagIT(gregexpr(wordXsYro(tags2,comorbid[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  TMcorpus<-subIT(src<-list(tags=tags3,str=paste(c(" com~",names(comorbid[i])," "),collapse=""),cor=TMcorpus))  
  rm(tags1,tags2,tags3)
}

# Remaining comorbid ~> ~com~
tags1<-tagIT(gregexpr(wordXX(comorbid[[1]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~com~ ",cor=TMcorpus))  
rm(tags1)


# TREATMENT (TREAT) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Treatment
treat=c(
  list(treat_term    =c("treatment")),
  list(treat_syns    =c(regIT(src=stems,"treat|interve|remedi|amelior|^train|psychotherapi|^therapi$|program|rehabilit"),"treat\\splan","train\\sprogram\\spractic","intervent\\sprogram", "treatment\\sprogram ","efficaci\\sprogram","train\\sprogram" ,"remedi\\sprogram\\srehabilit"))
  #list(type          =c(regIT(src=stems,"neuropsychogol$|psychiatri$|^behav|^medici|diet|psycholing")))
)

tags1<-tagIT(gregexpr(wordXsYro(treat[[2]],treat[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2<-tagIT(gregexpr(wordXsYr(tags1,treat[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3<-tagIT(gregexpr(wordXsYro(tags2,treat[[2]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags3,str=" ~trt~ ",cor=TMcorpus))
rm(tags1,tags2,tags3,src)


# THEORY (THEO) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

theories=c(
  list(theo_term =c("theory")),
  list(theo_syns =c(regIT(src=stems,"theor|model|mechan|hypothesi|framework|(sub|\\b)system$|proces"))),
  list(eti       =c(regIT(src=stems,"caus|etiol|epidem|diagnos|symptom|explanatori"))),
  list(mlt       =c(regIT(src=stems,"mix|double|doubl$|twofold|aggreg|multifactori|heterogen|multipl|differenti|combin"))),
  list(mon       =c(regIT(src=stems,"single|specific|core|basic|fundamental|essent|character"))),
  list(sub       =c(regIT(src=stems,"subtyp|^type|variant"))),
  list(mod       =c(regIT(src=stems,"model"))),
  list(tmp       =c(regIT(src=stems,"(speed|fast$|rapid|^temporal$)")))
)


# THEORY GENERAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(theories[[2]],theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)

theo <-unique(c(tags1,tags2,tags3))
rm(tags1, tags2, tags3)

for(i in 3:length(theories)){
  tags1 <-tagIT(gregexpr(wordXsYro(theories[[i]],theo),TMcorpus,perl=TRUE),TMcorpus)
  tags2 <-tagIT(gregexpr(wordXsYr(tags1,theories[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  tags3 <-tagIT(gregexpr(wordXsYro(tags2,theories[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  
  types <-tagIT(gregexpr(wordXsYr(theo,tags3),TMcorpus,perl=TRUE),TMcorpus)
  TMcorpus<-subIT(src<-list(tags=types,str=paste(c("theo~",names(theories[i])),collapse=""),cor=TMcorpus)) 
  rm(tags1,tags2,tags3,types,src)
}
# THEORY ETIOLOGY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# also change separate mentions of type etiology
tags1 <-tagIT(gregexpr(wordXsYro(regIT(src=stems,"etiol|epidem"),regIT(src=stems,"etiol|epidem")),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" theo~eti ",cor=TMcorpus)) 
rm(tags1,src)  


# THEORY REST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# also change separate mentions of type subtype
tags1 <-tagIT(gregexpr(regIT(src=stems,"compon"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" theo~com ",cor=TMcorpus)) 
rm(tags1,src)  
# also change separate mentions of type subtype
tags1 <-tagIT(gregexpr(wordXsYro(regIT(src=stems,"subtyp|^type|variant|surfac"),"~dys~"),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(regIT(src=stems,"subtyp|^type|variant|surfac"),tags1),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags2,str=" theo~sub ",cor=TMcorpus)) 
rm(tags1,tags2,theo,src)  

tags1 <-tagIT(gregexpr(wordXsYro(theories[[2]],theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,theories[[2]]),TMcorpus,perl=TRUE),TMcorpus)

theo <-unique(c(tags1,tags2,tags3))
TMcorpus<-subIT(src<-list(tags=tags2,str=" ~theo~ ",cor=TMcorpus)) 
rm(tags1, tags2, tags3, theo)

# NEUROSCIENCE (NEURO) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
neuro=c(
  list(neuro_term=c("neuro")),
  list(neuro_syns=c(regIT(src=stems,"neur(o|al)(sci|log|scientif|\\b)$"))),            
  list(ana       =c(regIT(src=stems,"neuroana"))),
  list(locstrc   =c(regIT(src=stems,"(^gyr|^lob|sulc|^hemispher$|region|(o|^)structur|^area$|^system$|topolog|nucle(u|i)|layer|neuron|^parvo$|^magno$|glia|axon|^struct|cell|pathway|circuitri|substrat|architectur|cytoa|tissu|slice|^cere|(cort(ex|ic)*o*\\b))"),"struc\\sanatom\\ssubstrat","struc\\snetwork", "brainstem","brain","(gr(a|e)y|white)\\smatter","anatom\\ssubstrat","struc\\s\\w*\\ssubstrat")),
  list(loctags   =c(regIT(src=stems,"((^anter(o|ior)*)|(^centr(o|al)*$)|(cross$)|(^extra$)|(^sub$)|(^sup(erior|ra)$)|(ventr(i|o|al))|((o|a|\\b)stri(at|atal|\\b))|(^front(o|al)*$)|(^int(ra|er)$)|(pariet(o|al)*)|(occipit(o|al)*)|(^medi(o|al)*$)|(mid(dl)*$)|(sagitt(o|al)*)|(^tempor(o|al)*$)|(posteri(or|al)*)|later|sylvi|somato|dors|^left|^right)"),"peri","bi","unilater")),
  list(locname   =c(regIT(src=stems,"(thala|wernick|insula|brodm|gesch|angular|heschl|broca|cingul|^striat$|planum|(pre|\\b)motor(c|\\b)|genicul|^ventricl$)"),"corpus\\scallosum")),
  list(funcstrc   =c(regIT(src=stems,"^system$|network|nucle(u|i)|layer|neuron|cell|parvo|magno|glia|pathway|circuitri|substrat|tissu|pattern|process|sassembl|^cere|(cort(ex|ic)*o*\\b))"),"brainstem","brain")),
  list(funct      =c(regIT(src=stems,"(neuro|electro)physio|neurotr|activ"),"neural\\scorrel","brain\\sactiv","brain\\sprocess","activ\\spattern","function\\sactiv\\spattern", "activ\\scell\\sassembl","cell\\sassembl","function\\sactiv","function\\snetwork","activ\\snetwork")),
  list(measr    =c(regIT(src=stems,"\\b(eeg|fmri|pet|erp|meg|imag)\\b|volumetr|morphometri|(electro|encephalo|tomo)gra(ph|m)|neuroimag|theta|beta|record"),
                   "diffus\\stensor","positron\\semiss\\stomographi","neuroimag\\stechniqu","struc\\sneuroimag","cerebr\\sblood\\sflow","eeg\\sactiv","evok\\spotenti","scalp\\spotenti","field\\seeg","function\\smagnet", "mism\\snegat","neuroimag","mismatch\\snegat","mismatch\\snegat\\swave")),
  list(patho     =c(regIT(src=stems,"neuro(psy|path)|ectop|malf|vasc|dyspl|myel|lesi|anomal|polymi|migrat|postm|foc(i|al)|neurotox|cystic|neural|asymm|hypersynchr|discon|(neuro|histo)patholog|(neuro|histo)(sur|rad|log$)|neurof|paroxysm|(neuro|patho|dys)genesi"),"focal\\sdisrupt", "cell\\ssize\\sdistribut","brain\\sfoci","aberr\\ssymmetri","vasc\\smalf", "abnorm\\sconnect","post\\smortem","brain\\s~dys~","~dys~\\sbrain","brain\\sdys~sld","dys~sld\\sbrain"))
)

# NEURO ANATOMY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(neuro[[2]],neuro[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,neuro[[2]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" ~cns~ ",cor=TMcorpus)) 
rm(tags1, tags2, tags3, src)

tags1 <-tagIT(gregexpr(wordXsYro(neuro[[3]],neuro[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,neuro[[3]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" cns~ana ",cor=TMcorpus))
rm(tags1, tags2, tags3)


# NEURO STRUCTURE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(neuro[[6]],neuro[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[6]]),TMcorpus,perl=TRUE),TMcorpus)
named <-tagIT(gregexpr(wordXsYro(tags2,neuro[[6]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(neuro[[5]],neuro[[5]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,neuro[[5]]),TMcorpus,perl=TRUE),TMcorpus)
loca <- tagIT(gregexpr(wordXsYro(tags4,neuro[[5]]),TMcorpus,perl=TRUE),TMcorpus)

tags5 <-tagIT(gregexpr(wordXsYro(neuro[[4]],neuro[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags6 <-tagIT(gregexpr(wordXsYr(tags5,neuro[[4]]),TMcorpus,perl=TRUE),TMcorpus)
struc <-tagIT(gregexpr(wordXsYro(tags6,neuro[[4]]),TMcorpus,perl=TRUE),TMcorpus)

namloc <-tagIT(gregexpr(wordXsYr(named,loca),TMcorpus,perl=TRUE),TMcorpus)
namstr <-tagIT(gregexpr(wordXsYr(named,struc),TMcorpus,perl=TRUE),TMcorpus)
locstr <-tagIT(gregexpr(wordXsYr(loca,struc),TMcorpus,perl=TRUE),TMcorpus)

namlocnam <-tagIT(gregexpr(wordXsYr(namloc,namstr),TMcorpus,perl=TRUE),TMcorpus)
namlocstr <-tagIT(gregexpr(wordXsYr(namloc,locstr),TMcorpus,perl=TRUE),TMcorpus)
locstrnam <-tagIT(gregexpr(wordXsYr(locstr,namstr),TMcorpus,perl=TRUE),TMcorpus)
struct   <-tagIT(gregexpr(wordXsYr(struc,c("~cns~","cns~ana","anatom","anatomi")),TMcorpus,perl=TRUE),TMcorpus)

struct1 <-unique(c(struc,loca,named,namloc,namstr,locstr,namlocnam,namlocstr,locstrnam,struct)) 
TMcorpus<-subIT(src<-list(tags=struct1,str=" cns~str ",cor=TMcorpus))   

struct2   <-tagIT(gregexpr(wordXsYr("cns~str",c("~cns~","cns~ana","anatom","anatomi")),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=struct2,str=" cns~str ",cor=TMcorpus))   

rm(tags1, tags2, tags3, tags4, tags5, tags6, src, struc,loca,named,namloc,namstr,locstr,namlocnam,namlocstr,locstrnam,struct,struct1,struct2)


# NEURO FUNTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(neuro[[8]],neuro[[8]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[8]]),TMcorpus,perl=TRUE),TMcorpus)
func  <-tagIT(gregexpr(wordXsYro(tags2,neuro[[8]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(neuro[[7]],neuro[[7]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,neuro[[7]]),TMcorpus,perl=TRUE),TMcorpus)
funcs <-tagIT(gregexpr(wordXsYro(tags4,neuro[[7]]),TMcorpus,perl=TRUE),TMcorpus)

funct <-unique(c(funcs,func))

tags5 <-tagIT(gregexpr(wordXsYr(funct,neuro[[7]]),TMcorpus,perl=TRUE),TMcorpus)
tags6 <-tagIT(gregexpr(wordXsYr(funct,neuro[[8]]),TMcorpus,perl=TRUE),TMcorpus) 
tags7 <-tagIT(gregexpr(wordXsYro(tags5,tags6),TMcorpus,perl=TRUE),TMcorpus)

funct <-unique(c(funct,tags7))

tags8 <-tagIT(gregexpr(wordXsYr(funct," ~cns~ "),TMcorpus,perl=TRUE),TMcorpus)

funct <-unique(c(funct,tags8))
TMcorpus<-subIT(src<-list(tags=funct,str=" cns~fun ",cor=TMcorpus))     

rm(tags1,tags2,tags3,tags4,tags5,tags6,tags7,tags8,func,funcs,funct,src)



# NEURO MEASUREMENT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(neuro[[9]],neuro[[9]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[9]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,neuro[[9]]),TMcorpus,perl=TRUE),TMcorpus)

tags4 <-tagIT(gregexpr(wordXsYr(tags3,"cns~fun"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags4,str=" cns~msr~fun ",cor=TMcorpus))     

tags5 <-tagIT(gregexpr(wordXsYr(tags3,"cns~str"),TMcorpus,perl=TRUE),TMcorpus)
tags6 <-tagIT(gregexpr(wordXsYr(tags3,"cns~ana"),TMcorpus,perl=TRUE),TMcorpus)
tags7 <-tagIT(gregexpr(wordXsYro(tags5,tags6),TMcorpus,perl=TRUE),TMcorpus) 
TMcorpus<-subIT(src<-list(tags=tags7,str=" cns~msr~str ",cor=TMcorpus))   

TMcorpus<-subIT(src<-list(tags=tags3,str=" cns~msr ",cor=TMcorpus))   
rm(tags1,tags2,tags3,tags4,tags5,tags6,tags7,src)


# NEUR PATHOLOGY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(neuro[[10]],neuro[[10]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,neuro[[10]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,neuro[[10]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" cns~pat ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIOLOGY (BIO) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

biology=c(
  list(bio_term=c("biology")),
  list(bio_syns=c(regIT(src=stems,"biol") )),
  list(gene   =c(regIT(src=stems,"(yto|ho|ro|\\b)gen(e|o)(t|\\b)|poly(g|mo)|inna|predis|hered|evol|congenit|twin|risk|loc(i|u)|chrom|bioch|knock|allel|genom|(di|mono)zygot|mendelian"),"candid\\s~dys~\\ssuscept\\sgene","candid\\s~dys~\\sgene","candid\\sgene","~dys~\\sgene","genet\\srisk")),
  list(devo  =c(regIT(src=stems,"(yo|po)gen(e|o)(s)|embryn|fet(a|u)|utero|puber|postnat|adult|child|toddler|infant|lifetim|adolesc"))),
  list(envo  =c(regIT(src=stems,"parent|^famili|nativ|school$|^educ|profes|caree|^soci|cultu|nativ|peer|popu|lifes|resourc|envir|ecolog"))),
  list(epigen=c("senviron\\sinteract","gene\\senviron","environ\\sgene\\sinteract","envir\\sgene","gene\\sinteract\\senviron",
                "environ\\sinteract\\sgene",regIT(src=stems,"epigenet|ontogenet|^express|^gene$|^genet$|envir|interact") )),
  list(body  =c(regIT(src=stems,"(\\b(emg)\\b)|(biof)|immun|^electro(d|clin)*\\b|kinet|endoc|card|^heart|horm|phenotyp|test(o|i)|estro|stress|myo|dendrit|capillari|musc|eye|miosi|^h*ear(t|i)*\\b)|stenosi") ))
)

# BIO GENERAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[2]],biology[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[2]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags3,str=" ~bio~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIO GENES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[3]],biology[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[3]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" bio~gen ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIO DEVELOPMENT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[4]],biology[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[4]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" bio~dev ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIO ENVIRONMENT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[5]],biology[[5]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[5]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[5]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" bio~env ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIO ENVIR_GENE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[6]],biology[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[6]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" bio~epi ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# BIO BODY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(biology[[7]],biology[[7]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,biology[[7]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,biology[[7]]),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=tags3,str=" bio~bod ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)


# LANGUAGE (LAN) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

language=c(
  list(spe_term=c("language")),
  list(lan_syns=c(regIT(src=stems,"^langu|tongu|(^|bi|mono)lingu"))),
  list(lan_mods=c(regIT(src=stems,"^mother|nativ|local$|^natur|synth|classic$|tonal|formal"))),  
  list(type=c(regIT(src=stems,"japa|chine|engl|dutch|germa|finn|dani|swedi|itali|spani|norw|fr(a|e)n|gree(c|k)|arab|hebr|polish|europ($|ean)"))) 
)


# LANGUAGE GENERAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(language[[2]],language[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,language[[2]]),TMcorpus,perl=TRUE),TMcorpus)
mod   <-tagIT(gregexpr(wordXsYro(tags2,language[[2]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(language[[2]],language[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,language[[4]]),TMcorpus,perl=TRUE),TMcorpus)
type  <-tagIT(gregexpr(wordXsYro(tags4,language[[4]]),TMcorpus,perl=TRUE),TMcorpus)

lang  <-unique(unlist(c(mod,type)))

TMcorpus<-subIT(src<-list(tags=lang,str=" ~lan~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,tags4,lang,src)



# SPEECH PRODUCT (SPR)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speech =c(
  list(spe_term=c("speech")),
  list(spe_syns=c(regIT(src=stems,"^speech|verbal|speak|spoken|pronunci|^oral$|dictate"))),
  list(spe_units=c(regIT(src=stems,"^(non|pseudo)word|syllab|conson|vowel|homophon"),"stop\\sconson","onset\\srime")),
  list(spe_mods=c(regIT(src=stems,"onset|distinct|cluster|contrast|unit|^grain|singl"),theories$mlt,language$type)),
  list(tmp=c(theories$tmp,regIT(src=stems,"^rate$|slow|^dur"),"theo~tmp")),
  list(act=c(regIT(src=stems,"voic|articul|phonotact|sonor|lary|product|reproduct|gestur"),"manner\\sarticul","articulatori\\smonitor\\sprocess","articul\\svari","articul\\sspeech", "articul\\svoic\\smanner","articulatori\\scompon\\sspeech","articulatori\\smovement"))
)



# SPEECH LANGUAGE UNITS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(speech[[3]],speech[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,speech[[3]]),TMcorpus,perl=TRUE),TMcorpus)
units <-tagIT(gregexpr(wordXsYro(tags2,speech[[3]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(speech[[4]],speech[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,speech[[4]]),TMcorpus,perl=TRUE),TMcorpus)
mod   <-tagIT(gregexpr(wordXsYro(tags4,speech[[4]]),TMcorpus,perl=TRUE),TMcorpus)

# these are speech~language units
unmod1 <-tagIT(gregexpr(wordXsYro(mod,units),TMcorpus,perl=TRUE),TMcorpus)
unmod2 <-tagIT(gregexpr(wordXsYr(unmod1,units),TMcorpus,perl=TRUE),TMcorpus)
unmod3 <-tagIT(gregexpr(wordXsYro(unmod2,units),TMcorpus,perl=TRUE),TMcorpus)
unmod4 <-tagIT(gregexpr(wordXsYr(mod,unmod3),TMcorpus,perl=TRUE),TMcorpus)

unmod  <-unique(c(unmod3,unmod4))
TMcorpus<-subIT(src<-list(tags=unmod,str=" lan~uni ",cor=TMcorpus))     
rm(tags1,tags2,tags3,tags4,units,mod,unmod1,unmod2,unmod3,unmod4,src)


# SPEECH PRODUCTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(speech[[2]],speech[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,speech[[2]]),TMcorpus,perl=TRUE),TMcorpus)
spr   <-tagIT(gregexpr(wordXsYro(tags2,speech[[2]]),TMcorpus,perl=TRUE),TMcorpus)

spr1   <-tagIT(gregexpr(wordXsYro(spr,speech[[6]]),TMcorpus,perl=TRUE),TMcorpus)
spr2   <-tagIT(gregexpr(wordXsYr(spr1,speech[[6]]),TMcorpus,perl=TRUE),TMcorpus)
spr3   <-tagIT(gregexpr(wordXsYro(spr2,speech[[6]]),TMcorpus,perl=TRUE),TMcorpus)
spr4   <-tagIT(gregexpr(wordXsYro(spr,spr3),TMcorpus,perl=TRUE),TMcorpus)

TMcorpus<-subIT(src<-list(tags=spr4,str=" ~spr~ ",cor=TMcorpus))     
rm(tags1,tags2,spr,spr1,spr2,spr3,spr4,src)

spelan  <-tagIT(gregexpr(wordXsYr("~spr~","lan~uni"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=spelan,str=" spr~lan~uni ",cor=TMcorpus))     
rm(spelan,src)


# SPEECH PRODUCTION TEMPORAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1  <-tagIT(gregexpr(wordXsYro(speech[[5]],speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
tags2  <-tagIT(gregexpr(wordXsYr(tags1,speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
tags3  <-tagIT(gregexpr(wordXsYro(tags2,speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)

spetmp1<-tagIT(gregexpr(wordXsYr(tags3,"~spr~"),TMcorpus,perl=TRUE),TMcorpus)
spetmp2<-tagIT(gregexpr(wordXsYr(tags3,speech[[2]]),TMcorpus,perl=TRUE),TMcorpus)

spptmp <-unique(c(spetmp1,spetmp2))
TMcorpus<-subIT(src<-list(tags=spptmp,str=" spr~tmp ",cor=TMcorpus))     
rm(spptmp,spetmp1,spetmp2,src)

lantmp  <-tagIT(gregexpr(wordXsYr("lan~uni",speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=lantmp,str=" lan~uni~tmp ",cor=TMcorpus))     
rm(lantmp,src)

# SCRIPT (SCR)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

script =c(
  list(scr_term=c("script")),
  list(scr_syns=c(regIT(src=stems,"^script|print|typolog|(hand|^)writ(e|$)"),"grapho\\smotor")),
  list(scr_units=c(regIT(src=stems,"^(non|pseudo)word|letter|symbol|character$|trigram"),"gram")),
  list(scr_mods=c(regIT(src=stems,"string|boundari|^form$|featur|cluster|contrast|writ|transpar|shallow|inconsist|opaqu|regular|graphic|unit|^grain|singl"),
                  "graphic\\sform", theories$mlt,language$type)),
  list(tmp=c(speech$tmp)),
  list(scr=c(regIT(src=stems,"alphab|ortho|logo|grapho")))
)


# SCRIPT LANGUAGE UNITS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(script[[4]],script[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,script[[6]]),TMcorpus,perl=TRUE),TMcorpus)
scrp <- tagIT(gregexpr(wordXsYro(tags2,script[[6]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=scrp,str=" ~scr~ ",cor=TMcorpus))     
rm(tags1,tags2,scrp,src)

tags1 <-tagIT(gregexpr(wordXsYro(script[[4]],script[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,script[[4]]),TMcorpus,perl=TRUE),TMcorpus)
units <-tagIT(gregexpr(wordXsYro(tags2,script[[4]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(script[[4]],script[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,script[[3]]),TMcorpus,perl=TRUE),TMcorpus)
mod   <-tagIT(gregexpr(wordXsYro(tags4,script[[3]]),TMcorpus,perl=TRUE),TMcorpus)

# these are script~language units
unmod1 <-tagIT(gregexpr(wordXsYro(units,mod),TMcorpus,perl=TRUE),TMcorpus)
unmod2 <-tagIT(gregexpr(wordXsYr("~scr~",unmod1),TMcorpus,perl=TRUE),TMcorpus)
unmod3 <-tagIT(gregexpr(wordXsYr(mod,unmod1),TMcorpus,perl=TRUE),TMcorpus)

unmod   <-unique(c(mod,unmod2,unmod3))
TMcorpus<-subIT(src<-list(tags=unmod,str=" scr~uni ",cor=TMcorpus))     
rm(tags1,tags2,tags3,tags4,unmod1,unmod2,unmod3,src)


# SCRIPT PRODUCTION  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

scrlan  <-tagIT(gregexpr(wordXsYr("~scr~","lan~uni"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=scrlan,str=" scr~uni ",cor=TMcorpus))     
rm(scrlan,src)

actmods= c(regIT(src=stems,"product|skill|perform|^abil|control|manu|coordin|sequenti|clum|repetit|behavior"))
tags1  <-tagIT(gregexpr(wordXsYr("scr~uni",actmods),TMcorpus,perl=TRUE),TMcorpus)
tags2  <-tagIT(gregexpr(wordXsYr("~scr~",actmods),TMcorpus,perl=TRUE),TMcorpus)
actwri   <-unique(c(tags1,tags2))
TMcorpus<-subIT(src<-list(tags=actwri,str=" scr~uni ",cor=TMcorpus))     
rm(tags1,tags2,actwri,src)


# SCRIPT PRODUCTION TEMPORAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scrtmp  <-tagIT(gregexpr(wordXsYr(speech[[2]],speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
arttmp  <-tagIT(gregexpr(wordXsYr("~scr~",speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
wrttmp  <-tagIT(gregexpr(wordXsYr("scr~uni",speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)

#artltmp  <-tagIT(gregexpr(wordXsYr("spr~lan~uni",speech[[5]]),TMcorpus,perl=TRUE),TMcorpus)
scrtmp  <-unique(c(scrtmp,arttmp,wrttmp))
TMcorpus<-subIT(src<-list(tags=scrtmp,str=" scr~tmp ",cor=TMcorpus))     
rm(scrtmp,arttmp,wrttmp,src)


# PERCEPTION (PER) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
perception=c(
  list(per_term=c("perception")),
  list(per_syns=c(regIT(src=stems,"perce(i|p)"))),
  list(per_mods=c(regIT(src=stems,"\\bsensori(neur|\\b)|process|detect|discrim|^sens$|identif|^ident|categor|recognit"))),
  list(measr   =c(regIT(src=stems,"psychoph|threshold|gap|attent|pattern|featur|nois|^modul$|categori|boundari|frequenc|amplitud"))),
  list(mmod    =c(regIT(src=stems,"multi(mod|sens)|audiovi|crossmod|integr|crossmod|sensorimotor"))),
  list(aud     =c(regIT(src=stems,"audiomet|audit|pitc|sine|aur|acou|^so(u|n)[no]|rhyth|beat|music|aur|hear(d|\\b)"),"rise\\stime","envelop\\smodul",
                  "envelop\\sonset","listen\\sdichot", "dichot\\slisten")),
  list(spe     =c(regIT(src=stems,"^speech$|formant|spectr|allophon|prosod|inton"),"speech\\srhyth","categor\\spercept\\sspeech","auditori\\sspeech\\sprocess")), 
  list(viz     =c("random\\sdot","appar\\smotion","vergenc","spatial", regIT(src=stems,"(op(h)*t(o|ha|al))|^visu|flick|lumin|spatio|object|occl|ocular$|^posit"))),
  list(som     =c(regIT(src=stems,"proprio|^tact|somatosens"))),
  list(tmp     =c(speech$tmp))
)

# PERCEPTION GENERAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(perception[[2]],perception[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,perception[[2]]),TMcorpus,perl=TRUE),TMcorpus)
perc1 <-tagIT(gregexpr(wordXsYro(tags2,perception[[2]]),TMcorpus,perl=TRUE),TMcorpus)

tags3 <-tagIT(gregexpr(wordXsYro(perception[[3]],perception[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYr(tags3,perception[[3]]),TMcorpus,perl=TRUE),TMcorpus)
perc2 <-tagIT(gregexpr(wordXsYro(tags4,perception[[3]]),TMcorpus,perl=TRUE),TMcorpus)

tags5 <-tagIT(gregexpr(wordXsYro(perception[[4]],perception[[4]]),TMcorpus,perl=TRUE),TMcorpus)
tags6 <-tagIT(gregexpr(wordXsYr(tags5,perception[[4]]),TMcorpus,perl=TRUE),TMcorpus)
perc3 <-tagIT(gregexpr(wordXsYro(tags6,perception[[4]]),TMcorpus,perl=TRUE),TMcorpus)

tags7 <-tagIT(gregexpr(wordXsYr(perc1,perc2),TMcorpus,perl=TRUE),TMcorpus)
tags8 <-tagIT(gregexpr(wordXsYro(tags7,perc3),TMcorpus,perl=TRUE),TMcorpus)
tags9 <-tagIT(gregexpr(wordXsYr(perc2,perc3),TMcorpus,perl=TRUE),TMcorpus)

percpt <-unique(unlist(c(tags7,tags8,tags9)))
rm(tags1,tags2,tags3,tags4,tags5,tags6,tags7,tags8,tags9,perc1,perc2,perc3)


# PERCEPTION AUDIO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYr(percpt,perception[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYro(tags1,perception[[6]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr(tags2,perception[[6]]),TMcorpus,perl=TRUE),TMcorpus)

aud <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=aud,str=" per~aud ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(perception[[6]],perception[[6]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~aud~ ",cor=TMcorpus))    


# PERCEPTION SPEECH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYro(c(percpt,"per~aud","~spr~"),perception[[7]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,percpt),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr(tags2,"per~aud"),TMcorpus,perl=TRUE),TMcorpus)

tags4 <-tagIT(gregexpr(wordXsYr("per~aud","lan~uni"),TMcorpus,perl=TRUE),TMcorpus)
tags5 <-tagIT(gregexpr(wordXsYr("per~aud","spr~lan~uni"),TMcorpus,perl=TRUE),TMcorpus)
tags6 <-tagIT(gregexpr(wordXsYr("per~aud",tags3),TMcorpus,perl=TRUE),TMcorpus)

spperc <-unique(unlist(c(tags3,tags4,tags5,tags6)))
TMcorpus<-subIT(src<-list(tags=tags3,str=" per~spp ",cor=TMcorpus)) 

rm(tags1,tags2,tags3,tags4,tags5,tags6,src)

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(perception[[7]],perception[[7]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~spp~ ",cor=TMcorpus))     


# PERCEPTION VISUAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYr(percpt,perception[[8]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYro(tags1,perception[[8]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr(tags2,perception[[8]]),TMcorpus,perl=TRUE),TMcorpus)

viz <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=viz,str=" per~viz ",cor=TMcorpus))     
rm(tags1,tags2,tags3,viz,src)

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(perception[[8]],perception[[8]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~spp~ ",cor=TMcorpus))     
# PERCEPTION SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYr(c(percpt,"per~viz"),"~scr~"),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr("per~viz","lan~uni"),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr("per~viz","scr~lan~uni"),TMcorpus,perl=TRUE),TMcorpus)

scrp <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=scrp,str=" per~scr ",cor=TMcorpus))     
rm(tags1,tags2,tags3,scrp,src)

# PERCEPTION SOMATO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tags1 <-tagIT(gregexpr(wordXsYr(percpt,perception[[9]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYro(tags1,perception[[9]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr(tags2,perception[[9]]),TMcorpus,perl=TRUE),TMcorpus)

soma <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=soma,str=" per~som ",cor=TMcorpus))     
rm(tags1,tags2,tags3,soma,src)

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(perception[[9]],perception[[9]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~som~ ",cor=TMcorpus))     

# PERCEPTION TEMPORAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tags1 <-tagIT(gregexpr(wordXsYr(perception[[10]],"per~aud"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" per~aud~tmp ",cor=TMcorpus))     

tags2 <-tagIT(gregexpr(wordXsYr(perception[[10]],"per~viz"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags2,str=" per~viz~tmp ",cor=TMcorpus))     

tags3 <-tagIT(gregexpr(wordXsYr(perception[[10]],"per~spp"),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags3,str=" per~spp~tmp ",cor=TMcorpus))     
rm(tags1,tags2,tags3,src)

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(perception[[5]],perception[[5]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" per~mlt ",cor=TMcorpus))    

# ACTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
action=c(
  list(act_term=c("action")),
  list(act_syns=c(regIT(src=stems,"^action|(psycho|\\b)motor$|postur|^balan|sway|grasp|reach|^tap|sensorim|exercis"),"motor\\sproduct",
                  "fine\\smotor","sensori\\smotor","motor\\sproduct\\skill","motor\\skill","postur\\ssway","postur\\sstabil")),   
  list(mods = c(regIT(src=stems,"product|skill|perform|(^|st)abil|control|manu|coordin|sequenti|clum|repetit|behavior"))),
  list(eye  =c(regIT(src=stems,"saccad|oculom|eye"),"ocular\\smotor")),
  list(spr  =c(speech$act,"speech\\sfluency")),
  list(tmp  =c(speech$tmp))
)

tags1 <-tagIT(gregexpr(wordXsYr(action[[2]],action[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(action[[2]],action[[3]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYr(tags2,action[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags4 <-tagIT(gregexpr(wordXsYro(action[[2]],action[[2]]),TMcorpus,perl=TRUE),TMcorpus)

acti <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=acti,str=" ~act~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,tags4,acti,src)

for(i in 4:length(action)){
  tags1 <-tagIT(gregexpr(wordXsYro(action[[i]],"~act~"),TMcorpus,perl=TRUE),TMcorpus)
  tags2 <-tagIT(gregexpr(wordXsYr(tags1,action[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  tags3 <-tagIT(gregexpr(wordXsYro(tags2,action[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  
  types <-tagIT(gregexpr(wordXsYr("~act~",tags3),TMcorpus,perl=TRUE),TMcorpus)
  TMcorpus<-subIT(src<-list(tags=types,str=paste(c(" act~",names(action[i])," "),collapse=""),cor=TMcorpus)) 
  rm(tags1,tags2,tags3,types,src)
}

# Clean up leftovers
tags1 <-tagIT(gregexpr(wordXsYro(action[[2]],action[[2]]),TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str=" ~act~ ",cor=TMcorpus))  

# READSPELL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

readspell=c(
  list(rsp_term=c("reading~spelling")),
  list(rsp_syns=c(regIT(src=stems,"spell|^read(in|\\b)|^literac|reader|name|decod")))
)

tags1 <-tagIT(gregexpr(wordXsYro(readspell[[2]],readspell[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,readspell[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,readspell[[2]]),TMcorpus,perl=TRUE),TMcorpus)

rs <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=rs,str=" ~lit~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,rs)  

# MEMORY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

memory=c(
  list(mem_term=c(regIT(src=stems,"memo"),"memori\\sstorag","storag\\scapac","memori\\saccess")),
  list(mem_type=c(regIT(src=stems,"episod|^semant|^term$|spatial|^recogn|audit|^visu|^verb|object|sequenc|^motor$|lexico"),"(short\\sterm)"))
)

tags1 <-tagIT(gregexpr(wordXsYro(memory[[1]],memory[[1]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,memory[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,memory[[1]]),TMcorpus,perl=TRUE),TMcorpus)

mem <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=mem,str=" ~mem~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,mem,src)


# ATTENTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

att=c(
  list(mem_term=c(regIT(src=stems,"attent")))
)

tags1 <-tagIT(gregexpr(wordXsYro(att[[1]],att[[1]]),TMcorpus,perl=TRUE),TMcorpus)

att <-unique(unlist(c(tags1)))
TMcorpus<-subIT(src<-list(tags=att,str=" ~att~ ",cor=TMcorpus))     
rm(tags1,att,src)

# COGNITION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cognition=c(
  list(cog_term=c(regIT(src=stems,"^cog(nit)*$") )),
  list(cog_abil=c(regIT(src=stems,"^solv|^decis|intell|reason|execut|profil|commun|error|thought|metacog|^attent|inform|socia|embod|functi|proces|load|effort|control|represent|^knowledg|awar|^abil|encod|strategi|strateg|name|decod|speed|^rate$"),"acdemi\\sachi","scholarly\\sachi")) 
)

tags1 <-tagIT(gregexpr(wordXsYro(cognition[[1]],cognition[[1]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,cognition[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,cognition[[1]]),TMcorpus,perl=TRUE),TMcorpus)

cog <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=cog,str=" ~cog~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,cog,src)



# LEARNING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

learn=c(
  list(lrn_term=c("learning")),
  list(lrn_syns=c(regIT(src=stems,"learn|acquis"))),   
  list(dev     =c(regIT(src=stems,"devel"))),
  list(mot     =c(action$act_syns)),
  list(lan     =c(regIT(src=stems,"languag"))),
  list(per     =c(regIT(src=stems,"audit|^visu|^sens$|sensori$|perceptu"),"auditori\\sprocess","audit\\svision","~per~","per~viz","per~aud")),
  list(spk     =c(perception$spk,"act~spk","per~spk"))
)

tags1 <-tagIT(gregexpr(wordXsYro(learn[[2]],learn[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags2 <-tagIT(gregexpr(wordXsYr(tags1,learn[[2]]),TMcorpus,perl=TRUE),TMcorpus)
tags3 <-tagIT(gregexpr(wordXsYro(tags2,learn[[2]]),TMcorpus,perl=TRUE),TMcorpus)

lrn <-unique(unlist(c(tags1,tags2,tags3)))
TMcorpus<-subIT(src<-list(tags=lrn,str=" ~lrn~ ",cor=TMcorpus))     
rm(tags1,tags2,tags3,lrn,src)

for(i in 3:length(learn)){
  tags1 <-tagIT(gregexpr(wordXsYro(learn[[i]],"~lrn~"),TMcorpus,perl=TRUE),TMcorpus)
  tags2 <-tagIT(gregexpr(wordXsYr(tags1,learn[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  tags3 <-tagIT(gregexpr(wordXsYro(tags2,learn[[i]]),TMcorpus,perl=TRUE),TMcorpus)
  
  types <-tagIT(gregexpr(wordXsYr("~lrn~",tags3),TMcorpus,perl=TRUE),TMcorpus)
  TMcorpus<-subIT(src<-list(tags=types,str=paste(c(" lrn~",names(learn[i])," "),collapse=""),cor=TMcorpus)) 
  rm(tags1,tags2,tags3,types,src)
}

tags1 <- tagIT(gregexpr("(impair)|(disord$)|(deficit*$)|(abnorm)|(dysfunct)|(disabl)|(poor)",TMcorpus,perl=TRUE),TMcorpus)
TMcorpus<-subIT(src<-list(tags=tags1,str="~imp~",cor=TMcorpus))     
rm(tags1,src)

tags1 <- tagIT(gregexpr("\\~\\w+\\~",TMcorpus,perl=TRUE),TMcorpus)
tags2 <- tagIT(gregexpr("\\w+\\~\\w+",TMcorpus,perl=TRUE),TMcorpus)
tags3 <- tagIT(gregexpr("\\w+\\~\\w+\\~\\w+",TMcorpus,perl=TRUE),TMcorpus)

inv <-unique(unlist(c(tags1,tags2,tags3)))

TM <- subNIT(src<-list(tags=inv,str=" ",cor=TMcorpus))     

save(TM,df,file=outfile)

}