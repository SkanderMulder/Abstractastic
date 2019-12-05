################################################################################ 
################################################################################ 
library(pacman) ;p_load(rmarkdown,RISmed,tm, wordcloud,RColorBrewer,SnowballC,data.table)
# library(rmarkdown)
# library(RISmed)
# library(tm)
# library(wordcloud)
# library(RColorBrewer)
# library(SnowballC)
# library(data.table)

#####	


CreateDFR<-function(query='(clinicaltrials.gov) AND "last 3 days"[dp]'   ) { 
#return(query)
# query <- 'Heerspink [Author]'
foo <- RISmed::EUtilsSummary(query)
bar <- RISmed::EUtilsGet(foo)

if(FALSE)
{
# Format the author character strings
author <- RISmed::Author(bar)
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
      author[[i]] <- paste(author[[i]], collapse = ', ')
}
# author <- unlist(author)
}
pubmed_data <- data.frame('Title'=RISmed::ArticleTitle(bar),
                          #'Author'=author,
                          'Year'=RISmed::YearPubmed(bar),
                          'Abstract'=RISmed::AbstractText(bar),					
						  'PMID'			=	RISmed::PMID(bar),
						  'Journal'=RISmed::Title(bar)
				    )
return(setDT(pubmed_data)) #nrow(pubmed_data)
}
a=CreateDFR()#slow
##getdataNTC code clintrialsgov ((mine pubmed)
getNTC<-function(y)
{
startloc= regexpr("NCT",y)
r=substr(y,startloc,startloc+10)
return(r)
}
NTCid=lapply( as.character(unlist(a[,3])) , getNTC)
NTClog=sapply(  NTCid ,function(x) grep('NCT0',x));NTClog<-lapply(NTClog, function(x){if(length(x)==0) return(0)
return(x)})
a$NTCid<-unlist(NTCid); a$NTClog<-unlist(NTClog)
new_a= a[which(unlist(NTClog) ==1),]
new_a<-new_a[]#last50papersn

as.character(unlist((a[,3])))[34]



getRVEST=function(x='NCT03478891')
{
urL=paste0('https://clinicaltrials.gov/ct2/show/',x,'?displayxml=true')
require(XML)
result = tryCatch({download.file(url = urL, destfile = "testing.xml")}, error = function(e) {
   return(NA)});if(is.na(result))return(NA)##endifmissing
rval=(xmlToList(xmlParse('testing.xml')))
fix_rval=lapply(rval,function(ZZ){ if(length(ZZ)==0){return(NA)}
return(toString(ZZ))} ) 
setDT(data.frame(fix_rval))
return(fix_rval)
}

getit=lapply( new_a$NTCid, getRVEST)#slow (mine trialsgov)

base= data.frame(getit[[1]])
for(QQ in 2:length(getit))#length(fb))
{
new=data.frame(getit[[QQ]])
base=rbind(setDT(base), setDT(new),fill=TRUE)
flush.console(); print(QQ)

}
check1=names(which(apply(base,2,function(x)sum(is.na(x))/length(x)) <0.9))
base[ ,check1, with=FALSE ]

#make link between ticker and papers
list.files('../data/')
tickers=read.xlsx('../data/tickers.xlsx')#NSYE

st=apply( base, 1,toString)
st= base$sponsors
###test
names(new_a)
newobj=cbind(new_a,base)
##endtest


search_string=lapply(tickers$Company,function(xz) strsplit(xz,',|/.')[[1]][1])  #define str
result=lapply(search_string,function(X)grep(X,st))  #for every company search through all records
processed_result=lapply(result, function(x){ if(length(x)==0){return(NA)}else{return(x)}})   

tmp1x=tickers[!is.na(unlist(processed_result)), ]
tmp2x=new_a[(unlist(result)), ]
oput=cbind(tmp1x,tmp2x) ;oput

oput=cbind( [which(unlist(ROUGHSEARCH))  ,]  , new_a  [ unlist(result)] )

write.csv(oput ,'oput.csv')

