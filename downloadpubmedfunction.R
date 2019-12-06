setwd( "C:/Users/stm/Box/SkandersBOX/PROFILomics/LiteratureMining/ID2019/unzip")
list.files()
library(pacman);p_load('rvest','hellno',stringr,data.table, XML)

#get all the data from an abstract (step2)

getAbstact=function(url,check=FALSE)
{
defineST<-a<-str_extract(pattern='[0-9]+',url)
if(check)
{
if( length(grep(defineST, list.files('../tmpfiles3/')))>0  )  #test if found
{
print(paste0('currently reading file:', defineST));flush.console() 
Rval<-tryCatch({ load( paste0('../tmpfiles3/',  defineST  ,'.Rdata')) },error = function(x) {return('NULL')})
if(exists('rval'))return(rval)
print(paste('REMOVING corrupt file:',defineST)) 
unlink( paste0('../tmpfiles3/',  defineST  ,'.Rdata'))
getAbstact(url,check=FALSE)
}}

p_load('rvest')
input=url; print(url);flush.console() 
 html_nodess=tryCatch({   read_html(input)  }, error = function(...) return(NA))
if(  !exists('html_nodess') )return(NA) ##ERRECHECK
 rval <- tryCatch({html_nodess %>% 
  html_nodes( ".rprtid , .auths , dd , .abstr p , #maincontent h1") %>%
  html_text()   }, error = function(...) getAbstact(url)) #if fail

print(toString(rval))

if(length(rval) == 0) return (NULL)
if(check)save('rval',file= paste0('../tmpfiles3/',  defineST  ,'.Rdata'))
return(rval)
}

library(pacman);p_load('rvest','hellno',stringr,data.table, XML)

SearchTerm ="Diabetic+Nephropathy+Biomarker+Human"
#SearchTerm ="Cardio+Metabolic+Biomarker"
#SearchTerm ="Transplantation+Microbiome"


rm(pubmedMine)
#define a list of search term (step1)
url2= paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="',SearchTerm,'"
&retmax=10000&tool=BLABLA')
con=url(url2)
xml_doc=read_xml(con)
close(con)
xml_data <- xmlParse(xml_doc)
test=xmlToList(xml_data)
PUBMEDID=(test["IdList" ])
print(paste0('the length is:',length(unlist(PUBMEDID))))
st=paste0( 'https://www.ncbi.nlm.nih.gov/pubmed/?term=' ,unlist(PUBMEDID))
#fwrite(DKDnephroid, 'DiabetesNephropathyPMID.csv')
#st=paste0( 'https://www.ncbi.nlm.nih.gov/pubmed/?term=' ,unlist(
#fread('DiabetesNephropathyPMID.csv')[,1]))

if(FALSE)  ##run a predetermined e-search
{
xml_data=xmlParse('esearch.xml')
test=xmlToList(xml_data)
DKD_NEPH_HUMAN_MARK=(test["IdList" ])
st=paste0( 'https://www.ncbi.nlm.nih.gov/pubmed/?term=' ,unlist(DKD_NEPH_HUMAN_MARK))
}

#runit
pubmedMine=lapply( (st), getAbstact,check=TRUE)
# MetaboliteCommonNamesObjList=lapply((metblinks),getCommonName)
save(pubmedMine,file=paste0('../', (gsub('+','',x=SearchTerm)) ,
'__pubmedMine_',Sys.Date(),'.Rdata'))
#load(paste0('../', (gsub('+','',x=SearchTerm)) ,'__pubmedMine_',Sys.Date(),'.Rdata'))
#load('C:/Users/stm/Box/SkandersBOX/PROFILomics/LiteratureMining/ID2019/Microbiome AND Transplantation__pubmedMine_2019-11-06.Rdata')



retabstr=function(x){
x<-unlist(x) #no list
x<-x[which(!grepl('Indexed for MEDLINE',x))] #no filler
z<-(min(  which(sapply(x, function(b)grepl('PMID:',b))) )) #legacy
if(z<2)z<-2 #legacy
Nchars=unlist(lapply(x, nchar))

#abst_loc<-sort(values<-(Nchars ),index=TRUE, decreasing=TRUE)$ix[1] #biggest text = abstr
reviter <-(rev(which(Nchars >200))) ;qq=reviter[1]
#if(length(reviter) >2)
#{ qq = abst_loc
#for( i  in 1:(length(reviter)-1) )
#{print(i)
#if ( reviter[i] - reviter[i+1] == 1) qq<- c(qq,reviter[i+1])
#}
#}
ret<-list(     Title = x[1],
          #Abstract= x[reviter[which(reviter > 2)] %>%
          # (function(x)c(min(x),max(x))) %>% (function(x)tryCatch({x[1]:x[2]},error=function(...)return(NA))) ] %>%
          #  (function(x)paste0(x,collapse=' '))  ,
          AllText<- x%>%    (function(x)paste0(x,collapse=' '))  , #####MAYCRASH if not available
          PMID2 = x[length(x)]) 
return(ret)  } 

x=pubmedMine[[3]]

grepl('Abstract', x)



dfr=do.call(rbind,lapply( pubmedMine,retabstr))
head(dfr, n=10L)
nn_loc=paste0('../', (gsub('+','',x=SearchTerm)) ,'__pubmedMine_',Sys.Date(),'.csv')
fwrite(dfr,nn_loc)

retabstr(x=pubmedMine[1] )
reviter=3

getAbstact(st[1], check=TRUE)

rm(rval)