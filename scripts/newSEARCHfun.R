library(pacman);p_load('rvest','hellno',stringr,data.table, XML)


getAbstact=function(URL,checkSave=FALSE)
{
print('getAbstact')
# defineST<-a<-str_extract(pattern='[0-9]+',url)
# if(checkSave)
# {
# if( length(grep(defineST, list.files('../tmpfiles3/')))>0  )  #test if found
# {
# print(paste0('currently reading file:', defineST));flush.console() 
# Rval<-tryCatch({ fread( paste0('../tmpfiles3/',  defineST  ,'.Rdata')) },error = function(x) {return('NULL')})
# return(Rval)
# }}

## parsing data from pubmedsite
p_load('rvest')
input=URL; print(URL);flush.console() 
 html_nodess=tryCatch({   read_html(input)  }, error = function(...) return(NA))
if(  !exists('html_nodess') )return(NA) ##ERRECHECK
 rval <- html_nodess %>% 
  html_nodes( ".rprtid , .auths , dd , .abstr p , #maincontent h1") %>%
  html_text()  

# print(toString(rval))

#output of object
if(length(rval) == 0) return (NULL)
if(checkSave)save('rval',file= paste0('../tmpfiles3/',  defineST  ,'.Rdata'))
return(rval)
}


getLink=function(SearchTerm, n=10)
{
print('getLink')
#define a list of search term (step1)
url2= paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="',SearchTerm,'"
&retmax=',n,'&tool=AbstraTastic')
con=url(url2)
xml_doc=read_xml(con)
# close(con)
xml_data <- xmlParse(xml_doc)
test=xmlToList(xml_data)
PUBMEDID=(test["IdList" ])
print(paste0('the length is:',length(unlist(PUBMEDID))))
st=paste0( 'https://www.ncbi.nlm.nih.gov/pubmed/?term=' ,unlist(PUBMEDID))
return(st)
}

formatAbstract=function(x)
{
x<-unlist(x) #no list
x<-x[which(!grepl('Indexed for MEDLINE',x))] #no filler
z<-(min(which(sapply(x, function(b)grepl('PMID:',b))) )) #legacy
if(z<2)z<-2 #legacy
Nchars=unlist(lapply(x, nchar))

abst_loc<-sort(values<-(Nchars ),index=TRUE, decreasing=TRUE)$ix[1] #biggest text = abstr

reviter =NA
qq=reviter[1];reviter <-(rev(which(Nchars >100))) 
if(length(reviter) >2)
{ qq = abst_loc
for( i  in 1:(length(reviter)-1) )
{#print(i)
if ( reviter[i] - reviter[i+1] == 1) qq<- c(qq,reviter[i+1])
}
}

ret<-list(     Title = x[1],
          Abstract= toString(x[qq])  ,
          PMID2 = x[length(x)]) 
return(ret)  } 


#calling fucntions
rval<-'DKD'  %>%  (function(x,n=3)(getLink(x,n)))  %>% lapply(. ,getAbstact)   %>% lapply(. ,formatAbstract) 
dfr=do.call(rbind,lapply( rval,retabstr))
