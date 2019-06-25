CreateDFR<-function(x)
{
  #rm(list=ls())
library(easyPubMed)
try({

    ## Retrieve PubMed data and return a list ot articles
  my_query  <- x
  my_query <- get_pubmed_ids(pubmed_query_string = my_query)
  my_data <- fetch_pubmed_data(my_query, encoding = "ASCII")
  listed_articles <- articles_to_list(my_data)
  }, silent = TRUE)

##Processdata
##EXTRACT EASYPUBMEDCRAP
fun<-function(x) lapply( c("Year","Month", "Day","ArticleTitle","AbstractText","JournalIssue","ArticleId","AffiliationInfo","MedlineTA"),  function(zz)custom_grep(listed_articles[[x]], zz, "char"))
Olist=lapply(1:length(listed_articles) ,fun );Olist[[2]]

Dates=lapply( Olist , function(tobj) {   Dateit<-(paste0(tobj[[1]][1],'-',tobj[[2]][1],'-', tobj[[3]][1]))  ;return(Dateit)}  )


# ###THRESHOLDDAYSOLD
# DaysOldMAX = 20000
# Sys.Date() - DaysOldMAX < unlist(
# lapply(1:length(Olist), function(x)Olist[[zz]][[1]][2])
# )
###TODO add selectionhere to dfr


ExtractedList=lapply(Olist, function(x)c(  #x[[1]][[2]][1],##Date
                             paste(x[[4]][[1]][]),##Title
                             paste(x[[5]][[1]][]),##Abstract
                                   paste(x[[9]][[1]][]),
                                         paste(x[[7]][[1]][]),
                                              # paste(x[[8]][[1]][]),##x
                                                     paste(x[[8]][[1]][])##x
                             ))

# Dates, ExtractedList

#this should be improved bymeans of iteratively combining all elements (not just first one)
#secondly the 'magic number 6' should be derived from the data OR options #2 fill in missing if missing(combine)s

Lvec=which(unlist(lapply(ExtractedList,length)) == max(unlist(lapply(ExtractedList,length))) )   #note that 3 is only because extracting 3
ExtractedListSS = lapply((Lvec), function(x)ExtractedList[[x]] )  #note that 3 is only because extracting 3
ExtractedDataframe=(matrix(unlist(ExtractedListSS)   ,ncol=    max(unlist(lapply(ExtractedList,length)))   , byrow=1))
dfr_rval=data.frame(ExtractedDataframe);dfr_rval$Date <-  unlist(Dates)[Lvec] 
return(dfr_rval)
}
# 
# 
#  v1=strsplit(listed_articles[[4]],' </');v1
#  v1[grepl( 't', casefold(v1))]

