################################################################################
# Authors: Dalibor Nakladal, Skander Mulder
# Date: 23/10/2015
# Description: This script queries PubMed, mines text of title, author names,
# year, abstract and outputs a wordcloud and an html with formatted mined text.
################################################################################
# LOAD libraries
if (!require('pacman')) install.packages('pacman')
pacman::p_load(rmarkdown,
               RISmed,
               Snowballc,
               tidyverse,
               hellno)

# setup folders
folder <- './temp/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)

################################################################################
# QUERY and actual data download from PubMed
################################################################################
# this is the text string with name of author as passed to the PubMed search bar
# query <- 'Henning RH[Author]'
# query <- 'Majtan T[Author]'
# query <- 'vascular adipose tissue'
query <- 'cystathionine beta synthase'
# create the main object of class 'Medline' which contains all mined data
# and has most methods from package RISmed applicable
pubmed <- EUtilsGet(EUtilsSummary(query))

################################################################################
# some HOUSEKEEPING
################################################################################
# Format the author character strings
author <- Author(pubmed)

# it would be great to simplify this for loop perhaps with lapply
for(i in seq_along(author)) {
      author[[i]] <- author[[i]]$LastName
      author[[i]] <- paste(author[[i]], collapse = ', ')
}
author <- unlist(author)

################################################################################
# EXTRACT interesting data
################################################################################
# JOURNALS
# journal information can be extracted with RISmed functions
# MedlineTA, Title and ISOAbbreviation (probs best for later snatching IF's)
journals <- ISOAbbreviation(pubmed)

# journal country (!)
Country(pubmed)

# the Volume component (and RISmed function) can be used to hint at the age
# of the journal (and impact?)

# extract sought information to create a subset of core dataset
pubmed.subset <- data.frame('Title'=ArticleTitle(pubmed),
                            'Author'=author,
                            'Year'=YearPubmed(pubmed),
                            'PMID'=PMID(pubmed),
                            'doi'=ELocationID(pubmed),
                            'Journal'=Title(pubmed),
                            'Abstract'=AbstractText(pubmed),
                            stringsAsFactors = FALSE)

################################################################################
# CREATE pre-formatted R markdown (.Rmd) file
################################################################################
# can not render/knit to pdf because of greek alphabet letters in text

# setup file path
folder <- './abstracts/'
ifelse(!dir.exists(folder), dir.create(folder), FALSE)
# setup file names
file <- paste0(query, '.Rmd')
path <- paste0(folder, file)

# set up YAML header
cat(c("---",
      "output:",
      paste0("      ", "html_document:"),
      paste0("            ", "self_contained: no"),
      "---",
      ""),
    file = path, append = TRUE, fill = TRUE, sep = "\n")

# write source text to .Rmd file
for (i in c(1:nrow(pubmed.subset))) {
      cat(c(paste('####', pubmed.subset$Title[i]),
            '\n',
            paste(pubmed.subset$Year[i], pubmed.subset$Journal[i]),
            '\n',
            paste0('https://www.ncbi.nlm.nih.gov/pubmed/', pubmed.subset$PMID[i]),
            '\n',
            ifelse(str_detect(string = pubmed.subset$doi[i],
                              pattern = regex(pattern = '^10')),
                   yes = paste0('https://doi.org/', pubmed.subset$doi[i]),
                   no = '\n'),
            '\n',
            pubmed.subset$Author[i],
            '\n',
            pubmed.subset$Abstract[i],
            '\n'),
          file = path, append = TRUE, fill = TRUE, sep = '\n')
}

################################################################################
# RENDER readable .html from .Rmd
################################################################################
# render(input = path, output_dir = folder, encoding = 'UTF-8')
# clean up workspace (remove source .Rmd file)
# file.remove(path)

# XML load code from Skander
# getRVEST=function(x='NCT03478891') {
#       
#       urL=paste0('https://clinicaltrials.gov/ct2/show/',x,'?displayxml=true')
#       require(XML)
#       result = tryCatch({download.file(url = urL, destfile = "testing.xml")}, error = function(e) {
#             return(NA)});if(is.na(result))return(NA)##endifmissing
#       rval=(xmlToList(xmlParse('testing.xml')))
#       fix_rval=lapply(rval,function(ZZ){ if(length(ZZ)==0){return(NA)}
#             return(toString(ZZ))} ) 
#       setDT(data.frame(fix_rval))
#       return(fix_rval)
# }
# 
# getit=lapply( new_a$NTCid, getRVEST)#slow (mine trialsgov)
# 
# base= data.frame(getit[[1]])
# for(QQ in 2:length(getit))#length(fb))
# {
#       new=data.frame(getit[[QQ]])
#       base=rbind(setDT(base), setDT(new),fill=TRUE)
#       flush.console(); print(QQ)
#       
# }
# check1=names(which(apply(base,2,function(x)sum(is.na(x))/length(x)) <0.9))
# base[ ,check1, with=FALSE ]
