
##################################################################################################################
##################################################################################################################
# Load glass door dataset
##################################################################################################################
##################################################################################################################

library(tidytext)

data<-readRDS("glassdoor_df")

n<-nrow(data)

bad_txt<-c("â", "-", ",", "/")
data$job_desc1<-gsub(paste(bad_txt, collapse="|"), " ", data$job_desc_raw)
data$job_desc2<-gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$job_desc1)
data$job_desc2<-gsub("([[:upper:]][[:upper:]][[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", data$job_desc2)
data$job_desc3<-gsub("[(]", " ", data$job_desc2)
data$job_desc3<-gsub("[)]", " ", data$job_desc3)


bachelor<-c("\\bbachelor\\b", "\\bbachelors\\b", "\\bundergraduate\\b", "\\bbs\\b", "\\bb.s.\\b", "\\bb.s\\b")
master<-c("\\bmaster\\b", "\\bmasters\\b", "graduate degree", "ms degree", "ms in", "m.s. in", "m.s in"  )
phd<-c("phd", "doctorate", "\\bph\\b")


for (i in 1:n)
{
  
data$python[i] <- any(grepl("\\bpython\\b", data$job_desc3[i], ignore.case=TRUE))
data$R[i] <- any(grepl("\\bR\\b", data$job_desc3[i], ignore.case=TRUE))
data$SAS[i] <- any(grepl("\\bSAS\\b", data$job_desc3[i], ignore.case=TRUE))
data$C[i] <- any(grepl("\\bC+\\b", data$job_desc3[i], ignore.case=TRUE))
data$stata[i] <- any(grepl("\\bstata\\b", data$job_desc3[i], ignore.case=TRUE))
data$SQL[i] <- any(grepl("\\bsql\\b", data$job_desc3[i], ignore.case=TRUE))
data$excel[i] <- any(grepl("\\bexcel\\b", data$job_desc3[i], ignore.case=TRUE))
data$tableau[i] <- any(grepl("\\btableau\\b", data$job_desc3[i], ignore.case=TRUE))
data$spss[i] <- any(grepl("\\bspss\\b", data$job_desc3[i], ignore.case=TRUE))
data$java[i] <- any(grepl("\\bjava\\b", data$job_desc3[i], ignore.case=TRUE))
data$linux[i] <- any(grepl("\\blinux\\b", data$job_desc3[i], ignore.case=TRUE))
data$matlab[i] <- any(grepl("\\bmatlab\\b", data$job_desc3[i], ignore.case=TRUE))
data$NLP[i] <- any(grepl("\\bNLP\\b", data$job_desc3[i], ignore.case=TRUE))
data$hadoop[i] <- any(grepl("\\bhadoop\\b", data$job_desc3[i], ignore.case=TRUE))
data$ruby[i] <- any(grepl("\\bruby\\b", data$job_desc3[i], ignore.case=TRUE))
data$oracle[i] <- any(grepl("\\boracle\\b", data$job_desc3[i], ignore.case=TRUE))
data$sas[i] <- any(grepl("\\bsas\\b", data$job_desc3[i], ignore.case=TRUE))
data$bs[i] <- any(grepl(paste(bachelor, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
data$bs2[i] <- any(grepl("bachelor", data$job_desc3[i], ignore.case=TRUE))
data$masters[i] <- any(grepl(paste(master, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
data$phd[i] <- any(grepl(paste(phd, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
data$degree[i] <- any(grepl("\\bdegree\\b", data$job_desc3[i], ignore.case=TRUE))

}

data$skills<- data$degree+data$phd+data$masters+data$phd+data$bs2+data$bs+data$sas+data$oracle+data$ruby+data$hadoop+data$NLP+data$matlab+data$linux+
  data$java+ data$spss +data$spss +data$tableau+ data$excel+data$SQL+data$stata + data$C + data$SAS +data$R+ data$python

data$skills2<- data$sas+data$oracle+data$ruby+data$hadoop+data$NLP+data$matlab+data$linux+
  data$java+ data$spss +data$spss +data$tableau+ data$excel+data$SQL+data$stata + data$C + data$SAS +data$R+ data$python

data$education<- data$masters+data$phd+data$bs


table(data$skills2)
table(data$education)

saveRDS(data, file="glassdoor_df_cleaned")

#hadoop java linux SOSS MATLAB NLP
summary(data$hadoop)
summary(data$NLP)
summary(data$matlab)
summary(data$linux)
summary(data$java)
summary(data$spss)
summary(data$ruby)
summary(data$oracle)
summary(data$sas)

#include vars for keywords like research etc.

#geo_code
#package ggmap

#vector city and sate
#output latitude and longitude

#Jitter Points

data$job_desc_raw
  unnest_tokens(word, word, )

strsplit(data$job_desc1[4], " ")

#Oracle
data$job_id[data$hadoop==0 & data$NLP==0 & data$java==0& data$R==0 & data$python==0]

data$job_id[data$skills==0]


data$job_id[data$hadoop==0 & data$NLP==0 & data$java==0& data$R==0 & data$python==0& data$excel==0 & data$sas==0]

data$emp_desc1<-gsub(paste(bad_txt, collapse="|"), " ", data$emp_desc_raw)
data$emp_desc2<-gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$emp_desc1)
data$emp_desc2<-gsub("([[:lower:]])([[:digit:]])", "\\1 \\2", data$emp_desc2)
data$emp_desc2<-gsub("([[:digit:]])([[:upper:]])", "\\1 \\2", data$emp_desc2)

data$job_desc2<-gsub("([[:digit:]][[:upper:]][[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", data$job_desc2)
data$job_desc3<-gsub("[(]", " ", data$job_desc2)
data$job_desc3<-gsub("[)]", " ", data$job_desc3)

strsplit(data$job_desc3[data$job_id=="2540303291"]," ")

strsplit(data$emp_desc2[data$job_id=="2525091352"]," ")

 2524534214 2540303291 2534082425 2519063493 2499480680 2541928381 2519295846 2456021237 2517953951 2495806023 2542468889 2540820285

for (i in 1:n)
{
  
  data$founded[i] <- any(grepl("\\bfounded\\b", data$emp_desc2[i], ignore.case=TRUE))
  data$industry[i] <- sub(".*Industry *(.*?) *Revenue.*", "\\1", data$emp_desc2[i], ignore.case=TRUE)
  data$revenue[i] <- any(grepl("\\brevenue\\b", data$emp_desc2[i], ignore.case=TRUE))
  
  
}
tabulate(data$industry)
summary(data$salaries)

summary(data$founded)
data$job_id[data$founded==0]


job_desc_words<-strsplit(data$job_desc3," ")


for (i in 1:n)
{
  
  data$salary_low[i] <- sub(".*[$] *(.*?) *k[-].*", "\\1", data$salaries[i], ignore.case=TRUE)
  data$salary_high[i] <- sub(".*[$] *(.*?) *k[-].*", "\\1", data$salaries[i], ignore.case=TRUE)
  
}

strsplit(data$job_desc3[data$job_id=="2525091352"]," ")

data$salary_low[data$salary_low=="Not listed"]<-""
data$salary_high[data$salary_high=="Not listed"]<-""


data$salary_average<-as.numeric(data$salary_low)+as.numeric(data$salary_high)

hist(data$salary_average)

gsub(".*Industry\\s*|first*", "", data$emp_desc2[2525091352], ignore.case=TRUE)
sub(".*[$] *(.*?) *k[-].*", "\\1", data$salaries[100])
sub(".*[$] *(.*?) *k[()].*", "\\1", data$salaries[100])

summary(lm(data$salary_average~data$SQL+data$python+data$phd+data$masters))