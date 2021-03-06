knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

packages<-c("rvest", "stringr","tidytext","vcd", "xtable", "ggplot2", "crayon", "huxtable", "magrittr", "bibtex")

for (i in packages){
  if(!require(i,character.only = T,quietly=T,warn.conflicts = F)){
    install.packages(i, repos = "http://cran.us.r-project.org")
  }
  require(i,character.only = T,quietly=T,warn.conflicts = F)
}

library(rvest)
library(tidytext)
library(vcd)
library(xtable)
library(ggplot2)
library(crayon)
library(huxtable)
library(stringr)
library(magrittr)
library(bibtex)
## #this is the code I used to scrape glassdoor
## 
## ##Used some of stephens code from lab for help, need to figure out how to cite?
## ###################################################################################################
## ###################################################################################################
## 
## ##Load Packages
## ###################################################################################################
## ###################################################################################################
## 
## library(rvest)
## library(stringr)
## 
## ###################################################################################################
## 
## ##  Urls for data scientist positions on glassdoor
## ##  note for some reason glassdoor does not let you go past job post page 33?
## ###################################################################################################
## 
## 
## url <- paste0("https://www.glassdoor.com/Job/new-york-data-scientist-jobs-SRCH_IL.0,8_IC1132348_KO9,23_IP",1,".htm")
## urls <- paste0("https://www.glassdoor.com/Job/new-york-data-scientist-jobs-SRCH_IL.0,8_IC1132348_KO9,23_IP", 1:33, ".htm")
## 
## df1<-data.frame()
## 
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ## First we will get data from summary data for each post from the search result urls
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## 
## 
## ###################################################################################################
## ##  creates fields which are the individual job posts from glassdoor
## ###################################################################################################
## for (i in 1:33){
##   paste(i)
##   download.file(urls[i], destfile = "scrapedpage.html", quiet=TRUE)
##   fields <-read_html("scrapedpage.html") %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "jl", " " ))]')
## 
##   ###################################################################################################
##   ##  creates job title which are the individual job posts
##   ###################################################################################################
##   title <- fields %>% html_nodes(".flexbox .jobLink") %>% html_text() %>% trimws()
## 
##   ###################################################################################################
##   ## gets salaries and replaces not listed when salary information is missing
##   ###################################################################################################
##   salaries <- sapply(fields, function(x) {
##     tmp <- html_nodes(x, ".small") %>% html_text()
##     ifelse(length(tmp) == 0, "Not listed", trimws(tmp))
##   })
##   salaries <- trimws(gsub("(Glassdoor est.)", "", salaries))
## 
##   ####################################################################################################
##   ##  gets job location
##   ###################################################################################################
## 
##   locations <- fields %>% html_nodes(".loc") %>% html_text() %>% trimws()
## 
## 
##   ####################################################################################################
##   ##  gets emplyer, employer id and job id
##   ###################################################################################################
## 
##   ##can;t figure out how to get gsub or strreplace to recognize the - to remove the city name
##   employer <- fields %>% html_nodes(".empLoc") %>% html_text() %>% trimws()
## 
##   employer_id<-fields %>% html_attr("data-emp-id")
## 
##   job_id<-fields %>% html_attr("data-id")
## 
## 
##   ####################################################################################################
##   ##  create and append data frame with job posts
##   ###################################################################################################
##   df1a<-cbind(job_id, employer_id, title, salaries, locations, employer)
## 
##   df1<-rbind(df1,df1a)
## }
## 
## save(df1, file="glassdoor_df1.r")
## 
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ## Now we will get data from accessing the websites for the individual job posts using
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## 
## 
## ####################################################################################################
## ##  creates job urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
## ###################################################################################################
## 
##  df1$job.urls <- paste0("https://www.glassdoor.com/job-listing/data-scientist-emc-research-JV_IC1145845_KO0,14_KE15,27.htm?jl=",
##                      df1$job_id)
## 
## ####################################################################################################
## ## gets the raw job description, days posted, and employed from the job posts
## ###################################################################################################
## 
##   df1$job_desc_raw<-""
##   df1$employer2<-""
## #used to compare null results in loop
##   a <- character(0)
##   n<-nrow(df1)
##   for (i in 1:n)
##     {
## 
##     Sys.sleep(1)
## 
##       download.file(df1[i,]$job.urls, destfile = "scrapedpage.html", quiet=TRUE)
## 
##         website<- read_html("scrapedpage.html")
## 
##         df1[i,]$job_desc_raw <-
##             (if (identical(a,(website %>% html_nodes("#JobDescContainer") %>% html_text())))
##               {
##                "NO DESCRIPTION LISTED"
##               }
## 
##             else
##               {
##                 website %>% html_nodes("#JobDescContainer") %>% html_text()
##               }
##             )
## 
##         df1[i,]$employer2 <-
##           (if (identical(a,(website  %>% html_nodes(".padRtSm") %>% html_text())))
##             {
## 
##               "NO EMPLOYER LISTED"
##             }
## 
##            else
##             {
##               website  %>% html_nodes(".padRtSm") %>% html_text()
##             }
##            )
##   }
## 
##   saveRDS(df1, file="glassdoor_df1")
## 
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ## Finally, we will get data from acccessing the employers glassdoor page
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## 
## ####################################################################################################
## ##  creates employer urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
## ###################################################################################################
## 
##   unique_emp_ids<-unlist(unique(df1$employer_id[df1$employer_id!=0]))
## 
##   emp.urls <- paste0("https://www.glassdoor.com/Overview/Working-at-ID-Analytics-EI_IE",
##                      unique_emp_ids,".11,23.htm")
## 
##   df2<-data.frame(unique_emp_ids, emp.urls)
## 
## ####################################################################################################
## ##  gets empoyer description text
## ###################################################################################################
## 
##   n<-nrow(df2)
## 
##   df2$emp_desc_raw<-""
## 
##  # df2<-df2[-c(387),]
## 
##   for (i in 1:n)
##   {
## 
##   Sys.sleep(1)
## 
##     download.file(as.character(df2$emp.urls[i]), destfile = "scrapedpage.html", quiet=TRUE)
## 
##     website<- read_html("scrapedpage.html")
## 
##     df2[i,]$emp_desc_raw <-
##       (if (identical(a,(website %>% html_nodes("#EmpBasicInfo") %>% html_text())))
##       {
##         "NO DESCRIPTION LISTED"
##       }
## 
##       else
##       {
##         website %>% html_nodes("#EmpBasicInfo") %>% html_text()
##       }
##       )
## 
##   }
## 
##   saveRDS(df2, file="glassdoor_df2")
## 
## 
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ##  merge job and employer datasets
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## ####################################################################################################
## 
## colnames(df2)[1] <- "employer_id"
## 
## glassdoor_df<-merge(x = df1, y = df2, by = "employer_id", all.x = TRUE)
## 
## saveRDS(glassdoor_df, file="glassdoor_df")
## 
## 
## 
## 
## 
## ## This is the code i used to extract data from the raw job and employer description
## 
## 
## ##################################################################################################################
## ##################################################################################################################
## # Load glass door dataset
## ##################################################################################################################
## ##################################################################################################################
## 
## library(tidytext)
## library(vcd)
## library(xtable)
## 
## 
## data<-readRDS("glassdoor_df")
## 
## n<-nrow(data)
## 
## ##################################################################################################################
## ##################################################################################################################
## # Create structured fields from raw job descriptions
## ##################################################################################################################
## ##################################################################################################################
## 
## 
## bad_txt<-c("�", "-", ",", "/")
## data$job_desc1<-gsub(paste(bad_txt, collapse="|"), " ", data$job_desc_raw)
## data$job_desc2<-gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$job_desc1)
## data$job_desc2<-gsub("([[:upper:]][[:upper:]][[:upper:]])([[:upper:]][[:lower:]])", "\\1 \\2", data$job_desc2)
## data$job_desc3<-gsub("[(]", " ", data$job_desc2)
## data$job_desc3<-gsub("[)]", " ", data$job_desc3)
## 
## 
## bachelor<-c("\\bbachelor\\b", "\\bbachelors\\b", "\\bundergraduate\\b", "\\bbs\\b", "\\bb.s.\\b", "\\bb.s\\b")
## master<-c("\\bmaster\\b", "\\bmasters\\b", "graduate degree", "ms degree", "ms in", "m.s. in", "m.s in"  )
## phd<-c("phd", "doctorate", "\\bph\\b")
## mba<-c("m.b.a", "\\mba\\b")
## stats<-c("statistics", "statistical", "regression", "modelling")
## 
## 
## 
## 
## for (i in 1:n)
## {
## 
## data$python[i] <- any(grepl("\\bpython\\b", data$job_desc3[i], ignore.case=TRUE))
## data$ml[i] <- any(grepl("machine learning", data$job_desc3[i], ignore.case=TRUE))
## data$opt[i] <- any(grepl("optimization", data$job_desc3[i], ignore.case=TRUE))
## data$stats[i] <- any(grepl("statistic", data$job_desc3[i], ignore.case=TRUE))
## data$risk[i] <- any(grepl("risk", data$job_desc3[i], ignore.case=TRUE))
## data$UX[i] <- any(grepl("UX", data$job_desc3[i], ignore.case=FALSE))
## data$bd[i] <- any(grepl("big data", data$job_desc3[i], ignore.case=TRUE))
## data$dm[i] <- any(grepl("data management", data$job_desc3[i], ignore.case=TRUE))
## data$pharma[i] <- any(grepl("pharmaceutical", data$job_desc3[i], ignore.case=TRUE))
## data$fs[i] <- any(grepl("financial services", data$job_desc3[i], ignore.case=TRUE))
## data$sd[i] <- any(grepl("software development", data$job_desc3[i], ignore.case=TRUE))
## data$program[i] <- any(grepl("programming", data$job_desc3[i], ignore.case=TRUE))
## data$research[i] <- any(grepl("research", data$job_desc3[i], ignore.case=TRUE))
## data$R[i] <- any(grepl("\\bR\\b", data$job_desc3[i], ignore.case=TRUE))
## data$SAS[i] <- any(grepl("\\bSAS\\b", data$job_desc3[i], ignore.case=TRUE))
## data$C[i] <- any(grepl("\\bC+\\b", data$job_desc3[i], ignore.case=TRUE))
## data$stata[i] <- any(grepl("\\bstata\\b", data$job_desc3[i], ignore.case=TRUE))
## data$SQL[i] <- any(grepl("\\bsql\\b", data$job_desc3[i], ignore.case=TRUE))
## data$excel[i] <- any(grepl("\\bexcel\\b", data$job_desc3[i], ignore.case=TRUE))
## data$tableau[i] <- any(grepl("\\btableau\\b", data$job_desc3[i], ignore.case=TRUE))
## data$spss[i] <- any(grepl("\\bspss\\b", data$job_desc3[i], ignore.case=TRUE))
## data$java[i] <- any(grepl("\\bjava\\b", data$job_desc3[i], ignore.case=TRUE))
## data$linux[i] <- any(grepl("\\blinux\\b", data$job_desc3[i], ignore.case=TRUE))
## data$matlab[i] <- any(grepl("\\bmatlab\\b", data$job_desc3[i], ignore.case=TRUE))
## data$NLP[i] <- any(grepl("\\bNLP\\b", data$job_desc3[i], ignore.case=TRUE))
## data$hadoop[i] <- any(grepl("\\bhadoop\\b", data$job_desc3[i], ignore.case=TRUE))
## data$ruby[i] <- any(grepl("\\bruby\\b", data$job_desc3[i], ignore.case=TRUE))
## data$oracle[i] <- any(grepl("\\boracle\\b", data$job_desc3[i], ignore.case=TRUE))
## data$sas[i] <- any(grepl("\\bsas\\b", data$job_desc3[i], ignore.case=TRUE))
## data$bs[i] <- any(grepl(paste(bachelor, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
## data$bs2[i] <- any(grepl("bachelor", data$job_desc3[i], ignore.case=TRUE))
## data$masters[i] <- any(grepl(paste(master, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
## data$phd[i] <- any(grepl(paste(phd, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
## data$mba[i] <- any(grepl(paste(mba, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
## data$stats[i] <- any(grepl(paste(stats, collapse="|"), data$job_desc3[i], ignore.case=TRUE))
## data$degree[i] <- any(grepl("\\bdegree\\b", data$job_desc3[i], ignore.case=TRUE))
## 
## }
## 
## #data$skills<- data$degree+data$phd+data$masters+data$phd+data$bs2+data$bs+data$sas+data$oracle+data$ruby+data$hadoop+data$NLP+data$matlab+data$linux+
## #  data$java+ data$spss +data$spss +data$tableau+ data$excel+data$SQL+data$stata + data$C + data$SAS +data$R+ data$python
## 
## data$skills2<- data$sas+data$oracle+data$ruby+data$hadoop+data$NLP+data$matlab+data$linux+
##   data$java+ data$spss +data$spss +data$tableau+ data$excel+data$SQL+data$stata + data$C + data$SAS +data$R+
##   data$python+data$ml+data$opt+data$bd+data$research+data$stats+data$dm+data$risk+data$fs+data$program+data$pharma+data$sd++data$UX
## 
## data$education<- data$masters+data$phd+data$bs
## 
## 
## #table(data$skills2)
## #table(data$education)
## 
## #saveRDS(data, file="glassdoor_df_cleaned")
## 
## 
## #data$job_id[data$skills2==0]
## #strsplit(data$job_desc3[data$job_id=="2426165858"]," ")
## 
## ##################################################################################################################
## ##################################################################################################################
## # Create structured fields from raw employer descriptions
## ##################################################################################################################
## ##################################################################################################################
## 
## 
## data$emp_desc1<-gsub(paste(bad_txt, collapse="|"), " ", data$emp_desc_raw)
## data$emp_desc2<-gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$emp_desc1)
## data$emp_desc2<-gsub("([[:lower:]])([[:digit:]])", "\\1 \\2", data$emp_desc2)
## data$emp_desc2<-gsub("([[:digit:]])([[:upper:]])", "\\1 \\2", data$emp_desc2)
## 
##  sub(".*years experience *(.*?)", "\\1", data$job_desc3[1], ignore.case=TRUE)
## 
##  data$job_desc3[1]
## 
## for (i in 1:n)
## {
## 
##   data$founded[i] <- any(grepl("\\bfounded\\b", data$emp_desc2[i], ignore.case=TRUE))
##   data$industry[i] <- sub(".*Industry *(.*?) *Revenue.*", "\\1", data$emp_desc2[i], ignore.case=TRUE)
##   data$revenue[i] <- any(grepl("\\brevenue\\b", data$emp_desc2[i], ignore.case=TRUE))
## 
## 
## }
## 
## 
## 
## for (i in 1:n)
## {
## 
##   data$salary_low[i] <- sub(".*[$] *(.*?) *k[-].*", "\\1", data$salaries[i], ignore.case=TRUE)
##   data$salary_high[i] <- sub(".*[$] *(.*?) *k[-].*", "\\1", data$salaries[i], ignore.case=TRUE)
## 
## }
## 
## #strsplit(data$job_desc3[data$job_id=="2525091352"]," ")
## 
## data$salary_low[data$salary_low=="Not listed"]<-""
## data$salary_high[data$salary_high=="Not listed"]<-""
## 
## 
## data$salary_average<-as.numeric(data$salary_low)+as.numeric(data$salary_high)
## 
## hist(data$salary_average, breaks=seq(0, 400, 25))
## 
## 
## #summary(lm(data$salary_average~data$phd+data$masters+data$mba+data$bs))
## 
## #model1<-(lm(data$salary_average~ data$sas+data$oracle+data$ruby+data$hadoop+data$NLP+data$matlab+data$linux+
## #  data$java+ data$spss +data$spss +data$tableau+ data$excel+data$SQL+data$stata + data$C + data$SAS +data$R+
## #  data$python+data$ml+data$opt+data$bd+data$research+data$stats+data$dm+data$risk+data$fs+data$program+data$pharma+data$sd++data$UX))
## 
## 
## #hist(data$salary_average, breaks=seq(0, 400, 25), xlab="Glassdoor Estimated Salary", main="Distribution of Estimated Salary for \nData Scientist Positions in the New York City Area")
## 
## #summary(data$salary_average)
## 
## saveRDS(data, file="glassdoor_df_cleaned")
## 
## #data<-readRDS("glassdoor_df_cleaned")
## #data<-data[data$job_desc_raw!="NO DESCRIPTION LISTED",]
## #data<-data[(is.na(data$salary_average)==FALSE),]
## 
## #table(data$education)
## 
## #myvars <- c("phd", "masters", "bs")
## #educ_data <- data[myvars]
## 
## #data$phd2<- ifelse(educ_data$phd == FALSE, "", "PhD")
## #data$ms2<- ifelse(educ_data$masters == FALSE, "", "MS")
## #data$bs2<- ifelse(educ_data$bs == FALSE, "", "BS")
## 
## #data$educ_cat<-str_trim(paste(data$bs2, data$ms2, data$phd2, sep=" "))
## 
## #add bs and phd to bs ms and phd
## #data$educ_cat<-ifelse(data$educ_cat == "BS  PhD", "BS MS PhD" , data$educ_cat)
## 
## #add ms and phd to phd
## #data$educ_cat<-ifelse(data$educ_cat == "MS PhD", "PhD" , data$educ_cat)
## #educ_cleaned$educ_cat<-ifelse(educ_cleaned$educ_cat == "MS PhD", "PhD" , educ_cleaned$educ_cat)
## 
## #add bs and ms to ms
## #data$educ_cat<-ifelse(data$educ_cat == "BS MS", "MS" , data$educ_cat)
## #educ_cleaned$educ_cat<-ifelse(educ_cleaned$educ_cat == "BS MS", "MS" , educ_cleaned$educ_cat)
## 
## 
