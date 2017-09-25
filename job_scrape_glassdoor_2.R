##Code from lab from stephen
##remember to figure out how to cite correctly

###To-do
#figure out how to cite code
#figure out how to package libraries

###################################################################################################
###################################################################################################
##Load Packages
###################################################################################################
###################################################################################################
rm(list=ls(all=TRUE))
library(rvest)
library(stringr)

###################################################################################################
##  Urls for data scientist positions on glassdoor 
##  note for some reason glassdoor does not let you go past job post page 33?
###################################################################################################

url <- paste0("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP",1,".htm")
urls <- paste0("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP", 1:33, ".htm")


###################################################################################################
##  creates fields which are the individual job posts 
###################################################################################################

download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
fields <-read_html("scrapedpage.html") %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "jl", " " ))]')

###################################################################################################
##  creates job title which are the individual job posts 
###################################################################################################
title <- fields %>% html_nodes(".flexbox .jobLink") %>% html_text() %>% trimws()

###################################################################################################
## gets salaries and replaces not listed when salary information is missing
###################################################################################################
salaries <- sapply(fields, function(x) {
  tmp <- html_nodes(x, ".small") %>% html_text()
  ifelse(length(tmp) == 0, "Not listed", trimws(tmp))
})
salaries <- trimws(gsub("(Glassdoor est.)", "", salaries))

####################################################################################################
##  gets job location 
###################################################################################################

locations <- fields %>% html_nodes(".loc") %>% html_text() %>% trimws()


####################################################################################################
##  gets emplyer, employer id and job id 
###################################################################################################

##can;t figure out how to get gsub or strreplace to recognize the - to remove the city name
employer <- fields %>% html_nodes(".empLoc") %>% html_text() %>% trimws()

employer_id<-fields %>% html_attr("data-emp-id")

job_id<-fields %>% html_attr("data-id")

####################################################################################################
##  creates job urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
###################################################################################################
job.urls <- paste0("https://www.glassdoor.com/job-listing/data-scientist-emc-research-JV_IC1145845_KO0,14_KE15,27.htm?jl=",
                   job_id)

####################################################################################################
## gets the rae job description, days posted, and employed from the job posts
###################################################################################################

more_job_facts <- sapply(job.urls, function(x) {
  Sys.sleep(5)
  download.file(x, destfile = "scrapedpage.html", quiet=TRUE)
  website<- read_html("scrapedpage.html")
  job_desc_raw <- website %>% html_nodes("#JobDescContainer") %>% html_text()
  employer2 <- if ((website  %>% html_nodes(".padRtSm") %>% html_text())==""){
    
    "no employer listed"
  } else{
    
    website  %>% html_nodes(".padRtSm") %>% html_text()
  }
    
  c(job_desc_raw, employer2)
})

more_job_facts <- unname(more_job_facts)

job_desc_raw<-more_job_facts[1,]
employer2<-more_job_facts[2,]


####################################################################################################
##  creates employer urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
###################################################################################################

emp.urls <- paste0("https://www.glassdoor.com/Overview/Working-at-ID-Analytics-EI_IE",
                   employer_id,".11,23.htm")

more_job_facts2 <- sapply(emp.urls, function(x) {
  Sys.sleep(5)
  download.file(x, destfile = "scrapedpage.html", quiet=TRUE)
  
  website<- read_html("scrapedpage.html")
  
  
  emp_desc_raw <- website %>% html_nodes("#EmpBasicInfo") %>% html_text()

})
more_job_facts2 <- unname(more_job_facts2)

df<-cbind(job_id, employer_id, title, salaries, locations, job_desc_raw, more_job_facts2, employer, employer2)


####################################################################################################
####################################################################################################
####################################################################################################
## First we will get data from summary data for each post from the search result urls
####################################################################################################
####################################################################################################
####################################################################################################


###################################################################################################
##  creates fields which are the individual job posts from glassdoor
###################################################################################################
for (i in 1:33){
  paste(i)
  download.file(urls[i], destfile = "scrapedpage.html", quiet=TRUE)
  fields <-read_html("scrapedpage.html") %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "jl", " " ))]')
  
  ###################################################################################################
  ##  creates job title which are the individual job posts 
  ###################################################################################################
  title <- fields %>% html_nodes(".flexbox .jobLink") %>% html_text() %>% trimws()
  
  ###################################################################################################
  ## gets salaries and replaces not listed when salary information is missing
  ###################################################################################################
  salaries <- sapply(fields, function(x) {
    tmp <- html_nodes(x, ".small") %>% html_text()
    ifelse(length(tmp) == 0, "Not listed", trimws(tmp))
  })
  salaries <- trimws(gsub("(Glassdoor est.)", "", salaries))
  
  ####################################################################################################
  ##  gets job location 
  ###################################################################################################
  
  locations <- fields %>% html_nodes(".loc") %>% html_text() %>% trimws()
  
  
  ####################################################################################################
  ##  gets emplyer, employer id and job id 
  ###################################################################################################
  
  ##can;t figure out how to get gsub or strreplace to recognize the - to remove the city name
  employer <- fields %>% html_nodes(".empLoc") %>% html_text() %>% trimws()
  
  employer_id<-fields %>% html_attr("data-emp-id")
  
  job_id<-fields %>% html_attr("data-id")
  
  
  ####################################################################################################
  ##  create and append data frame with job posts
  ###################################################################################################
  df1a<-cbind(job_id, employer_id, title, salaries, locations, employer)
  
  df1<-rbind(df1,df1a)
}
 
####################################################################################################
####################################################################################################
####################################################################################################
## Now we will get data from accessing the websites for the individual job posts using
####################################################################################################
####################################################################################################
####################################################################################################


####################################################################################################
##  creates job urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
###################################################################################################

 df1$job.urls <- paste0("https://www.glassdoor.com/job-listing/data-scientist-emc-research-JV_IC1145845_KO0,14_KE15,27.htm?jl=",
                     df1$job_id)
  
####################################################################################################
## gets the raw job description, days posted, and employed from the job posts
###################################################################################################
  
  df1$job_desc_raw<-""
  df1$employer2<-""
#used to compare null results in loop  
  a <- character(0)

  for (i in 493:1050)
    {

    Sys.sleep(2)
    
      download.file(df1[i,]$job.urls, destfile = "scrapedpage.html", quiet=TRUE)
        
        website<- read_html("scrapedpage.html")

        df1[i,]$job_desc_raw <- 
            (if (identical(a,(website %>% html_nodes("#JobDescContainer") %>% html_text())))
              {
               "NO DESCRIPTION LISTED"
              } 
            
            else
              {
                website %>% html_nodes("#JobDescContainer") %>% html_text()
              }
            )        
    
        df1[i,]$employer2 <- 
          (if (identical(a,(website  %>% html_nodes(".padRtSm") %>% html_text())))
            {
      
              "NO EMPLOYER LISTED"
            } 
           
           else
            {
              website  %>% html_nodes(".padRtSm") %>% html_text()
            }
           )
  }
  
####################################################################################################
####################################################################################################
####################################################################################################
## Finally, we will get data from acccessing the employers glassdoor page
####################################################################################################
####################################################################################################
####################################################################################################

####################################################################################################
##  creates employer urls from job id, note if this part breaks, just replace the first part of the past 0 with another link
###################################################################################################
  
  df1$emp.urls <- paste0("https://www.glassdoor.com/Overview/Working-at-ID-Analytics-EI_IE",
                     df1$employer_id,".11,23.htm")
  

####################################################################################################
##  gets empoyer description text
###################################################################################################
  
  unique_emp_urls<-unlist(unique(df1$emp.urls[df1$employer_id!=0]))
  df2<-data.frame(unique_emp_urls)

  n<-nrow(df2)

  df2$emp_desc_raw<-""

 # df2<-df2[-c(387),]  
      
  for (i in 540:n)
  {
    
   Sys.sleep(2)
    
    download.file(as.character(df2$unique_emp_urls[i]), destfile = "scrapedpage.html", quiet=TRUE)
    
    website<- read_html("scrapedpage.html")
    
    df2[i,]$emp_desc_raw <- 
      (if (identical(a,(website %>% html_nodes("#EmpBasicInfo") %>% html_text())))
      {
        "NO DESCRIPTION LISTED"
      } 
      
      else
      {
        website %>% html_nodes("#EmpBasicInfo") %>% html_text()
      }
      )        
    
  }
  
  length(unique(df1$emp.urls))
  
  to_ch
 
  df2[1,]$unique_emp_urls
  
  download.file(as.character(df2$unique_emp_urls[1]), destfile = "scrapedpage.html", quiet=TRUE)
  
  download.file(df1[i,]$job.urls, destfile = "scrapedpage.html", quiet=TRUE)
  
  df2$unique_emp_urls[387]
  as.character(df2$unique_emp_urls[1])
  df1[2,]$job.urls
  
  download.file(as.character(df2$unique_emp_urls[i]), destfile = "scrapedpage.html", quiet=TRUE)
  
  website<- read_html("scrapedpage.html")
  
  download.file(as.character(df2$unique_emp_urls[1]), destfile = "scrapedpage.html", quiet=TRUE)
  
  website<- read_html("scrapedpage.html")
  
  df2$emp_desc_raw[1] <- 
    (if (identical(a,(website %>% html_nodes("#EmpBasicInfo") %>% html_text())))
    {
      "NO DESCRIPTION LISTED"
    } 
    
    else
    {
      website %>% html_nodes("#EmpBasicInfo") %>% html_text()
    }
    )