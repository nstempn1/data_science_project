###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#THIS IS OLD CODE BEFORE I FIGURED OUT WHAT I WAS DOING
###################################################################################
###################################################################################
###################################################################################
###################################################################################

library(rvest)

library(XML)

# Store web url
url <- read_html("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14.htm")

#get location of position
location<-url %>%  html_nodes(".empLoc .loc") %>% html_text()
location

#get employer of position
employer<-url %>%  html_nodes("i")%>% html_attr("data-employer-shortname")
employer <- employer[!is.na(employer)]

#get employer id of position
#I want to use this to get more data on employers
employer_id<-url %>%  html_nodes("i")%>% html_attr("data-employer-id")
employer_id <- employer_id[!is.na(employer_id)]

#get id of position
#I want to try and use this to ge more information about job posts
job_id<-url %>%  html_nodes("i")%>% html_attr("data-job-id")
job_id <- job_id[!is.na(job_id)]

#get title of position
title<-url %>%  html_nodes("i")%>% html_attr("data-jobtitle")
title <- title[!is.na(title)]

#get minimum salary estimate of position
min_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-min-salary")
min_salary <- min_salary[!is.na(min_salary)]

#get maximum salary estimate of position
max_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-max-salary")
max_salary <- max_salary[!is.na(max_salary)]

#get median salary estimate of position
med_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-med-salary")
med_salary <- med_salary[!is.na(med_salary)]

glassdoor_a <- data.frame(job_id, employer,employer_id, title, min_salary, max_salary, med_salary)

glassdoor_a


glassdoor_b<-url %>%  html_nodes(xpath = '//*[@class="flexbox empLoc"]')%>%  html_text()

glassdoor_b


#the code above only gets information for the first page of results
#the code below is a loop for the remaining pages

for (i in 2:907){
 
url <- read_html(print(paste("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP", i, ".htm", sep="")))
Sys.sleep(5)
location<-url %>%  html_nodes(".empLoc .loc") %>% html_text()
location

employer<-url %>%  html_nodes("i")%>% html_attr("data-employer-shortname")
employer <- employer[!is.na(employer)]


employer_id<-url %>%  html_nodes("i")%>% html_attr("data-employer-id")
employer_id <- employer_id[!is.na(employer_id)]

job_id<-url %>%  html_nodes("i")%>% html_attr("data-job-id")
job_id <- job_id[!is.na(job_id)]

title<-url %>%  html_nodes("i")%>% html_attr("data-jobtitle")
title <- title[!is.na(title)]

min_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-min-salary")
min_salary <- min_salary[!is.na(min_salary)]

max_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-max-salary")
max_salary <- max_salary[!is.na(max_salary)]

med_salary<-url %>%  html_nodes("i")%>% html_attr("data-displayed-med-salary")
med_salary <- med_salary[!is.na(med_salary)]

glassdoor_a <- rbind(glassdoor_a, data.frame(job_id, employer,employer_id, title, min_salary, max_salary, med_salary))

partb<-url %>%  html_nodes(xpath = '//*[@class="flexbox empLoc"]')%>%  html_text()
glassdoor_b<- c(glassdoor_b, partb)


}

##########################################################################################
##########################################################################################
##########################################################################################
#Get more data from the actual job postings on glassdoor using Job_IDS
##########################################################################################
##########################################################################################
##########################################################################################

url <- read_html("https://www.glassdoor.com/job-listing/data-scientist-novetta-JV_IC1130374_KO0,14_KE15,22.htm?jl=2368633574")

url %>%  html_nodes(".empLoc .loc") %>% html_text()

