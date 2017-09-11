library(rvest)

library(XML)

# Store web url
url <- read_html("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14.htm")

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

glassdoor_a <- data.frame(job_id, employer,employer_id, title, min_salary, max_salary, med_salary)

glassdoor_a


glassdoor_b<-url %>%  html_nodes(xpath = '//*[@class="flexbox empLoc"]')%>%  html_text()

glassdoor_b

# Store web url

for (i in 2:907){
 
url <- read_html(print(paste("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP", i, ".htm", sep="")))

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

glassdoor_a
glassdoor_b

