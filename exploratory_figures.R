library(textmining)
library(wordcloud)
library(stringr)

data<-readRDS("glassdoor_df_cleaned")


data$location_state<-str_sub(data$location, -2, -1)

data$location_state[data$location_state=="es"]<-"Unknown"

state_cnt<-sort(table(data$location_state))


jpeg('location_state_barplot.jpg')

par(las=2)
barplot(state_cnt, main="Data Scientist Positions by State", 
        ylab="States", xlab="Count", horiz=TRUE)
dev.off()


R<-mean(data$R)
stata<-mean(data$stata)
sas<-mean(data$sas)
oracle<-mean(data$oracle)
ruby<-mean(data$ruby)
hadoop<-mean(data$hadoop)
matlab<-mean(data$matlab)
linux<-mean(data$linux)
java<-mean(data$java)
spss<-mean(data$spss)
tableau<-mean(data$tableau)
excel<-mean(data$excel)
SQL<-mean(data$SQL)
C<-mean(data$C)
python<-mean(data$python)


skills_sum<-c(R, stata, sas, oracle, ruby, hadoop, matlab, linux, java, spss, tableau, excel, SQL, C, python)
label<- c("R", "stata", "sas", "oracle", "ruby", "hadoop", "matlab", "linux", "java", "spss", "tableau", "excel", "SQL", "C", "python")

skills<-data.frame(skills_sum, label)


jpeg('skills_barplot.jpg')

par(las=2)
barplot(skills$skills_sum, main="Data Scientist Positions by Skills", 
        ylab="Skills", xlab="Count", horiz=TRUE, names.arg=c("R", "stata", "sas", "oracle", "ruby", "hadoop", "matlab", "linux", "java", "spss", "tableau", "excel", "SQL", "C", "python"))
dev.off()


phd<-mean(data$phd)
masters<-mean(data$masters)
bs<-mean(data$bs)

educ_sum<-c(phd, masters, bs)
label<-c("phd", "masters", "bs")

education<-data.frame(educ_sum, label)


jpeg('education_barplot.jpg')

par(las=2)
barplot(education$educ_sum, main="Data Scientist Positions by Education", 
        ylab="Education", xlab="Count", horiz=TRUE, names.arg=c( "phd", "masters", "bs"))
dev.off()
