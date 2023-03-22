library(lubridate)
library(tidyverse)
library(googlesheets4)

#campus
#df<-read.csv("I:\\My Data Sources\\classroom stuff\\Final.txt",sep=",",row.names = NULL,na.strings="NA")


getwd()
#df<-read.csv("./teachstats/Final.txt",sep=",",row.names = NULL,na.strings="")
#df<-read.csv("I:\\My Data Sources\\classroom stuff\\Final.txt",sep=",",row.names = T,na.strings="")

df <- range_read("https://docs.google.com/spreadsheets/d/1A5c3qo2oRGWPE3ZIZxJ78dWpiX8Xdehx1EThqlbrhQY/edit#gid=0",range='A1:H331')


df <- df %>%
  filter(prenp != 'neither',
         prenp != is.na(prenp)) %>%
  mutate(legacy_code_yr = gsub("(.)(.)(.)(.)",'\\1\\.\\2\\.\\3\\.\\4',legacy_code_yr)) %>%
separate(col = legacy_code_yr,
         into =c('decade','year_start','year_end','quart'),
         sep='[.]',remove = F) %>%
  mutate(decade = case_when(decade == "C" ~ 2,
                            decade == "B" ~ 1,
                            decade == "A" ~ 0)) %>%
  mutate(year=case_when(
    quart < 3 ~ paste0('20',decade,year_start),
    quart > 2 ~ paste0('20',decade,year_end)),
    month = case_when(
      quart == 1 ~ 07,
      quart == 2 ~ 10,
      quart == 3 ~ 01,
      quart == 4 ~ 04
  )) %>%
  mutate (Date = ymd(paste0(year,'-',month,'-','01'))) %>%
  mutate(quarter = factor(quarter),
         Sex = factor(tolower(gender)),
         Version =  as.factor(Version))

df %>%
  group_by(Sex, Version,prenp) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from =  prenp, values_from = n) %>%

  mutate(prop_yes = Yes/No) %>%

  ggplot(aes(x =Version, y=prop_yes))+
  geom_point()+geom_line()+
  facet_grid(~Sex)+
  ggtitle("Proportion of students saying yes to prenup/n a '1.0' proportion means equal between No and Yes.\n Bigger than 1 means many more Yes while Less than 1 means more no's")

  filter (quarter == 202014) %>% count(gender,prenp)




colnames(df)<-colnames(df)[2:6]
#df<-df[,1:5]
#df<-data.frame(na.omit(df,row.names=NULL))
df$name<-factor(df$name)
#df$quarter<-factor(df$quarter)
df$prenup<-factor(as.character(df$prenup))

str(df)
summary(df)


df<-df[complete.cases(df),]  #this remmoves students who didn't clearly indicate what to do

genderdf<-table(df$gender)

prenupdf<-table(df$prenup)
chisq.test(prenupdf)
as.data.frame(prenupdf) %>%
  rename(prenup=Var1) %>%
  ggplot(aes(x=prenup,y=Freq,group=1,label=Freq))+
  geom_point()+
  geom_line()+
  labs(title="Should a Prenup be signed?")+
  scale_y_continuous(limits = c(110, 135))+
  geom_text(aes(y = Freq + 2))


prenupVersiondf<-table(df$version,df$prenup)
chisq.test(prenupVersiondf)

as.data.frame(prenupVersiondf) %>%
  rename(version=Var1,prenup=Var2) %>%
  ggplot(aes(x=version,y=Freq,group=prenup,color=prenup,label=Freq)) +
  geom_point()+
  geom_line()+
  labs(title="Should a Prenup be signed, by version") +
  scale_y_continuous(limits = c(45, 80))+
  geom_text(aes(y = Freq + 2))

as.data.frame(prenupVersiondf) %>%
  rename(version=Var1,prenup=Var2) %>%
  ggplot(aes(x=prenup,y=Freq,label=Freq,group=version))+geom_line() +
  geom_point()+facet_wrap(~version)+
  scale_y_continuous(limits = c(45, 80))+
  geom_text(aes(y = Freq + 2))


genPrenupVerdf<-table(df$version,df$prenup,df$gender)
genderData<-as.data.frame(genPrenupVerdf) %>%
  rename(version=Var1,prenup=Var2,gender=Var3)

genderData.lm<-lm(Freq ~ version + prenup + gender,data=genderData)
summary(genderData.lm)
anova(genderData.lm)
confint(genderData.lm)

genderData.lm1 <- data.frame(Fitted = fitted(genderData.lm),
                       Residuals = resid(genderData.lm), gender = genderData$gender)


  ggplot(genderData.lm1, aes(Fitted, Residuals, colour = gender)) + geom_point()

  #https://www.r-bloggers.com/one-way-analysis-of-variance-anova/

as.data.frame(genPrenupVerdf) %>%
  rename(version=Var1,prenup=Var2,gender=Var3) %>%
  ggplot(aes(x=version,y=Freq,group=prenup,color=prenup,label=Freq)) +
  geom_point()+
  geom_line()+
  labs(title="Should a Prenup be signed, by version and gender") +
  scale_y_continuous(limits = c(0, 75))+
  geom_text(aes(y = Freq + 3))+
  facet_wrap(~gender)



#recent class#
RecentClassdf<-df %>% filter(quarter == '1673')
RecentGenPrenupVerdf<-table(RecentClassdf$version,RecentClassdf$prenup,RecentClassdf$gender)
as.data.frame(RecentGenPrenupVerdf) %>%
  rename(version=Var1,prenup=Var2,gender=Var3) %>%
  ggplot(aes(x=version,y=Freq,group=prenup,color=prenup,label=Freq)) +
  geom_point()+
  geom_line()+
  labs(title="Should a Prenup be signed, by version and gender") +
  scale_y_continuous(limits = c(0, 8))+
  geom_text(aes(y = Freq + .5))+
  facet_wrap(~gender)




plot1<-ggplot(data=df, aes(x=gender,fill=..x..>1))
plot1+coord_cartesian()+geom_bar()+facet_wrap(~version,ncol=2)+labs(title='count of male and female\n\nA=Male asks for Prenup\nB=Female asks for Prenup',x="Gender",y = "Count")+guides(fill=FALSE)

plot2<-ggplot(data=df, aes(x=prenup,fill=gender))
plot2+ geom_bar(stat="bin")+facet_wrap(~gender+version,ncol=4)+labs(title='count of male and female',x="Sign Prenup?",y = "Count")+guides(fill=FALSE)






#the following pulls the 3rd number from code in quarter to create month of start
for (i in 1:nrow(df)) {
  if (substr(df$quarter[i],3,3) == '1') {df$term[i]<-6}
  else if (substr(df$quarter[i],3,3) == '2') {df$term[i]<-9}
  else if (substr(df$quarter[i],3,3) == '3') {df$term[i]<-1}
  else if (substr(df$quarter[i],3,3) == '4') {df$term[i]<-4}
}
df$term<-as.integer(df$term)
##Below, this tests the course's actualy year, eg, 673 means 3rd term (winter) of 2006-7 year, which is actually in 2007.
for (i in 1:nrow(df)) {
  if (substr(df$quarter[i],1,1) == '5') {df$year[i]<-"1/1/2015"}
  else if(substr(df$quarter[i],3,3) <3) {df$year[i]<-paste0("1/1/200",substr(df$quarter[i],1,1))}
  else if(substr(df$quarter[i],3,3) > 2) {df$year[i]<-paste0("1/1/200",substr(df$quarter[i],2,2))}
}

df
###lubridate
#change term to month, year to year
df$term<-month(df$term,label = T)
df$year<-dmy(df$year)
df$year<-year(df$year)
df$Start_date<-mdy(paste0(df$term,"/1/",df$year))

qt<-df%>%select( Start_date,gender,version,prenup)%>%group_by(Start_date,version,gender,prenup)%>%summarise(count =n())

ts<-ggplot(data=qt,aes(x=Start_date,y=count,color=gender))
#ts+ geom_line()+facet_grid(~version+prenup)+geom_smooth()


qtDiff<-df%>%select(Start_date,gender,version,prenup)%>%group_by(Start_date,version,gender,prenup)%>%summarise(count=sum(prenup))

qtrCount<-df%>%select(Start_date)%>%group_by(Start_date)%>%summarise(count = n())

finaldf<-dcast(df,Start_date+version+gender~prenup)
finaldf<-mutate(finaldf,Diff=Yes-No)

finalPlot<-ggplot(data=finaldf,aes(x=Start_date,y=Diff,color=gender))
finalPlot+ geom_point()+ facet_wrap(~version+gender,nrow=1)+geom_smooth()

print(tbl_df(temp),n=44)

##########3
#Below is for just the recent group.

summerdf<-df[df$quarter == 561,]


plot (x=summerdf$version, y = as.factor(summerdf$prenup), main = "proportion of responses\n y = yes to prenup, \nn = no",xlab ="A = Male asks for prenup,\nB = Female asks for prenup")

plot (x=as.factor(summerdf$prenup), main = "count  of responses\n y = yes to prenup, \nn = no")

str(summerdf)

summary(summerdf)

gender<-table(summerdf$prenup)
prenupGender<-table(summerdf$version,summerdf$prenup)
genPrenupVer<-table(summerdf$version,summerdf$prenup,summerdf$gender)


t.test(as.numeric(as.factor(summerdf$prenup))~summerdf$version)

plot1<-ggplot(data=summerdf, aes(x=gender,fill=..x..>1))
plot1+coord_cartesian(ylim = c(0.9, 12))+geom_bar()+facet_wrap(~version,ncol=2)+labs(title='count of male and female',x="Gender",y = "Count")

plot2<-ggplot(data=summerdf, aes(x=prenup,fill=c(1)))
plot2+stat_count()+facet_wrap(~gender+version,ncol=4)+labs(title='count of male and female',x="Sign Prenup?",y = "Count")+guides(fill=FALSE)
