foreign<-read.csv("산업별_외국인_투자_유치_실적_20230406153001.csv",fileEncoding = "CP949",header=F)
foreign_tot <- foreign[c(1,3),c(-1,-2)]
foreign_tot <- t(foreign_tot)
foreign_tot<-foreign_tot[c(F,T),]
(foreign_tot<-data.frame(foreign_tot))
foreign1<-as.Date(foreign_tot[,1],"%Y.%m/%d")
foreign2<-as.numeric(foreign_tot[,2])
foreign_tot<-data.frame(foreign1,foreign2)



foreign_sub <- foreign[c(1,3,4,8,20,31),c(-1,-2)]
foreign_sub <- t(foreign_sub)
foreign_sub<-foreign_sub[c(F,T),]
(foreign_sub<-data.frame(foreign_sub))
foreign1<-as.Date(foreign_sub[,1],"%Y.%m/%d")
foreign2 <- sapply(foreign_sub[,2:6], as.numeric)
foreign_sub<-data.frame(foreign1,foreign2)
colnames(foreign_sub)<-c("Q","total","primary","manufacture","service","management")
foreign_sub
str(foreign_sub)
#install.packages("reshape2")
library(reshape2)
#install.packages("lubridate")
library(lubridate)
year<-c(2000:2022)
foreign_sub_year<-data.frame(year)

foreign_sub_year$total <-dcast(foreign_sub, year(foreign_sub[,1]) ~ ., value.var = "total", sum)[,2]
foreign_sub_year$primary <-dcast(foreign_sub, year(foreign_sub[,1]) ~ ., value.var = "primary", sum)[,2]
foreign_sub_year$manufacture <-dcast(foreign_sub, year(foreign_sub[,1]) ~ ., value.var = "manufacture", sum)[,2]
foreign_sub_year$service <-dcast(foreign_sub, year(foreign_sub[,1]) ~ ., value.var = "service", sum)[,2]
foreign_sub_year$management <-dcast(foreign_sub, year(foreign_sub[,1]) ~ ., value.var = "management", sum)[,2]
foreign_sub_year



library(reshape2)
year <- c(2000:2022)
saving_invest_year <- data.frame(saving_invest)
saving_invest_year[,-1] <- sapply(saving_invest_year[,-1], as.numeric)
str(saving_invest_year)
saving_invest

saving_invest_year<-data.frame(year)
saving_invest_year$saving<-dcast(saving_invest, year(saving_invest[,1]) ~ ., value.var = "saving", sum)[,2]
saving_invest_year$investment<-dcast(saving_invest, year(saving_invest[,1]) ~ ., value.var = "investment", sum)[,2]
saving_invest_year$GNDI<-dcast(saving_invest, year(saving_invest[,1]) ~ ., value.var = "GNDI", sum)[,2]
saving_invest_year



library(dplyr)

# year를 기준으로 그룹화하여 각 연도별로 열별로 합산



# 결과 확인
print(df_summarised)





customer<-read.csv("소비자물가지수_2020100__20230406152954.csv",fileEncoding = "CP949",header=F)
customer<-customer[c(1,2),-1]
(customer<-t(customer))
class(customer)
dim(customer)
Q<-as.Date(customer[,1],"%Y.%m/%d")
customer2<-as.numeric(customer[,2])
customer<-data.frame(Q,customer2)
customer<-customer[c(-92,-93),]

marriage_Title <- "시도_시군구_월별_혼인_20230406153012.csv"

#install.packages("stringr")
library(stringr)
#install.packages("lubridate")
library(lubridate)

marriage <- read.csv(marriage_Title, fileEncoding="CP949",header=F,colClasses=rep("character",277))
marriage <- marriage[c(1,2),]

marriage<-marriage[c(1,2),-1]
marriage<-as.data.frame(t(marriage))
marriage[,1]<-str_trim(marriage[,1], side='right')
marriage[,1] <- paste0(marriage[,1], "/04")
marriage[,1] <- as.Date(marriage[,1],"%Y.%m/%d")
marriage[,2] <- as.numeric(marriage[,2])

Q_date <- matrix(0, nrow = trunc(nrow(marriage)/3))
Q_data <- matrix(0, nrow = trunc(nrow(marriage)/3))

#for (i in 1:trunc(length(marriage[,1])/3) {
for (i in 1:trunc(nrow(marriage)/3)) {
  i3 <- (i-1) *3
  add_data <- sum(marriage[i3+1,2],marriage[i3+2,2],marriage[i3+3,2])
  date_str <- paste0(year(marriage[i3+1,1]), ".",((i-1)%%4)+1, "/4")
  Q_date[i] <- date_str
  Q_data[i] <- add_data
}
marriageq <- data.frame(Q = as.Date(Q_date,"%Y.%m/%d"),marriage_cnt = Q_data)
marriageq
marriage<-marriageq[-92,]

saving_invest<-read.csv("총저축과_총투자_원계열__명목__분기_및_연간__20230407143912.csv",fileEncoding = "CP949",header=F)
saving_invest<-as.data.frame(t(saving_invest[c(1,2,9,17),-1]))
saving_invest[,1]<-as.Date(saving_invest[,1],"%Y.%m/%d")
colnames(saving_invest)<-c("Q","saving","investment","GNDI")
saving_invest

econ_age <- read.csv("연령별_경제활동인구_총괄_20230407142148.csv", fileEncoding = "CP949", header = F)
econ_age <- t(econ_age[c(1,3),-1])
econ_age <- econ_age[c(F,F,T,T,F,F,F,F),]
(econ_age <- data.frame(econ_age))
econ_age1 <- as.Date(econ_age[c(T,F),1], "%Y.%m/%d")
econ_age2 <- econ_age[c(T,F),-1]
econ_age3 <- econ_age[c(F,T),-1]
econ_age <- data.frame(econ_age1, econ_age2, econ_age3)  
colnames(econ_age)<-c("Q","취업자","실업자")
econ_age
# gdp<-read.csv("분기별_경제성장률.csv",fileEncoding = "CP949",header=F)
# gdp<-t(gdp[,-1])
# gdp1 <- as.Date(gdp[,1],"%Y.%m/%d")
# gdp2 <- as.numeric(gdp[,2])
# gdp<-data.frame(gdp1,gdp2)

# 2009~2019
per_temp_work<-read.csv("성_종사자규모별_근로형태_비정규직_별_취업자_20230407142817.csv",fileEncoding="CP949",header=F)
per_temp_work<-per_temp_work[c(1,3),c(-1,-2)]
year<-t(per_temp_work[1,c(F,T,F,F,F,F,F)])
per<-t(per_temp_work[2,c(F,T,F,F,F,F,F)])
temp<-t(per_temp_work[2,c(F,F,T,F,F,F,F)])
per_temp_work<-data.frame(year,per,temp)
colnames(per_temp_work)<-c("year","per","temp")
per_temp_work$year<-2009:2019
per_temp_work


# 2009~2019
work_time<-read.csv("산업별_임금_및_근로시간__표준산업분류9차__20230406164418.csv",fileEncoding = "CP949",header=F)
work_time<-as.data.frame(t(work_time[c(1,3),c(-1,-2)]))
work_time[,1] <- as.Date(work_time[,1],"%Y")
colnames(work_time)<-c("year","time")
work_time

#2009~2019
wage<-read.csv("임금결정률_20230407143807.csv",fileEncoding = "CP949",header=F)
wage<-as.data.frame(t(wage[c(1,3,4),c(-1)]))
wage[,1] <- as.Date(paste0(paste0(wage[,1], ".01"),".01"), "%Y.%m.%d")
colnames(wage)<-c("year","private","public")
wage

## 2000 1/4~2022 3/4
foreign_sub
customer
marriage
econ_age

saving_invest




# 외국인투자 0년후 
rm(Q_dataset)
Q_dataset <- merge(foreign_sub,customer,by="Q")
Q_dataset <- merge(Q_dataset,marriage,by="Q")
Q_dataset <- merge(Q_dataset,econ_age,by="Q")
Q_dataset <- merge(Q_dataset,saving_invest,by="Q")
Q_dataset
Q_dataset[,-1] <- sapply(Q_dataset[,-1], as.numeric)
## 2009~2019 연간
foreign_sub_year[10:20,]

#-------------------------
saving_invest

per_temp_work
work_time
wage

per_temp_work$year<-2009:2019
work_time$year <- as.character(year(work_time$year))
wage$year <- as.character(year(wage$year))
rm(Y_dataset)

Y_dataset <- merge(foreign_sub_year[10:20,],per_temp_work,by="year")
Y_dataset<-merge(Y_dataset,work_time,by="year")
Y_dataset<-merge(Y_dataset,wage,by="year")
Y_dataset
Y_dataset[,-1] <- sapply(Y_dataset[,-1], as.numeric)



###################
# 외국인투자의 1년후 
foreign_sub_1<-foreign_sub
foreign_sub_1$Q<-foreign_sub$Q+years(1)
rm(Q_dataset_1)
Q_dataset_1 <- merge(foreign_sub_1,customer,by="Q")
Q_dataset_1 <- merge(Q_dataset_1,marriage,by="Q")
Q_dataset_1 <- merge(Q_dataset_1,econ_age,by="Q")
Q_dataset_1 <- merge(Q_dataset_1,saving_invest,by="Q")
Q_dataset_1[,-1] <- sapply(Q_dataset_1[,-1], as.numeric)
Q_dataset_1


foreign_sub_year_1<-foreign_sub_year
foreign_sub_year_1$year<-foreign_sub_year_1$year+1
rm(Y_dataset_1)
Y_dataset_1 <- merge(foreign_sub_year_1[10:20,],per_temp_work,by="year")
Y_dataset_1<-merge(Y_dataset_1,work_time,by="year")
Y_dataset_1<-merge(Y_dataset_1,wage,by="year")
Y_dataset_1[,-1] <- sapply(Y_dataset_1[,-1], as.numeric)
Y_dataset_1

# 외국인투자의 2년후 
foreign_sub_2<-foreign_sub
foreign_sub_2$Q<-foreign_sub$Q+years(2)
rm(Q_dataset_2)
Q_dataset_2 <- merge(foreign_sub_2,customer,by="Q")
Q_dataset_2 <- merge(Q_dataset_2,marriage,by="Q")
Q_dataset_2 <- merge(Q_dataset_2,econ_age,by="Q")
Q_dataset_2 <- merge(Q_dataset_2,saving_invest,by="Q")
Q_dataset_2[,-1] <- sapply(Q_dataset_2[,-1], as.numeric)
Q_dataset_2

foreign_sub_year_2<-foreign_sub_year
foreign_sub_year_2$year<-foreign_sub_year_2$year+2
rm(Y_dataset_2)
Y_dataset_2 <- merge(foreign_sub_year_2[10:20,],per_temp_work,by="year")
Y_dataset_2<-merge(Y_dataset_2,work_time,by="year")
Y_dataset_2<-merge(Y_dataset_2,wage,by="year")
Y_dataset_2[,-1] <- sapply(Y_dataset_2[,-1], as.numeric)
Y_dataset_2

# 외국인투자의 3년후 
foreign_sub_3<-foreign_sub
foreign_sub_3$Q<-foreign_sub$Q+years(3)
rm(Q_dataset_3)
Q_dataset_3 <- merge(foreign_sub_3,customer,by="Q")
Q_dataset_3 <- merge(Q_dataset_3,marriage,by="Q")
Q_dataset_3 <- merge(Q_dataset_3,econ_age,by="Q")
Q_dataset_3 <- merge(Q_dataset_3,saving_invest,by="Q")
Q_dataset_3[,-1] <- sapply(Q_dataset_3[,-1], as.numeric)
Q_dataset_3

foreign_sub_year_3<-foreign_sub_year
foreign_sub_year_3$year<-foreign_sub_year_3$year+3
rm(Y_dataset_3)
Y_dataset_3 <- merge(foreign_sub_year_3[10:20,],per_temp_work,by="year")
Y_dataset_3<-merge(Y_dataset_3,work_time,by="year")
Y_dataset_3<-merge(Y_dataset_3,wage,by="year")
Y_dataset_3[,-1] <- sapply(Y_dataset_3[,-1], as.numeric)
Y_dataset_3


per_temp_work
work_time
wage
rm(Y2_dataset)
Y2_dataset<-merge(saving_invest_year,per_temp_work,by="year")
Y2_dataset<-merge(Y2_dataset,work_time,by="year")
Y2_dataset<-merge(Y2_dataset,wage,by="year")
Y2_dataset[,-1] <- sapply(Y2_dataset[,-1], as.numeric)
Y2_dataset

Q_dataset_sc<-data.frame(scale(Q_dataset[,-1]))
Q_dataset_sc_1<-data.frame(scale(Q_dataset_1[,-1]))
Q_dataset_sc_2<-data.frame(scale(Q_dataset_2[,-1]))
Q_dataset_sc_3<-data.frame(scale(Q_dataset_3[,-1]))
Y_dataset_sc<-data.frame(scale(Y_dataset[,-1]))
Y_dataset_sc_1<-data.frame(scale(Y_dataset_1[,-1]))
Y_dataset_sc_2<-data.frame(scale(Y_dataset_2[,-1]))
Y_dataset_sc_3<-data.frame(scale(Y_dataset_3[,-1]))

Y2_dataset_sc<-data.frame(scale(Y2_dataset[,-1]))
