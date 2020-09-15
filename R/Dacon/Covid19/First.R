#성별에 따른 Patient 비율
Patient <- PatientInfo
Patient <- Patient[!(Patient$sex == "" ), ]
Patient_Sex <- Patient %>%  group_by(sex) %>%  summarize(Count = n())
Patient_Sex
library(ggplot2)
ggplot(Patient_Sex, aes(sex, Count, fill = sex)) + geom_bar(position="dodge", stat = "identity")

#감염 원인에 따른 Patient 비율
head(Patient)
unique(Patient$infection_case)
Patient <- Patient[!(Patient$infection_case ==""),]
unique(Patient$infection_case)
Infection_case <- Patient %>%  group_by(infection_case) %>% summarize(Count = n())
ggplot(Infection_case, aes(reorder(infection_case, -Count), Count, fill = infection_case)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x=element_text(angle=90, hjust=1))
Infection_Month <- Patient$confirmed_date
Infection_Month <- substr(Infection_Month, 1,7)
head(Infection_Month)
Patient <- cbind(Patient, Infection_Month)
Infection_Jan <- subset(Patient, Patient$Infection_Month == "2020-01")
Infection_Feb <- subset(Patient, Patient$Infection_Month == "2020-02")
Infection_Mar <- subset(Patient, Patient$Infection_Month == "2020-03")
Jan_Case <- Infection_Jan %>%  group_by(infection_case) %>%  summarize(Count = n())
ggplot(Jan_Case, aes(reorder(infection_case, -Count), Count, fill = infection_case)) + geom_bar(position = "dodge", stat = "identity")+ theme(axis.text.x=element_text(angle =90, hjust = 1))
Feb_Case <- Infection_Feb %>%  group_by(infection_case) %>%  summarize(Count = n())
Feb_Case<- Feb_Case[!(Feb_Case$infection_case == ""),]
Mar_Case <- Infection_Mar %>%  group_by(infection_case) %>%  summarize(Count = n())
Mar_Case <- Mar_Case[!(Mar_Case$infection_case ==""),]

#평균 접촉자 수
head(Patient)
A <- c((mean(Infection_Jan$contact_number, na.rm= T)), (mean(Infection_Feb$contact_number, na.rm= T)), (mean(Infection_Mar$contact_number, na.rm = T)), (mean(Patient$contact_number, na.rm =T)))
E <- c("Jan", "Feb", "Mar", "Total")
Mean_Contact <- data.frame(E,A)
colnames(Mean_Contact) <- c("Month", "Mean")
Mean_Contact
ggplot(Mean_Contact, aes(reorder(Month, -Mean), Mean, fill = Month)) + geom_bar(position = "dodge", stat = "identity")

#확진부터 퇴원까지 평균 기간
Date <- data.frame(Patient$confirmed_date, Patient$released_date)
Date$Patient.confirmed_date <- as.Date(Date$Patient.confirmed_date)
Date$Patient.released_date <- as.Date(Date$Patient.released_date)
Periods <- Date$Patient.released_date - Date$Patient.confirmed_date
Date <- cbind(Date, Periods)
Date$Periods <- as.numeric(as.factor(Date$Periods))
Duration2 <- Date %>%  group_by(Periods) %>%  summarize(Count = n())
Duration2 <- na.omit(Duration2)
Duration2$Periods <- as.character(Duration2$Periods)
library(tidyverse)
library(ggplot2)
ggplot(Duration2, aes(reorder(Periods, -Count), Count, fill = Periods)) + geom_bar(stat = "identity", position = "dodge")
Duration2$Periods <- as.numeric(Duration2$Periods)
mean(Duration2$Periods)
Duration2$Periods <- as.character(Duration2$Periods)

#SearchTrend와 확진자수의 관계
head(SearchTrend)
head(Time)
SearchTrend_2020 <- subset(SearchTrend, as.Date(date)>=as.Date('2020-01-20'))
head(SearchTrend_2020)
SearchTrend_2020$date
Time_SearchTrend <- subset(Time, as.Date(date)>=as.Date('2020-01-20') & as.Date(date) <= as.Date('2020-03-16'))
Time_SearchTrend <- cbind(Time_SearchTrend, SearchTrend_2020$coronavirus)
Time_SearchTrend$date <- substr(Time_SearchTrend$date, 6,10)
head(Time_SearchTrend)
Confirmed_Time <- ggplot(Time_SearchTrend, aes(date, confirmed, group= 1)) + geom_line(color = "blue") + theme(axis.text.x = element_text(angle = 60, hjust =1))
Confirmed_Time + geom_line(aes(date, SearchTrend_2020$coronavirus*100, group = 1), color = "red")  
head(Time_SearchTrend)
str(Time_SearchTrend)                                                                                                                       
names(Time_SearchTrend)[names(Time_SearchTrend) == 'SearchTrend_2020$coronavirus'] <- c("Trend")
Test <- lm(confirmed~Trend, data = Time_SearchTrend)
plot(Test)
summary(Test)
Test2 <- lm(Trend~confirmed, data = Time_SearchTrend)
plot(Test2)
summary(Test2)

#고령자 비율

head(Region)
Elderly <- Region %>% group_by(city,elderly_population_ratio, elderly_alone_ratio) %>% summarize()
Elderly
Nursing_home <- Region %>%  group_by(city, nursing_home_count) %>%  summarize()
Nursing_home
head(PatientInfo)
Patient_City <- PatientInfo %>%  group_by(sex, age, city)
head(Patient_City)
cityofpatient <- Patient_City %>% group_by(city) %>%  summarize(count=n())
head(cityofpatient)
sum(is.na(cityofpatient))
cityofpatient <- cityofpatient[!(cityofpatient$city == ""),]
sum(is.na(cityofpatient))
unique(cityofpatient$city)
cityofpatient <- na.omit(cityofpatient)
head(Elderly)
head(cityofpatient)
Elderly <- merge(Elderly, cityofpatient, by = 'city')
head(Elderly)
lm(elderly_population_ratio~ count , data = Elderly)
summary(lm(count~elderly_population_ratio, data = Elderly))
plot(lm(count~elderly_population_ratio, data = Elderly))
ggplot(data=Elderly, aes(x=count, y=elderly_population_ratio, colour=city)) + geom_point()

Patient_Province <- PatientInfo %>% group_by(province,age) %>%  summarize(count = n())
head(Patient_Province)
sum(is.na(Patient_Province))
Region_Province <- Region %>%  group_by(province, elderly_population_ratio, elderly_alone_ratio) %>%  summarize()
head(Region_Province)
sum(is.na(Region_Province))
Patient_Province_Elderly <- Patient_Province %>%  filter(age == '60s' | age == "70s" | age == "80s" | age == "90s" | age == "100s")
head(Patient_Province_Elderly)
Mean_Elderly <- Region_Province %>% group_by(province) %>%  summarize(Mean_Eldelry = mean(elderly_population_ratio), Mean_alone = mean(elderly_alone_ratio))
Patient_Province_Elderly <- Patient_Province_Elderly %>%  group_by(province) %>%  summarize(count = n())
head(Patient_Province_Elderly)
str(Patient_Province_Elderly)
library(dplyr)
Patient_Province_Elderly <- as.data.frame(Patient_Province_Elderly)
head(Patient_Province_Elderly)
Patient_Province_Elderly <- merge(Patient_Province_Elderly, Mean_Elderly, by= 'province')
ggplot(Patient_Province_Elderly, aes(province, count, color = province)) + geom_point()
library(tidyverse)
library(ggplot2)
head(Time)
head(TimeGender)
head(Time)
head(Time_SearchTrend)
head(Case)
head(PatientInfo)
head(TimeProvince)
head(PatientRoute)
str(PatientRoute)
head(TimeGender)
head(Case)
Daegu <- Case %>%  subset(province == "Daegu")
head(Daegu)
str(Daegu)
Daegu
head(PatientInfo)
str(PatientInfo)
Daegu <- PatientInfo %>% subset(province == "Daegu")
str(Daegu)


#확진자 이동 경로 지도에 뿌리기
library(ggmap)
library(ggplot2)
library(qmap)
register_google(key= 'AIzaSyAMIXkbiT1qMx0T7wmAAzDG7zPDxvZH8Ow') #google$)C?!<- Av558& 0!A.?@1b @'GX<- 181[ API 0hA$ 5n7O
str(PatientRoute)
mean(PatientRoute$latitude)
mean(PatientRoute$longitude)
Route_Map <- get_googlemap(center = c(lon = 127.089, lat =37.29184), zoom= 6, maptype = c("roadmap")) %>% ggmap 
Route_Map
PatientRoute$patient_id <- as.numeric(PatientRoute$patient_id)
PatientRoute$latitude <- as.numeric(PatientRoute$latitude)
PatientRoute$longitude <- as.numeric(PatientRoute$longitude)
head(PatientRoute)
str(PatientRoute)
Route_Map <- Route_Map + geom_point(data= PatientRoute, aes(x =longitude , y= latitude, color = patient_id), size= 1) + geom_jitter(width = 0.1)  
Route_Map
str(Route_Map)
str(PatientRoute)


#요양원수와 확진자수의 관계
head(Region)
str(Region)
library(dplyr)
Nursing_home <- Region %>%  group_by(province, nursing_home_count) %>% summarize()
head(Nursing_home)
Nursing_home <- Nursing_home %>% group_by(province) %>%  summarize(count = sum(nursing_home_count))
PatientInfo$patient_id <- as.character(PatientInfo$patient_id)
Patient_location <- PatientInfo %>%  group_by(province) %>%  summarize(count = n())
Nursing_Province <- merge(Nursing_home, Patient_location, by = 'province')
Nursing_Province$Ratio <- (Nursing_Province$count.y/2243)*100
head(Nursing_Province)
str(Nursing_Province)
Ratio_lm <- lm(count.x~Ratio, data = Nursing_Province)
plot(Ratio_lm)
summary(Ratio_lm)

#11일 이상 입원 환자 수 
head(PatientInfo)
head(NoReleased)
NoReleased$Days <- as.Date("2020-03-31") -as.Date(NoReleased$confirmed_date)
str(NoReleased$Days)
NoReleased$Days <- as.numeric(NoReleased$Days, units = "days")
head(NoReleased$Days)
NoReleased <- NoReleased %>%  subset(Days >= 11)
str(PatientInfo)
1865/2243*100
NoReleased <- NoReleased %>%  subset(age != "")
head(NoReleased)
ggplot(NoReleased, aes(age, Days, fill = age)) + geom_bar(stat= "identity")
