#####데이터 불러오기 및 확인#####
getwd() #현재 File Directory 확인
setwd("~/Desktop/R_Codes") #File Directory 재설정
Airbnb <- read.csv("Airbnb_Lists.csv", header= T, comment.char = "#") #Airbnb_Lists 파일 불러오기 (첫번째 열을 열 이름으로, 주석은 "#"으로 구분)
str(Airbnb) #Airbnb 데이터 structure 확인 
head(Airbnb) #Airbnb 데이터 첫 6 records 확인
colnames(Airbnb) #Airbnb 데이터 열 이름 확인
######데이터 전처리#####
Airbnb <-Airbnb[,-c(2,4,9,10,11,16,17,1819,21,22,23,28,30,31,36,47,51,60,70,71,72,73,74,75,78,79,87,88,89,91,93,94,95)] #정말 필요 없는 열 삭제 
library(tidyverse) #tidyverse package 불러오기
library(dplyr) #dplyr package 불러오기
Airbnb$price <- parse_number(as.character(Airbnb$price))
Airbnb$accommodates <- as.numeric(as.character(Airbnb$accommodates))
Airbnb$bathrooms <- as.numeric(as.character(Airbnb$bathrooms))
Airbnb$bedrooms <- as.numeric(as.character(Airbnb$bedrooms))
Airbnb$review_scores_rating <- as.numeric(as.character(Airbnb$review_scores_rating))
Airbnb$number_of_reviews <- as.numeric(as.character(Airbnb$number_of_reviews))
Airbnb$review_scores_accuracy <- as.numeric(as.character(Airbnb$review_scores_accuracy))
Airbnb$review_scores_checkin <- as.numeric(as.character(Airbnb$review_scores_checkin))
Airbnb$review_scores_communication <- as.numeric(as.character(Airbnb$review_scores_communication))
Airbnb$review_scores_location <- as.numeric(as.character(Airbnb$review_scores_location))
Airbnb$review_scores_value <- as.numeric(as.character(Airbnb$review_scores_value))
Airbnb$review_scores_cleanliness <- as.numeric(as.character(Airbnb$review_scores_cleanliness))
Airbnb$latitude <- as.numeric(as.character(Airbnb$latitude))
Airbnb$longitude <- as.numeric(as.character(Airbnb$longitude))
#12~25번 Codes: factor/character 또는 $표시가 붙은 숫자 모두 numeric으로 바꾸기 
Airbnb$neighbourhood <- gsub("\303\266", "oe", Airbnb$neighbourhood) 
Airbnb$neighbourhood <- gsub("\303\237", "ss", Airbnb$neighbourhood)
Airbnb$neighbourhood <- gsub("\303\274", "oe", Airbnb$neighbourhood)
Airbnb$neighbourhood <- gsub("\303\244", "ae", Airbnb$neighbourhood)
#19~22 Codes: 독일어 특수문자 오류를 각각 알맞은 다른 발음기호로 변경
Airbnb$City <- ifelse( Airbnb$neighbourhood %in% c("Wilmersdorf", "Moabit", "Schoeneberg", "Charlottenburg", "Westend", "Kladow", "Tiergarten", "Schmargendorf", "Hansaviertel", "Charlottenburg-Nord", "Grunewald", "Dahlem"), Airbnb$City <- "CityWest", 
                       ifelse(Airbnb$neighbourhood %in%  "Mitte", Airbnb$City <- "Mitte",
                              ifelse(Airbnb$neighbourhood %in%  c("Prenzlauer Berg", "Kreuzberg", "Friedrichshain", "Fennpfuhl", "Lichtenberg", "Potsdamer Platz", "Marzahn"), Airbnb$City <- "East Central",
                                     ifelse(Airbnb$neighbourhood %in%  c("Wedding", "Niederschoenhausen", "Heiligensee", "Tegel", "Waidmannslust",  "Weissensee", "Pankow", "Blankenburg", "Reinickendorf", "Frohnau", "Franzoesisch Buchholz" ,"Wittenau", "Gatow", "Haselhorst", "Wilhelmstadt", "Maerkisches_Viertel", "Staaken", "Karow", "Siemensstadt", "Heinersdorf", "Buch"), Airbnb$City <- "North",
                                            ifelse(Airbnb$neighbourhood %in% c("Mahlsdorf", "Biesdorf", "Kaulsdorf", "Alt-Hohenschoenhausen", "Friedrichsfelde", "Hellersdorf", "Buckow"), Airbnb$City <- "East",
                                                   ifelse(Airbnb$neighbourhood %in%  c("Neukoelln","Wannsee", "Friedrichshagen", "Rummelsburg", "Oberschoeneweide", "Steglitz", "Tempelhof", "Baumschulenweg", "Marienfelde", "Adlershof", "Bohnsdorf", "Altglienicke", "Rudow", "Plaenterwald", "Zehlendorf", "Johannisthal", "Gropiusstadt", "Nikolassee", "Koepenick", "Schmoeckwitz", "Alt-Treptow", "Mueggelheim", "Britz", "Halensee", "Karlshorst", "Friedenau", "Lichterfelde", "Lichtenrade", "Lankwitz", "Rahnsdorf", "Mariendorf", "Niederschoeneweide"), Airbnb$City <- "South", Airbnb$City <- ""))))))
Airbnb <- Airbnb[!Airbnb$City == "",] #City 구역이 구분 되지 않은 행 모두 제거 
#Airbnb 지역에 따라서 크게 5구역으로 나누어 City라는 새로운 Column을 생성하여 5 구역으로 나눔. 지역 이름이 표시 되지 않은 곳은 ""으로 나눔. 지역을 나눈 기준은 https://wikitravel.org/en/Berlin 참고.
summary(Airbnb$number_of_reviews, na.rm = T) #리뷰 갯수의 평균, 중앙값 보기
summary(Airbnb$review_scores_rating) #리뷰점수의 평균, 중앙값 보기
Airbnb_Top <- Airbnb %>%  filter(review_scores_rating >= 94) %>%  filter(number_of_reviews >=18) %>%   arrange(desc(review_scores_rating, number_of_reviews)) #리뷰 점수가 94점 이상이고, 리뷰갯수가 1달에 18개 이상인 데이터만 가져오기 그리고 내림차순
Airbnb_Low <- Airbnb %>%  filter(review_scores_rating < 94) %>% filter(number_of_reviews < 18)  %>%  arrange(desc(review_scores_rating, number_of_reviews))  #리뷰 점수가 94점 미만이고, 리뷰 갯수가 1달에 18개 미만인 데티어만 가져오기 그리고 내림차순
#####Airbnb 전체 데이터 분석하기#####

Airbnb %>%  group_by(City) %>%  summarize(Number_Of_Airbnb = n()) #City 구역 별 Airbnb 갯수
library(ggmap)
library(ggplot2)
library(qmap)
register_google(key= 'AIzaSyAMIXkbiT1qMx0T7wmAAzDG7zPDxvZH8Ow') #google에서 지도를 가져오기 위해서 구글 API 계정 등록
mean(Airbnb$latitude) #Airbnb 데이터의 latitude 평균 값
mean(Airbnb$longitude) #Airbnb 데이터의 longitude 평균 값
Berlin_Airbnb_Map <- get_googlemap(center = c(lon = 13.40651, lat =52.50979), zoom= 11, maptype = c("roadmap")) %>% ggmap #Airbnb 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Map <- Berlin_Airbnb_Map + geom_point(data= Airbnb, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1) + ggtitle("Berlin Airbnb") #위 Berlin_Airbnb_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Map
Airbnb %>%  group_by(City) %>%  summarize(Mean_Price_By_District = mean(price)) #City 구역 별 Airbnb 평균 가격
Airbnb_Price_Graph <- ggplot(data= Airbnb, aes(x = City, y = price, group = City)) + geom_boxplot( color = "Blue" , width = 0.3) + coord_cartesian(ylim = c(0,100)) + stat_summary(fun.y="mean", geom="point", shape=20, size=3, color="Red") #Airbnb 지역 별 가격 중앙값과 평균값을 boxplot으로 나타냄. y축은 0-100으로 설정하고 평균값 추가를 위해 stat_summary 함수 사용.
Airbnb_All_Mean <- Airbnb %>%  group_by(City) %>%  summarize(Mean_Rating_By_District = mean(review_scores_rating, na.rm=T))# Airbnb 지역별 평균 점수 
Airbnb_All_Mean_Graph <- ggplot(Airbnb_All_Mean, aes(City,Mean_Rating_By_District, fill = City )) + geom_bar(stat = "identity", position = "dodge2") #Airbnb 지역별 평균 점수를 geom_bar로 그림
Airbnb_All_Mean_Graph
Airbnb_All_Review <- Airbnb %>%  group_by(City) %>%  summarize(Mean_Number_of_Reviews = mean(number_of_reviews, na.rm=T))# Airbnb 지역별 평균 리뷰 갯수
Airbnb_All_Review_Graph <- ggplot(Airbnb_All_Review, aes(City, Mean_Number_of_Reviews, fill = City)) + geom_bar(stat = "identity", position = "dodge2") + coord_flip() + ggtitle("Mean_Number_of_Review")
Airbnb_All_Review_Graph
length(unique(Airbnb$host_is_superhost))
unique(Airbnb$host_is_superhost)
Airbnb_1 <- Airbnb[!Airbnb$host_is_superhost == "",] 
Airbnb_1 %>%  group_by(City) %>%  summarize(n= n())
Airbnb_1 %>%  group_by(City, host_is_superhost) %>%  summarize(Mean_Number_of_Superhost = n())
Airbnb_1_1 <- subset(Airbnb_1, host_is_superhost == "t")
Airbnb_Superhost_All
Airbnb_Superhost_All <- Airbnb_1_1 %>% group_by(City, host_is_superhost) %>%  summarize(n= n())
Airbnb_Superhost_Rate_All <- if (Airbnb_Superhost_All$City %in% "CityWest"){
  (Airbnb_Superhost_All$n)/3241*100
}else if(Airbnb_Superhost_All$City %in% "East"){
  (Airbnb_Superhost_All$n)/214*100
}else if(Airbnb_Superhost_All$City %in% "East Central"){
  (Airbnb_Superhost_All$n)/7515*100
}else if(Airbnb_Superhost_All$City %in% "Mitte"){
  (Airbnb_Superhost_All$n)/1758*100
}else if(Airbnb_Superhost_All$City %in% "South"){
  (Airbnb_Superhost_All$n)/4498*100
}else {
  (Airbnb_Superhost_All$n)/2045*100
}
Airbnb_Superhost_Rate_All
Airbnb_Superhost_Rate_All <- data.frame("City" = c("CityWest","East","East Central", "Mitte", "South", "North"), "Rate" = Airbnb_Superhost_Rate_All)
Airbnb_Superhost_Rate_All_Graph <- ggplot(Airbnb_Superhost_Rate_All, aes(City, Rate, fill = City)) + geom_bar(stat = "identity", position = "dodge2") + coord_flip()
Airbnb_Superhost_Rate_All_Graph

Airbnb_Count <- Airbnb %>%  group_by(City) %>% summarize(Rate_of_Airbnb = n())
Airbnb_Count
Airbnb_Count_Graph <- ggplot(Airbnb_Count, aes(City, Rate_of_Airbnb, fill = City)) + geom_bar(stat = "identity") + coord_flip()
Airbnb_Count_Graph
z <- subset(Airbnb, Airbnb$City == "South")
z <- z %>%  group_by(neighbourhood) %>%  summarize(mean_price = mean(price)) %>%  arrange(desc(mean_price))

Airbnb_All_Number <- Airbnb %>%  group_by(City) %>%  summarize(Number_of_Airbnb =  n())
Airbnb_All_Number
Airbnb_All_Mean_Price <- Airbnb %>%  group_by(City) %>%  summarize(Mean_All_Price = mean(price))
Airbnb_All_Mean_Price
colnames(Airbnb)
ggplot(Airbnb, aes(   review_scores_rating ,number_of_reviews,  color = City)) + geom_point(shape = 15, size = 3) +stat_smooth(color='black', fill='grey')
#####Airbnb_Top과 Airbnb_Low 데이터 비교#####
#####1. Airbnb_Top와 Airbnb_Low 데이터 지도에 표시하기#####
mean(Airbnb_Top$latitude) #Airbnb_Top 데이터의 latitude 평균 값
mean(Airbnb_Top$longitude) #Airbnb_Top 데이터의 longitude 평균 값
Berlin_Airbnb_Top_Map <- get_googlemap(center = c(lon = 13.40362, lat =52.51105), zoom= 11, maptype = c("roadmap")) %>% ggmap #Airbnb_Top 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Top_Map <- Berlin_Airbnb_Top_Map + geom_point(data= Airbnb_Top, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1) + ggtitle("Top_Airbnb_on_the_Map") #위 Berlin_Airbnb_Top_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Top_Map
mean(Airbnb_Low$latitude) #Airbnb_Top 데이터의 latitude 평균 값
mean(Airbnb_Low$longitude) #Airbnb_Top 데이터의 longitude 평균 값
Berlin_Airbnb_Low_Map <- get_googlemap(center = c(lon = 13.40579, lat =52.50945), zoom= 11, maptype = c("roadmap")) %>% ggmap #Airbnb_Top 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Low_Map <- Berlin_Airbnb_Top_Map + geom_point(data= Airbnb_Low, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1)  + ggtitle("Low_Airbnb_on_the_Map")#위 Berlin_Airbnb_Top_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Low_Map
Airbnb %>%  group_by(City) %>% summarize(n= n())
Airbnb_Rate_Top <- if (Airbnb_Top_Rate$City %in% "CityWest"){
  (Airbnb_Top_Rate$Top_Count)/3245*100
}else if(Airbnb_Top_Rate$City %in% "East"){
  (Airbnb_Top_Rate$Top_Count)/214*100
}else if(Airbnb_Top_Rate$City %in% "East Central"){
  (Airbnb_Top_Rate$Top_Count)/7515*100
}else if(Airbnb_Top_Rate$City %in% "Mitte"){
  (Airbnb_Top_Rate$Top_Count)/1758*100
}else if(Airbnb_Top_Rate$City %in% "South"){
  (Airbnb_Top_Rate$Top_Count)/4498*100
}else {
  (Airbnb_Top_Rate$Top_Count)/2045*100
}
Airbnb_Top_Count <- Airbnb_Top %>%  group_by(City) %>%  summarize(Top_Count = n())
Airbnb_Low_Count <- Airbnb_Low %>%  group_by(City) %>%  summarize(Low_Count = n())
Airbnb_Top_Count
Airbnb_Low_Count
Airbnb_Rate_Top
Airbnb_Top_Rate <- Airbnb_Top %>%  group_by(City) %>%  summarize(Top_Count = n()/3101*100)
Airbnb_Top_Rate
Airbnb_Low_Rate <- Airbnb_Low %>%  group_by(City) %>%  summarize(Low_Count = n()/3504*100)
Airbnb_Low_Rate
Airbnb_Rate_Comparison <- inner_join(Airbnb_Top_Rate, Airbnb_Low_Rate)
Airbnb_Rate_Comparison <- melt(Airbnb_Rate_Comparison)
colnames(Airbnb_Rate_Comparison) = c("City","Count","Rate")
Airbnb_Rate_Comparison_Graph <-ggplot(Airbnb_Rate_Comparison, aes(City, Rate, fill = Count)) + geom_bar(stat = "identity", position = "dodge2")
Airbnb_Rate_Comparison_Graph
#####2. 그외 데이터 비교#####
mean(Airbnb_Top$price) #Airbnb_Top 가격 평균
mean(Airbnb_Low$price) #Airbnb_Low 가격 평균
Mean_Airbnb_Top_Price <- as.data.frame( Airbnb_Top %>%  group_by(City)  %>%  summarize(Mean_Airbnb_Top_Price = mean(price, na.rm = T)) )#Airbnb_Top 데이터의 구역별 가격 평균을 데이터프레임으로 만들기
Mean_Airbnb_Low_Price <-as.data.frame( Airbnb_Low %>%  group_by(City) %>%  summarize(Mean_Airbnb_Low_Price = mean(price, na.rm=T))) #Airbnb_Top 데이터의 구역별 가격 평균을 데이터프레임으로 만들기
Airbnb_Price_Comparison <- inner_join(Mean_Airbnb_Top_Price, Mean_Airbnb_Low_Price) #두 데이터 하나로 합치기 
library(data.table) #melt 함수 사용을 위하여 다운로드
Airbnb_Price_Comparison <- melt(Airbnb_Price_Comparison) #ggplot을 그리기 위해 데이터 프레임 구조 변경
colnames(Airbnb_Price_Comparison) = c("City", "Top_Low_Price", "Mean_Price") #열 이름 바꾸기
Airbnb_Price_Comparison_Graph <- ggplot(Airbnb_Price_Comparison,aes(City, Mean_Price, fill = Top_Low_Price)) + geom_bar(stat ="identity", position = "dodge2") #x 축은 City, y축은 가격 평균, 그리고 그래프는 Top데이터와 Low데이터로 각각 채우고, 독립된 bar graph를 그리고 두 그래프 사이는 띄어놓는 구조로 그림.
Airbnb_Price_Comparison_Graph
Airbnb_Low <- Airbnb_Low[!Airbnb_Low$host_is_superhost == "",] #Airbnb_Low$host_is_superhost 데이터 중 ""로 되어있는 데이터 제거
Airbnb_Top_Superhost <-as.data.frame(Airbnb_Top %>%  group_by(host_is_superhost) %>% summarize(superhost_Top = n()/3101*100)) #superhost 여부에 대한 비율 데이터 프레임 만들기 (전체 Airbnb_Top 데이터 중)
Airbnb_Low_Superhost <- as.data.frame(Airbnb_Low %>% group_by(host_is_superhost) %>%  summarize(superhost_Low = n()/3504*100)) #superhost 여부에 대한 비율 데이터 프레임 만들기 (전체 Airbnb_Low 데이터 중)
Airbnb_Superhost_Comparison <- inner_join(Airbnb_Top_Superhost, Airbnb_Low_Superhost) #위 두 데이터 합치기
Airbnb_Superhost_Comparison$host_is_superhost <- toupper(Airbnb_Superhost_Comparison$host_is_superhost) #'t'와 'f' 대문자로 바꾸기
Airbnb_Superhost_Comparison <- Airbnb_Superhost_Comparison[-c(1),] #"f" 행 모두 삭제
Airbnb_Superhost_Comparison <- melt(Airbnb_Superhost_Comparison) #데이터 구조 변환
colnames(Airbnb_Superhost_Comparison) <- c("host_is_superhost", "Type", "Rate") #변환된 데이터 프레임 열 이름 변경하기
Airbnb_Superhost_Comparison_Graph <- ggplot(Airbnb_Superhost_Comparison, aes(Type, Rate, fill = Type)) + geom_bar(stat= "identity", position = "dodge2") #그래프 그리기
Airbnb_Superhost_Comparison_Graph
Airbnb_Top$amenities <- as.character(Airbnb_Top$amenities) #amenities integer에서 문자열로 변경
Airbnb_Low$amenities <- as.character(Airbnb_Low$amenities)#amenities integer에서 문자열로 변경
Airbnb_Top$amenities <- gsub("\\{", "", Airbnb_Top$amenities)
Airbnb_Top$amenities <- gsub("\\}", "", Airbnb_Top$amenities)
Airbnb_Low$amenities <- gsub("\\{", "", Airbnb_Low$amenities)
Airbnb_Low$amenities <- gsub("\\}", "", Airbnb_Low$amenities)
Airbnb_Top$amenities <- gsub("\"","", Airbnb_Top$amenities)
library(tidyverse)
Airbnb_Top %>%  group_by(room_type) %>% summarize( n= n()/3101*100)
Airbnb_Low %>%  group_by(room_type) %>%  summarize( n =n()/3504*100)
Airbnb_Low$amenities <- gsub("\"", "", Airbnb_Low$amenities)
#91~98번 codes: Airbnb_Top과 Airbnb_Low 데이터에서 각각 {},""를 모두 ""로 대체
library(udpipe)
library(NLP)
library(tm)
library(cld3)
library(textcat)
ud_model_en <- udpipe_download_model(language = "english") #udpipe 페키지를 이용, 영문 데이터 POS 태깅을 위한 영어 프로그램 다운로드
ud_model_en <- udpipe_load_model(ud_model_en$file_model) #영어 파일을 적용
Airbnb_Top_Amenities <- subset(Airbnb_Top$amenities, textcat(VectorSource(Airbnb_Top$amenities))=="english")  #textcat 함수를 이용해서 Airbnb_Top$amenities에서 영어로 작성된 데이터만 subset을 이용하여 나눔
Airbnb_Top_Amenities <- gsub("\342\200\231" , "", Airbnb_Top_Amenities) #글자 오류 ""으로 대체 
Airbnb_Top_Amenities <- tolower(Airbnb_Top_Amenities) #모두 소문자로 변경
Top_Amenities <- udpipe_annotate(ud_model_en, x = Airbnb_Top_Amenities, trace =T) #POS 태깅을 함
Top_Amenities <- as.data.frame(Top_Amenities) #태깅된 데이터 데이터 프레임으로 변경
Top_Amenities_Freq <- subset(Top_Amenities, upos %in% "NOUN") #POS 태깅 중 NOUN만 찾기
Top_Amenities_Freq <- txt_freq(x = Top_Amenities_Freq$lemma) #각각의 명사의 빈도수 체크
Airbnb_Low_Amenities <- subset(Airbnb_Low$amenities, textcat(VectorSource(Airbnb_Low$amenities)) == "english") #Airbnb_Top과 같이 영문으로 작성된 데이터 찾기
Airbnb_Low_Amenities <- gsub("\343\200\231", "" , Airbnb_Low_Amenities) #글자 오류 ""으로 대체
Airbnb_Low_Amenities <- tolower(Airbnb_Low_Amenities) #모두 소문자로 변경
Low_Amenities <- udpipe_annotate(ud_model_en, x = Airbnb_Low_Amenities, trace= T) #POS 태깅하기
Low_Amenities <- as.data.frame(Low_Amenities) #데이터 프레임으로 변경
Low_Amenities_Freq <- subset(Low_Amenities, upos %in% "NOUN") #POS 태깅 중 NOUN만 찾기
Low_Amenities_Freq <- txt_freq(x = Low_Amenities_Freq$lemma) #각각의 명사의 빈도수 체크
Top_Amenities_Comparison <- Top_Amenities_Freq %>%  filter(key == "wifi" | key == "tv" | key == "refrigerator" |key == "balcony" | key == "heating" | key == "parking" | key == "dishwasher" |key == "microwave") #명사들 중 wifi, tv, refrigerator, balcony, heating, parking, dishwasher, microwave이라는 단어만 찾아서 따로 데이터 프레임 만들기
Low_Amenities_Comparison <- Low_Amenities_Freq %>%  filter(key == "wifi" | key == "tv" | key == "refrigerator" |key == "balcony" | key == "heating" | key == "parking" | key == "dishwasher" | key == "microwave") #위와 같음
Top_Amenities_Comparison$Rate <- Top_Amenities_Comparison$freq/2370*100   #전체 데이터로 나누어 비율을 알아냄     
Low_Amenities_Comparison$Rate <- Low_Amenities_Comparison$freq/2466*100   #위와 같음
colnames(Low_Amenities_Comparison) <- c("key", "Freq_Low","Freq_pct_Low", "Rate_Low") #inner_join을 위해 Low_Amenities의 열이름 변경
Amenities_Comparison <- inner_join(Top_Amenities_Comparison, Low_Amenities_Comparison) # 위 두 데이터 합치기
Amenities_Comparison_Graph<- ggplot(Amenities_Comparison , aes(key, Rate, group =1)) + geom_line(color= "blue", lwd= 1) + geom_line(aes(key, Rate_Low), color = "red", lwd = 1)+ geom_point() + geom_point(aes(key, Rate_Low)) + ggtitle("Amenities_Comparison") #Top과 Low의 Rate 데이터를 가지고 line Graph를 만듬. x 축= 'key', y축 = 빈도 비율, geom_point로 점을 찍고, 굵기는 lwd= 1
Amenities_Comparison_Graph
Airbnb_Top_Identity <- Airbnb_Top %>%  group_by(host_identity_verified) %>%  summarize(Rate_Top=n()/3101*100)
Airbnb_Low_Identity <- Airbnb_Low %>%  group_by(host_identity_verified) %>% summarize(Rate_Low=n()/3504*100)
Airbnb_Identity <- inner_join(Airbnb_Top_Identity, Airbnb_Low_Identity)
Airbnb_Identity$host_identity_verified <- toupper (Airbnb_Identity$host_identity_verified)
colnames(Airbnb_Identity) = c("Identity", "Top", "Low")
Airbnb_Identity
length(unique(Airbnb_Top$host_id)) #중복되지 않은 host_id 갯수를 세서, Airbnb_Top의 전체 데이터와의 차이를 구하면 그 값이 중복값
length(unique(Airbnb_Low$host_id)) #중복되지 않은 host_id 갯수를 세서, Airbnb_Top의 전체 데이터와의 차이를 구하면 그 값이 중복값
306/3101*100 #Airbnb_Top의 중복되는 host 비율 구하기
239/3504*100 #Airbnb_Low의 중복되는 host 비율 구하기
Airbnb_Top_hosts <- subset(Airbnb_Top, duplicated(Airbnb_Top$host_id) ==T)
Airbnb_Low_hosts <- subset(Airbnb_Low, duplicated(Airbnb_Low$host_id) == T)
mean(Airbnb_Top_hosts$latitude) #Airbnb_Top_hosts 데이터의 latitude 평균 값
mean(Airbnb_Top_hosts$longitude) #Airbnb_Top_hosts 데이터의 longitude 평균 값
Berlin_Airbnb_Top_Hosts_Map <- get_googlemap(center = c(lon = 13.40146, lat =52.51329), zoom= 12, maptype = c("roadmap")) %>% ggmap #Airbnb_Top 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Top_Hosts_Map <- Berlin_Airbnb_Top_Hosts_Map + geom_point(data= Airbnb_Top_hosts, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1) + ggtitle("Airbnb_Top_Multiple_Map") #위 Berlin_Airbnb_Top_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Top_Hosts_Map
mean(Airbnb_Low_hosts$latitude) #Airbnb_Low_hosts 데이터의 latitude 평균 값
mean(Airbnb_Low_hosts$longitude) #Airbnb_Low_hosts 데이터의 longitude 평균 값
Berlin_Airbnb_Low_Hosts_Map <- get_googlemap(center = c(lon = 13.391, lat =52.51077), zoom= 12, maptype = c("roadmap")) %>% ggmap #Airbnb_Low 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Low_Hosts_Map <- Berlin_Airbnb_Low_Hosts_Map + geom_point(data= Airbnb_Low_hosts, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1) + ggtitle("Airbnb_Low_Multipe_Map") #위 Berlin_Airbnb_Top_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Low_Hosts_Map
Airbnb_Top_Multiple <- Airbnb_Top_hosts %>%  group_by(City) %>%  summarize(Top_Mutiple=n()/306*100) #도시 별 여러 채를 가지고 있는 Airbnb 비율
Airbnb_Low_Multiple <- Airbnb_Low_hosts %>%  group_by(City) %>%  summarize(Low_Multiple=n()/239*100) #도시 별 여러 채를 가지고 있는 Airbnb 비율
Airbnb_Multiple_Houses <- inner_join(Airbnb_Top_Multiple,Airbnb_Low_Multiple)
Airbnb_Multiple_Houses <- melt(Airbnb_Multiple_Houses)
colnames(Airbnb_Multiple_Houses) <- c("City", "Airbnb", "Rate")
ggplot(Airbnb_Multiple_Houses, aes(City, Rate, fill = Airbnb )) + geom_bar(stat= "identity", position= "dodge2")
######Airbnb_Top과 Airbnb_Low Text Mining#####
ud_model_de <- udpipe_download_model(language = "german") #dpipe 페키지를 이용, 독일어 데이터 POS 태깅을 위한 독일어 프로그램 다운로드
ud_model_de <- udpipe_load_model(ud_model_de$file_model) #독일어 파일을 적용
??textcat
Top_Description_de <- subset(Airbnb_Top, textcat(VectorSource(Airbnb_Top$description)) =="german")  #독일어로 작성된 description만 찾기
Top_Description_de$description <- gsub("<U+00E4>", "ae", Top_Description_de$description)
Top_Description_de$description  <- gsub("<U+00FC>", "ue", Top_Description_de$description )
Top_Description_de$description  <- gsub("<U+00DF", "ss", Top_Description_de$description )
Top_Description_de$description  <- gsub("<U+00F6>", "oe",Top_Description_de$description )
Top_Description_de$description <- gsub("\303\274", "ue", Top_Description_de$description )
Top_Description_de$description  <- gsub("\303\266", "oe",Top_Description_de$description )
Top_Description_de$description  <- gsub("\303\237", "ss", Top_Description_de$description )
Top_Description_de$description  <- gsub("\303\244", "ae", Top_Description_de$description )
Top_Description_de$description  <- gsub("\303\251", "e", Top_Description_de$description )
Top_Description_de$description  <- gsub("\303\234", "ue", Top_Description_de$description )
#133~142번 codes: 독일어 특수 문자 오류를 다른 글자로 바꾸기
Top_Description_de$description  <- tolower(Top_Description_de$description ) #모든 글자 소문자로 변경
Top_Description_de$description  <- gsub("\\W", " ", Top_Description_de$description ) #정규표현식을 이용, 문자또는 숫자가 아닌 것을 모두 공백으로 변경
Top_Description_de$description  <- removePunctuation(Top_Description_de$description ) #구두점 삭제
Top_Description_de$description  <- removeWords(Top_Description_de$description , stopwords('german')) #독일어 불용어 삭제
Top_Description_de$description  <- removeWords(Top_Description_de$description , stopwords("english")) #영어 불용어 삭제
Top_Description_de$description  <- removeWords(Top_Description_de$description , c("fuer", "ueber", "m", "s", "cm", "Person", "person", "Verfuegung", "min", "minu", "lage", "raeume", "tuer", "minut", "gaest","Umgebung", "qm")) #그 외 삭제 하고 싶은 단어 삭제
Top_Description_ger <- Top_Description_de$description #description만 따로 저장
Top_Description_ger <- udpipe_annotate(ud_model_de, x = Top_Description_ger, trace =T) #독일어 description POS 태깅
Top_Description_ger <- as.data.frame(Top_Description_ger) #데이터 프레임으로 변경
Top_Rake_de <- keywords_rake(x = Top_Description_ger, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                             relevant =Top_Description_ger$upos %in% c("NOUN", "ADJ"),
                             ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction) 기반 단어 추출/ Top_Description_ger에서 token을 기준으로 POS 태깅 중 명사와 형용사의 연관성을 기반으로 분석 / 단어 개수는 최대 3개 까지.
Top_Rake_de$key <- factor(Top_Rake_de$keyword, levels = rev(Top_Rake_de$keyword))
Top_Rake_de_barchart <- barchart(key ~rake, data= head(subset(Top_Rake_de, freq >3), 30), col = "orange", 
                                 main  = "Rake_Top_Description_ger", xlab = "Rake") # Top_Rake_de 기준으로 상위 30개, 빈도수 3이상인 단어만 뽑아서, 주황색으로 그래프 그리기
Top_Rake_de_barchart
library(RColorBrewer)
library(wordcloud)
palete <- brewer.pal(9, 'Set3') #파스텔색깔 팔레트 만들기
Top_Rake_de_wordcloud <- wordcloud(words = Top_Rake_de$key, freq = Top_Rake_de$freq,min.freq = 3 ,max.words= 100, random.order = FALSE, scale=c(5, 1), rot.per=0.25,
                                   
                                   random.color=T, colors=palete) #워드클라우드 만들기 단어는 Top_Rake_de 중 key, 빈도수는 Top_Rake_de에서 빈도수 열, 그리고 최소 빈도수는 3, 최대 단어수는 100, 빈도수에 따라 배열을 하고, 크기는 0.25
Top_Rake_de_wordcloud
Top_Description_en <- subset(Airbnb_Top, textcat(VectorSource(Airbnb_Top$description)) == "english") #영어로 작성된 description만 찾기
Top_Description_en$description <- gsub("<U+00E4>", "ae", Top_Description_en$description)
Top_Description_en$description  <- gsub("<U+00FC>", "ue", Top_Description_en$description)
Top_Description_en$description  <- gsub("<U+00DF", "ss", Top_Description_en$description )
Top_Description_en$description  <- gsub("<U+00F6>", "oe",Top_Description_en$description )
Top_Description_en$description <- gsub("\303\274", "ue", Top_Description_en$description )
Top_Description_en$description  <- gsub("\303\266", "oe",Top_Description_en$description )
Top_Description_en$description  <- gsub("\303\237", "ss", Top_Description_en$description )
Top_Description_en$description  <- gsub("\303\244", "ae", Top_Description_en$description )
Top_Description_en$description  <- gsub("\303\251", "e", Top_Description_en$description )
Top_Description_en$description  <- gsub("\303\234", "ue", Top_Description_en$description )
#167~177번 codes: 독일어 특수 문자 오류를 다른 글자로 바꾸기
Top_Description_en$description  <- tolower(Top_Description_en$description ) #모든 글자 소문자로 변경
Top_Description_en$description  <- gsub("\\W", " ", Top_Description_en$description ) #정규표현식을 이용, 문자또는 숫자가 아닌 것을 모두 공백으로 변경
Top_Description_en$description  <- removePunctuation(Top_Description_en$description ) #구두점 삭제
Top_Description_en$description  <- removeWords(Top_Description_en$description , stopwords('german')) #독일어 불용어 삭제
Top_Description_en$description  <- removeWords(Top_Description_en$description , stopwords("english")) #영어 불용어 삭제
Top_Description_en$description  <- removeWords(Top_Description_en$description , c("fuer", "ueber", "m", "s", "cm", "Person", "person", "Verfuegung", "min", "minu", "lage", "raeume", "tuer", "minut", "gaest","Umgebung", "qm", "minutes", "sqm", "min", "cm","km","heart","beautiful")) #그 외 삭제 하고 싶은 단어 삭제
Top_Description_eng <- Top_Description_en$description #description만 따로 저장
Top_Description_eng <- udpipe_annotate(ud_model_en, x = Top_Description_eng, trace =T) #영어 description POS 태깅
Top_Description_eng <- as.data.frame(Top_Description_eng) #데이터 프레임으로 변경
length(unique(Top_Description_ger$token))
length(unique(Low_Description_ger$token))
length(unique(Top_Description_eng$token))
length(unique(Low_Description_eng$token))

Top_Rake_en <- keywords_rake(x = Top_Description_eng, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                             relevant =Top_Description_eng$upos %in% c("NOUN", "ADJ"),
                             ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction) 기반 단어 추출/ Top_Description_ger에서 token을 기준으로 POS 태깅 중 명사와 형용사의 연관성을 기반으로 분석 / 단어 개수는 최대 3개 까지.
Top_Rake_en$key <- factor(Top_Rake_en$keyword, levels = rev(Top_Rake_en$keyword))
Top_Rake_en_barchart <- barchart(key ~rake, data= head(subset(Top_Rake_en, freq >3), 30), col = "orange", 
                                 main  = "Rake_Top_Description_eng", xlab = "Rake") # Top_Rake_de 기준으로 상위 30개, 빈도수 3이상인 단어만 뽑아서, 주황색으로 그래프 그리기
Top_Rake_en_barchart
Top_Rake_en_wordcloud <- wordcloud(words = Top_Rake_en$key, freq = Top_Rake_en$freq,min.freq = 4,max.words= 100, random.order = FALSE, scale=c(5, 1), rot.per=0.25,
                                   random.color=T, colors=palete) #워드클라우드 만들기 단어는 Top_Rake_de 중 key, 빈도수는 Top_Rake_de에서 빈도수 열, 그리고 최소 빈도수는 3, 최대 단어수는 100, 빈도수에 따라 배열을 하고, 크기는 0.25
Top_Rake_en_wordcloud

Airbnb_Top %>%  group_by( room_type) %>% summarize(n= n())


Low_Description_de <- subset(Airbnb_Low, textcat(VectorSource(Airbnb_Low$description)) =="german")  #독일어로 작성된 description만 찾기
Low_Description_de$description <- gsub("<U+00E4>", "ae", Low_Description_de$description)
Low_Description_de$description  <- gsub("<U+00FC>", "ue", Low_Description_de$description )
Low_Description_de$description  <- gsub("<U+00DF", "ss", Low_Description_de$description )
Low_Description_de$description  <- gsub("<U+00F6>", "oe",Low_Description_de$description )
Low_Description_de$description <- gsub("\303\274", "ue", Low_Description_de$description )
Low_Description_de$description  <- gsub("\303\266", "oe",Low_Description_de$description )
Low_Description_de$description  <- gsub("\303\237", "ss", Low_Description_de$description )
Low_Description_de$description  <- gsub("\303\244", "ae", Low_Description_de$description)
Low_Description_de$description <- gsub("\303\251", "e", Low_Description_de$description )
Low_Description_de$description  <- gsub("\303\234", "ue", Low_Description_de$description)
#198~108번 codes: 독일어 특수 문자 오류를 다른 글자로 바꾸기
Low_Description_de$description  <- tolower(Low_Description_de$description ) #모든 글자 소문자로 변경
Low_Description_de$description  <- gsub("\\W", " ", Low_Description_de$description) #정규표현식을 이용, 문자또는 숫자가 아닌 것을 모두 공백으로 변경
Low_Description_de$description <- removePunctuation(Low_Description_de$description ) #구두점 삭제
Low_Description_de$description  <- removeWords(Low_Description_de$description , stopwords('german')) #독일어 불용어 삭제
Low_Description_de$description  <- removeWords(Low_Description_de$description , stopwords("english")) #영어 불용어 삭제
Low_Description_de$description  <- removeWords(Low_Description_de$description , c("fuer", "ueber", "m", "s", "cm", "Person", "person", "Verfuegung", "min", "minu", "lage", "raeume", "tuer", "minut", "gaest","Umgebung", "qm","sqm", "qm","km","cm")) #그 외 삭제 하고 싶은 단어 삭제
Low_Description_ger <- Low_Description_de$description #description만 따로 저장
Low_Description_ger <- udpipe_annotate(ud_model_de, x = Low_Description_ger, trace =T) #독일어 description POS 태깅
Low_Description_ger <- as.data.frame(Low_Description_ger) #데이터 프레임으로 변경
library(lattice)
Low_Rake_de <- keywords_rake(x = Low_Description_ger, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                             relevant =Low_Description_ger$upos %in% c("NOUN", "ADJ"),
                             ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction) 기반 단어 추출/ Low_Description_ger에서 token을 기준으로 POS 태깅 중 명사와 형용사의 연관성을 기반으로 분석 / 단어 개수는 최대 3개까지
Low_Rake_de$key <- factor(Low_Rake_de$keyword, levels = rev(Low_Rake_de$keyword))
Low_Rake_de_barchart <- barchart(key ~rake, data= head(subset(Low_Rake_de, freq >3), 30), col = "green", 
                                 main  = "Rake_Low_Description_ger", xlab = "Rake") # Low_Rake_de 기준으로 상위 30개, 빈도수 3이상인 단어만 뽑아서, 주황색으로 그래프 그리기
Low_Rake_de_barchart
Low_Rake_de_wordcloud <- wordcloud(words = Low_Rake_de$key, freq = Low_Rake_de$freq,min.freq = 3 ,max.words= 100, random.order = FALSE, scale=c(5, 1), rot.per=0.25,
                                   random.color=T, colors=palete)#워드클라우드 만들기 단어는 Low_Rake_de 중 key, 빈도수는 Low_Rake_de에서 빈도수 열, 그리고 최소 빈도수는 3, 최대 단어수는 100, 빈도수에 따라 배열을 하고, 크기는 0.25

Low_Description_en <- subset(Airbnb_Low, textcat(VectorSource(Airbnb_Low$description)) == "english") #영어로 작성된 description만 찾기
Low_Description_en$description <- gsub("<U+00E4>", "ae", Low_Description_en$description)
Low_Description_en$description <- gsub("<U+00FC>", "ue", Low_Description_en$description)
Low_Description_en$description  <- gsub("<U+00DF", "ss", Low_Description_en$description)
Low_Description_en$description  <- gsub("<U+00F6>", "oe",Low_Description_en$description )
Low_Description_en$description <- gsub("\303\274", "ue", Low_Description_en$description)
Low_Description_en$description  <- gsub("\303\266", "oe",Low_Description_en$description)
Low_Description_en$description  <- gsub("\303\237", "ss",Low_Description_en$description)
Low_Description_en$description  <- gsub("\303\244", "ae", Low_Description_en$description)
Low_Description_en$description  <- gsub("\303\251", "e", Low_Description_en$description )
Low_Description_en$description  <- gsub("\303\234", "ue", Low_Description_en$description )
#133~142번 codes: 독일어 특수 문자 오류를 다른 글자로 바꾸기
Low_Description_en$description  <- tolower(Low_Description_en$description ) #모든 글자 소문자로 변경
Low_Description_en$description  <- gsub("\\W", " ", Low_Description_en$description ) #정규표현식을 이용, 문자또는 숫자가 아닌 것을 모두 공백으로 변경
Low_Description_en$description  <- removePunctuation(Low_Description_en$description ) #구두점 삭제
Low_Description_en$description  <- removeWords(Low_Description_en$description , stopwords('german')) #독일어 불용어 삭제
Low_Description_en$description  <- removeWords(Low_Description_en$description , stopwords("english")) #영어 불용어 삭제
Low_Description_en$description <- removeWords(Low_Description_en$description , c("fuer", "ueber", "m", "s", "cm", "Person", "person", "Verfuegung", "min", "minu", "lage", "raeume", "tuer", "minut", "gaest","Umgebung", "qm", "minutes", "sqm", "min", "cm","km","heart","beautiful")) #그 외 삭제 하고 싶은 단어 삭제
Low_Description_eng <- Low_Description_en$description #description만 따로 저장
Low_Description_eng <- udpipe_annotate(ud_model_en, x = Low_Description_eng, trace =T) #영어 description POS 태깅
Low_Description_eng <- as.data.frame(Low_Description_eng) #데이터 프레임으로 변경

Low_Rake_en <- keywords_rake(x = Low_Description_eng, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                             relevant =Low_Description_eng$upos %in% c("NOUN", "ADJ"),
                             ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction) 기반 단어 추출/ Top_Description_ger에서 token을 기준으로 POS 태깅 중 명사와 형용사의 연관성을 기반으로 분석 / 단어 갯수는 최대 3개까지.
Low_Rake_en
Low_Rake_en$key <- factor(Low_Rake_en$keyword, levels = rev(Low_Rake_en$keyword))
Low_Rake_en_barchart <- barchart(key ~rake, data= head(subset(Low_Rake_en, freq >3), 30), col = "green", 
                                 main  = "Rake_Low_Description_eng", xlab = "Rake") # Low_Rake_en 기준으로 상위 30개, 빈도수 3이상인 단어만 뽑아서, 주황색으로 그래프 그리기
Low_Rake_en_barchart
Low_Rake_en_wordcloud <- wordcloud(words = Low_Rake_en$key, freq = Low_Rake_en$freq,min.freq = 3,max.words= 100, random.order = FALSE, scale=c(5, 1), rot.per=0.25,
                                   random.color=T, colors=palete) #워드클라우드 만들기 단어는 Top_Rake_de 중 key, 빈도수는 Top_Rake_de에서 빈도수 열, 그리고 최소 빈도수는 3, 최대 단어수는 100, 빈도수에 따라 배열을 하고, 크기는 0.25

Low_Rake_en_wordcloud

h <- Airbnb %>% group_by( City, room_type) %>%  summarize(n = n(), price= mean(price))

Review <- reviews_summary
is.character(Review$listing_id)
Review$listing_id <- as.character(Review$listing_id)
Review$date <- as.Date(Review$date)
Review <- left_join(Review, Airbnb, by = c("listing_id" = "id"))
Review %>%  group_by(City, date) %>%  summarize(n = n())
Review[Review$City == "Mitte"]
Review <- Review %>%  filter(!is.na(City))
Date_City <- Review %>%  group_by(Year, City) %>%  summarize(n = n()/353485*100) %>%  arrange(desc(n))
Date_City
ggplot(Date_City, aes(x=Year, y= n , group= City, color = City)) +
  geom_line() +
  ggtitle("Number_of_Review_by_Month")
Review <- cbind(Review, Year = substr(Review$date, 1,4))
colnames(Review)
Review <- Review[,-c(72)]

unique_listing_id <- unique(Review$listing_id)
Lists_Year <- Review %>%  group_by(City, listing_id, Year) %>% summarize (n= n())
Lists_Year
unique(Review$Year)
is.factor(Review$Year)
mean(Review$review_scores_rating, na.rm =T)
median(Review$number_of_reviews)
Review_Top <- Review %>% filter(review_scores_rating >=94, number_of_reviews  >= 70) 
Review_Low <- Review %>%  filter(review_scores_rating <94, number_of_reviews <70)
Date_Top <- Review_Top %>%  group_by(Year, City) %>%  summarize(n = n()/114582*100) %>%  arrange(desc(n))
Date_Low <- Review_Top %>%  group_by(Year, City) %>% summarize(n= n()/55440*100) %>% arrange(desc(n))
Date_Top
Date_Low
ggplot(Date_Top , aes( x = Year, y = n , group = City, color = City)) + geom_line()
ggplot(Date_Low, aes(x = Year, y= n, group = City, color = City)) + geom_line()
House_Number <-  Review %>%  group_by(Year, City) %>%  summarize(n= length(unique(listing_id)))
ggplot(House_Number,aes( x = Year, y= n , group = City, color = City)) + geom_line() + geom_point(size = 1, shape = 5)
Number_Review <- Review %>%  group_by(Year, City) %>%  summarize( n = n())
ggplot(Number_Review, aes( x= Year, y =  n , group =City, color = City))+ geom_line()
is.numeric(Airbnb$price)
Price_City <- Airbnb %>%  group_by(City, room_type) %>%  summarize(n = mean(price))
ggplot(Price_City, aes(City, n , color = room_type )) + geom_bar()
Place <- c("Berghain", "Brandenburger Tor", "Kreuzberg", "Alexanderplatz", "Tiergarten", "Berliner Hbf", "East Side Gallery")
Longitude <- c(52.5067, 52.5098, 52.49860, 52.5195, 52.507920, 52.5249, 52.5018)
Latitude <- c(13.4392 ,13.3752, 13.391799,13.4072, 13.337755, 13.3692, 13.4402)
Berlin_Places <- data.frame(Place, Longitude, Latitude)
Berlin_Airbnb_Map <- get_googlemap(center = c(lon = 13.40651, lat =52.50979), zoom= 11, maptype = c("roadmap")) %>% ggmap #Airbnb 지도의 중심은 각각 longitude와 latitude의 평균값, 지도 타입은 도로가 나와있는 roadmap을 ggmap 페키지를 이용하여 불러오기
Berlin_Airbnb_Map <- Berlin_Airbnb_Map + geom_point(data= Airbnb, aes(x =longitude , y= latitude, color = City), size= 0.5) + geom_jitter(width = 0.1) + geom_point(data = Berlin_Places, aes(x = Longitude, y  =Latitude, color = Place), size = 3) #위 Berlin_Airbnb_Map에 산점도로 위도/경도 데이터를 이용하여 color는 City 열 데이터로 구분 하고, 점의 크기는 0.5, geom_jitter 함수로 점사이 간격 0.1
Berlin_Airbnb_Map
library(tidyverse)
Airbnb %>%  group_by(City) %>%  summarize(n= n())
