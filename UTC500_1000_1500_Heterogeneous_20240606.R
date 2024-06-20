
rm(list=ls())  # 这行代码是清空当前工作环境中的所有对象，确保你开始一个干净的工作空间

library(DescTools)
library(foreign)
library(Matrix)
library(lfe)  
library(magrittr)
library(naniar)
library(dplyr)
library(zoo)
library(readxl)
library(mice)
library(rio)
library(DMwR2)
library(ggplot2)
library(plm)
library(clubSandwich)
library(stargazer)
library(gsubfn)
library(margins)
library(car)

library(lfe)
library(sandwich)

############################## 读取2018年数据############################################
## 数据路径

str_year='2018'

str0='D:/Sentiment_Brazil/brazil_twitter_points_2018-2022_sentiment/year'
file_str0=paste(str0, str_year, '/', sep="")
file_str=paste(str0, str_year, '/DataDrivingFiles2/', sep="")

##情感数据名称

sentiment_UTC_data='brazil_twitter_points_UTC_'

input_file_name= paste(file_str0, sentiment_UTC_data, str_year, '.csv', sep="")  # 字符串连接
Lat_sentiment_TUC = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                             strip.white = FALSE)

print(nrow(Lat_sentiment_TUC))

# 获取具体值
ID<-Lat_sentiment_TUC[,1]
# 将数值向量转换为字符类型，并在前面添加字母 # paste0()函数
ID <- paste0("user", as.character(ID))  
names(ID)[1] <- "ID"


# 定义要替换的值
old_value <- -1
new_value <- 0.25

# 使用索引将第三列中值为 old_value 的元素替换为 new_value
Lat_sentiment_TUC[, 5][Lat_sentiment_TUC[, 5] == old_value] <- new_value
Lat_sentiment_TUC[, 6][Lat_sentiment_TUC[, 6] == old_value] <- new_value
Lat_sentiment_TUC[, 7][Lat_sentiment_TUC[, 7] == old_value] <- new_value
# 获取具体值
# 获取具体值
Lat<-Lat_sentiment_TUC[,2]
Lon<-Lat_sentiment_TUC[,3]
sentiment<-Lat_sentiment_TUC[,4]
UTC500<-Lat_sentiment_TUC[,5]
UTC1000<-Lat_sentiment_TUC[,6]
UTC1500<-Lat_sentiment_TUC[,7]


# 对矩阵中的所有元素进行平方操作
UTC500_2 <- UTC500^2
names(UTC500_2)[1] <- "UTC500_2"
UTC1000_2 <- UTC1000^2
names(UTC1000_2)[1] <- "UTC1000_2"
UTC1500_2 <- UTC1500^2
names(UTC1500_2)[1] <- "UTC1500_2"
#########################


## 数据读取函数：
loadcsv2<-function(data_dir, input_file_name){
  
  input_file_name = paste(data_dir, input_file_name, '.csv', sep='')
  #print(paste('start a regression:',input_file_name))
  
  fea_poi_all = read.csv(file=input_file_name, 
                         header=FALSE, sep=',', fill=TRUE, blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
  return(fea_poi_all)
}

## 数据名称 list:


CityName=paste('brazil_twitter_points_', str_year,'_CityName', sep='')
StateName=paste('brazil_twitter_points_', str_year,'_StateName', sep='')

userid=paste('brazil_twitter_points_', str_year,'_userid', sep='')
useridtotalnumber=paste('brazil_twitter_points_', str_year,'_useridtotalnumber_all1819', sep='')


Precipitation=paste('brazil_twitter_points_', str_year,'_Precipitation', sep='')
humidity=paste('brazil_twitter_points_', str_year,'_Relativehumidity_2m', sep='')
Wind=paste('brazil_twitter_points_', str_year,'_Windspeed_10m', sep='')
Surface_pressure=paste('brazil_twitter_points_', str_year,'_Surface_pressure', sep='')
cloudcover=paste('brazil_twitter_points_', str_year,'_cloudcover', sep='')
MeanTemperature=paste('brazil_twitter_points_', str_year,'_Temperature_new20240614', sep='')
#MeanTemperature=paste('brazil_twitter_points_', str_year,'_Skin_temperature_new20240614', sep='')


nightlight=paste('brazil_twitter_points_', str_year,'_nightlight', sep='')
population=paste('brazil_twitter_points_', str_year,'_population', sep='')
settlement=paste('brazil_twitter_points_', str_year,'_settlement', sep='')
impervious=paste('brazil_twitter_points_', str_year,'_impervious', sep='')


City_area=paste('brazil_twitter_points_', str_year,'_City_area', sep='')
CityScale=paste('brazil_twitter_points_', str_year,'_CityScale', sep='')
GDP=paste('brazil_twitter_points_', str_year,'_GDP2018', sep='')
Geography=paste('brazil_twitter_points_', str_year,'_Geography', sep='')

yearmonth=paste('brazil_twitter_points_', str_year,'_yearmonth', sep='')
yearmonthcity=paste('brazil_twitter_points_', str_year,'_yearmonthcity', sep='')
yearmonthday=paste('brazil_twitter_points_', str_year,'_yearmonthday', sep='')
diffhours=paste('brazil_twitter_points_', str_year,'_diffhours', sep='')

lcz=paste('brazil_twitter_points_', str_year,'_lcz', sep='')
lcz = loadcsv2(file_str, lcz)
names(lcz)[1] <- "lcz"


## 读取数据集
CityName = loadcsv2(file_str, CityName)
StateName = loadcsv2(file_str, StateName)
print(nrow(StateName))
names(CityName)[1] <- "CityName"
names(StateName)[1] <- "StateName"

userid = loadcsv2(file_str, userid)
names(userid)[1] <- "userid"
useridtotalnumber = loadcsv2(file_str, useridtotalnumber)
names(useridtotalnumber)[1] <- "useridtotalnumber"



Precipitation = loadcsv2(file_str, Precipitation)
Precipitation$V1[is.na(Precipitation$V1)]<-mean(Precipitation$V1,na.rm=TRUE) # 缺少数据变为均值
humidity = loadcsv2(file_str, humidity)
humidity$V1[is.na(humidity$V1)]<-mean(humidity$V1,na.rm=TRUE) # 缺少数据变为均值
Wind = loadcsv2(file_str, Wind)
Wind$V1[is.na(Wind$V1)]<-mean(Wind$V1,na.rm=TRUE) # 缺少数据变为均值
Surface_pressure = loadcsv2(file_str, Surface_pressure)
Surface_pressure$V1[is.na(Surface_pressure$V1)]<-mean(Surface_pressure$V1,na.rm=TRUE) # 缺少数据变为均值
cloudcover = loadcsv2(file_str, cloudcover)
cloudcover$V1[is.na(cloudcover$V1)]<-mean(cloudcover$V1,na.rm=TRUE) # 缺少数据变为均值

names(Precipitation)[1] <- "Precipitation"
names(humidity)[1] <- "humidity"
names(Surface_pressure)[1] <- "Surface_pressure"
names(Wind)[1] <- "Wind"
names(cloudcover)[1] <- "cloudcover"


MeanTemperature = loadcsv2(file_str, MeanTemperature)
MeanTemperature$V1[is.na(MeanTemperature$V1)]<-mean(MeanTemperature$V1,na.rm=TRUE) # 缺少数据变为均值
MeanTemperature<- MeanTemperature -273.15
names(MeanTemperature)[1] <- "MeanTemperature"



nightlight = loadcsv2(file_str, nightlight)
nightlight$V1[is.na(nightlight$V1)]<-mean(nightlight$V1,na.rm=TRUE) # 缺少数据变为均值
nightlight<-log(nightlight+1)
population = loadcsv2(file_str, population)
population$V1[is.na(population$V1)]<-mean(population$V1,na.rm=TRUE) 
population<-log(population+1)
settlement = loadcsv2(file_str, settlement)
settlement$V1[is.na(settlement$V1)]<-mean(settlement$V1,na.rm=TRUE) 
settlement<-log(settlement+1)
names(nightlight)[1] <- "nightlight"
names(population)[1] <- "population"
names(settlement)[1] <- "settlement"


impervious = loadcsv2(file_str, impervious)
impervious[impervious > 1] <- 1
names(impervious)[1] <- "impervious"
unimpervious<- 1-impervious
unimpervious[unimpervious<0]<- 0 # 小于0则为0
names(unimpervious)[1] <- "unimpervious"


City_area = loadcsv2(file_str, City_area)
City_area<-log(City_area+1)
names(City_area)[1] <- "City_area"
CityScale = loadcsv2(file_str, CityScale)
names(CityScale)[1] <- "CityScale"

GDP = loadcsv2(file_str, GDP)
names(GDP)[1] <- "GDP"
Geography = loadcsv2(file_str, Geography)
names(Geography)[1] <- "Geography"


yearmonth = loadcsv2(file_str, yearmonth)
yearmonthday = loadcsv2(file_str, yearmonthday)
diffhours = loadcsv2(file_str, diffhours)
names(yearmonth)[1] <- "yearmonth"
names(yearmonthday)[1] <- "yearmonthday"
names(diffhours)[1] <- "diffhours"

# 各个变量按列合并
query_sample2018 <- cbind( sentiment, 
                           yearmonth,yearmonthday,
                           City_area, CityScale,GDP, Geography,
                           UTC500, UTC500_2,UTC1000, UTC1000_2, UTC1500, UTC1500_2, 
                           Lat,Lon, diffhours, lcz,
                           Precipitation, humidity, Wind, Surface_pressure,cloudcover,MeanTemperature,
                           nightlight,population, settlement, impervious, unimpervious, 
                           CityName, StateName, userid, useridtotalnumber)

rm(sentiment, 
   yearmonth,yearmonthday,
   City_area, CityScale,GDP, Geography,
   UTC500, UTC500_2,UTC1000, UTC1000_2, UTC1500, UTC1500_2, 
   Lat,Lon, diffhours,lcz,
   Precipitation, humidity, Wind, Surface_pressure,cloudcover,MeanTemperature,
   nightlight,population, settlement, impervious, unimpervious, 
   CityName, StateName, userid, useridtotalnumber)





############################## 读取2019年数据############################################
## 数据路径

str_year='2019'

str0='D:/Sentiment_Brazil/brazil_twitter_points_2018-2022_sentiment/year'
file_str0=paste(str0, str_year, '/', sep="")
file_str=paste(str0, str_year, '/DataDrivingFiles2/', sep="")

##情感数据名称

sentiment_UTC_data='brazil_twitter_points_UTC_'

input_file_name= paste(file_str0, sentiment_UTC_data, str_year, '.csv', sep="")  # 字符串连接
Lat_sentiment_TUC = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                             strip.white = FALSE)

print(nrow(Lat_sentiment_TUC))

# 获取具体值
ID<-Lat_sentiment_TUC[,1]
# 将数值向量转换为字符类型，并在前面添加字母 # paste0()函数
ID <- paste0("user", as.character(ID))  
names(ID)[1] <- "ID"


# 定义要替换的值
old_value <- -1
new_value <- 0.25

# 使用索引将第三列中值为 old_value 的元素替换为 new_value
Lat_sentiment_TUC[, 5][Lat_sentiment_TUC[, 5] == old_value] <- new_value
Lat_sentiment_TUC[, 6][Lat_sentiment_TUC[, 6] == old_value] <- new_value
Lat_sentiment_TUC[, 7][Lat_sentiment_TUC[, 7] == old_value] <- new_value
# 获取具体值
# 获取具体值
Lat<-Lat_sentiment_TUC[,2]
Lon<-Lat_sentiment_TUC[,3]
sentiment<-Lat_sentiment_TUC[,4]
UTC500<-Lat_sentiment_TUC[,5]
UTC1000<-Lat_sentiment_TUC[,6]
UTC1500<-Lat_sentiment_TUC[,7]


# 对矩阵中的所有元素进行平方操作
UTC500_2 <- UTC500^2
names(UTC500_2)[1] <- "UTC500_2"
UTC1000_2 <- UTC1000^2
names(UTC1000_2)[1] <- "UTC1000_2"
UTC1500_2 <- UTC1500^2
names(UTC1500_2)[1] <- "UTC1500_2"
#########################


## 数据读取函数：
loadcsv2<-function(data_dir, input_file_name){
  
  input_file_name = paste(data_dir, input_file_name, '.csv', sep='')
  #print(paste('start a regression:',input_file_name))
  
  fea_poi_all = read.csv(file=input_file_name, 
                         header=FALSE, sep=',', fill=TRUE, blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
  return(fea_poi_all)
}

## 数据名称 list:


CityName=paste('brazil_twitter_points_', str_year,'_CityName', sep='')
StateName=paste('brazil_twitter_points_', str_year,'_StateName', sep='')

userid=paste('brazil_twitter_points_', str_year,'_userid', sep='')
useridtotalnumber=paste('brazil_twitter_points_', str_year,'_useridtotalnumber_all1819', sep='')


Precipitation=paste('brazil_twitter_points_', str_year,'_Precipitation', sep='')
humidity=paste('brazil_twitter_points_', str_year,'_Relativehumidity_2m', sep='')
Wind=paste('brazil_twitter_points_', str_year,'_Windspeed_10m', sep='')
Surface_pressure=paste('brazil_twitter_points_', str_year,'_Surface_pressure', sep='')
cloudcover=paste('brazil_twitter_points_', str_year,'_cloudcover', sep='')
MeanTemperature=paste('brazil_twitter_points_', str_year,'_Temperature_new20240614', sep='')
#MeanTemperature=paste('brazil_twitter_points_', str_year,'_Skin_temperature_new20240614', sep='')


nightlight=paste('brazil_twitter_points_', str_year,'_nightlight', sep='')
population=paste('brazil_twitter_points_', str_year,'_population', sep='')
settlement=paste('brazil_twitter_points_', str_year,'_settlement', sep='')
impervious=paste('brazil_twitter_points_', str_year,'_impervious', sep='')


City_area=paste('brazil_twitter_points_', str_year,'_City_area', sep='')
CityScale=paste('brazil_twitter_points_', str_year,'_CityScale', sep='')
GDP=paste('brazil_twitter_points_', str_year,'_GDP2018', sep='')
Geography=paste('brazil_twitter_points_', str_year,'_Geography', sep='')

yearmonth=paste('brazil_twitter_points_', str_year,'_yearmonth', sep='')
yearmonthcity=paste('brazil_twitter_points_', str_year,'_yearmonthcity', sep='')
yearmonthday=paste('brazil_twitter_points_', str_year,'_yearmonthday', sep='')
diffhours=paste('brazil_twitter_points_', str_year,'_diffhours', sep='')

lcz=paste('brazil_twitter_points_', str_year,'_lcz', sep='')
lcz = loadcsv2(file_str, lcz)
names(lcz)[1] <- "lcz"

## 读取数据集
CityName = loadcsv2(file_str, CityName)
StateName = loadcsv2(file_str, StateName)
print(nrow(StateName))
names(CityName)[1] <- "CityName"
names(StateName)[1] <- "StateName"

userid = loadcsv2(file_str, userid)
names(userid)[1] <- "userid"
useridtotalnumber = loadcsv2(file_str, useridtotalnumber)
names(useridtotalnumber)[1] <- "useridtotalnumber"



Precipitation = loadcsv2(file_str, Precipitation)
Precipitation$V1[is.na(Precipitation$V1)]<-mean(Precipitation$V1,na.rm=TRUE) # 缺少数据变为均值
humidity = loadcsv2(file_str, humidity)
humidity$V1[is.na(humidity$V1)]<-mean(humidity$V1,na.rm=TRUE) # 缺少数据变为均值
Wind = loadcsv2(file_str, Wind)
Wind$V1[is.na(Wind$V1)]<-mean(Wind$V1,na.rm=TRUE) # 缺少数据变为均值
Surface_pressure = loadcsv2(file_str, Surface_pressure)
Surface_pressure$V1[is.na(Surface_pressure$V1)]<-mean(Surface_pressure$V1,na.rm=TRUE) # 缺少数据变为均值
cloudcover = loadcsv2(file_str, cloudcover)
cloudcover$V1[is.na(cloudcover$V1)]<-mean(cloudcover$V1,na.rm=TRUE) # 缺少数据变为均值

names(Precipitation)[1] <- "Precipitation"
names(humidity)[1] <- "humidity"
names(Surface_pressure)[1] <- "Surface_pressure"
names(Wind)[1] <- "Wind"
names(cloudcover)[1] <- "cloudcover"


MeanTemperature = loadcsv2(file_str, MeanTemperature)
MeanTemperature$V1[is.na(MeanTemperature$V1)]<-mean(MeanTemperature$V1,na.rm=TRUE) # 缺少数据变为均值
MeanTemperature<- MeanTemperature -273.15
names(MeanTemperature)[1] <- "MeanTemperature"



nightlight = loadcsv2(file_str, nightlight)
nightlight$V1[is.na(nightlight$V1)]<-mean(nightlight$V1,na.rm=TRUE) # 缺少数据变为均值
nightlight<-log(nightlight+1)
population = loadcsv2(file_str, population)
population$V1[is.na(population$V1)]<-mean(population$V1,na.rm=TRUE) 
population<-log(population+1)
settlement = loadcsv2(file_str, settlement)
settlement$V1[is.na(settlement$V1)]<-mean(settlement$V1,na.rm=TRUE) 
settlement<-log(settlement+1)
names(nightlight)[1] <- "nightlight"
names(population)[1] <- "population"
names(settlement)[1] <- "settlement"


impervious = loadcsv2(file_str, impervious)
impervious[impervious > 1] <- 1
names(impervious)[1] <- "impervious"
unimpervious<- 1-impervious
unimpervious[unimpervious<0]<- 0 # 小于0则为0
names(unimpervious)[1] <- "unimpervious"


City_area = loadcsv2(file_str, City_area)
City_area<-log(City_area+1)
names(City_area)[1] <- "City_area"
CityScale = loadcsv2(file_str, CityScale)
names(CityScale)[1] <- "CityScale"

GDP = loadcsv2(file_str, GDP)
names(GDP)[1] <- "GDP"
Geography = loadcsv2(file_str, Geography)
names(Geography)[1] <- "Geography"


yearmonth = loadcsv2(file_str, yearmonth)
yearmonthday = loadcsv2(file_str, yearmonthday)
diffhours = loadcsv2(file_str, diffhours)
names(yearmonth)[1] <- "yearmonth"
names(yearmonthday)[1] <- "yearmonthday"
names(diffhours)[1] <- "diffhours"


# 各个变量按列合并
query_sample2019 <- cbind( sentiment, 
                           yearmonth,yearmonthday,
                           City_area, CityScale,GDP, Geography,
                           UTC500, UTC500_2,UTC1000, UTC1000_2, UTC1500, UTC1500_2, 
                           Lat,Lon, diffhours,lcz,
                           Precipitation, humidity, Wind, Surface_pressure,cloudcover,MeanTemperature,
                           nightlight,population, settlement, impervious, unimpervious, 
                           CityName, StateName, userid, useridtotalnumber)

rm(sentiment, 
   yearmonth,yearmonthday,
   City_area, CityScale,GDP, Geography,
   UTC500, UTC500_2,UTC1000, UTC1000_2, UTC1500, UTC1500_2, 
   Lat,Lon, diffhours,lcz,
   Precipitation, humidity, Wind, Surface_pressure,cloudcover,MeanTemperature,
   nightlight,population, settlement, impervious, unimpervious, 
   CityName, StateName, userid, useridtotalnumber)


################################合并2018年和2019年数据####################################


query_sample <- rbind(query_sample2018, query_sample2019)

###########################################################################################
#rm(query_sample2018, query_sample2019) #清空变量
## 样列分析：
# 选择满足特定条件的数据的所有行进行分析
#subset_data <- data[data$y %in% c("A", "C"), ]
# 选择满足特定条件的数据的所有行进行分析
#subset_data <- query_sample[query_sample$keyword %in% c("X", "Y"), ]
# 选取夏天时间的数据进行实验
#query_sample <- query_sample0[query_sample0$yearmonth %in% c("Y2018M01", "Y2018M02", "Y2018M03","Y2018M04","Y2018M05", "Y2018M10","Y2018M11", "Y2018M12",
#"Y2019M01", "Y2019M02", "Y2019M03","Y2019M04","Y2019M05", "Y2019M10","Y2019M11", "Y2019M12"), ] # yearmonth summer
## 去除RS,SC 2个州的冬天数据
#query_sample <- subset(query_sample0, !(StateName %in% c("RS", "SC") & yearmonth %in% c("Y2018M06", "Y2018M07","Y2018M08","Y2019M06", "Y2019M07","Y2019M08") ) )
#rm(subset_data)



###################################### 去除异常值####################################
query_sample2<-subset(query_sample, !(sentiment < 0.15 & UTC500>0.30) )

query_sample4<- query_sample2
query_sample2 = subset(query_sample2, (useridtotalnumber>100) ) # 过滤tweet文本过少的数据
query_sample2 <- na.omit(query_sample2) # 删除缺少项


rm(query_sample)
rm(Lat_sentiment_TUC)
#summary(query_sample2)

#################################### 去取变量均值与标准差####################################
sd_utc5<-sd(query_sample2$UTC500)
sd_utc10<-sd(query_sample2$UTC1000)
sd_utc15<-sd(query_sample2$UTC1500)
sd_sent<-sd(query_sample2$sentiment)
mean_utc5<-mean(query_sample2$UTC500)
mean_utc10<-mean(query_sample2$UTC1000)
mean_utc15<-mean(query_sample2$UTC1500)
mean_sent<-mean(query_sample2$sentiment)

#Precipitation, humidity, Wind, Surface_pressure,cloudcover,MeanTemperature,

mean_Precipitation<- mean(query_sample2$Precipitation)
sd_Precipitation<- sd(query_sample2$Precipitation)

mean_humidity<- mean(query_sample2$humidity)
sd_humidity<- sd(query_sample2$humidity)

mean_Wind<-mean(query_sample2$Wind)
sd_Wind<-sd(query_sample2$Wind)

mean_cloudcover<-mean(query_sample2$cloudcover)
sd_cloudcover<-sd(query_sample2$cloudcover)

mean_MeanTemperature<-mean(query_sample2$MeanTemperature)
sd_MeanTemperature<-sd(query_sample2$MeanTemperature)

mean_Surface_pressure<-mean(query_sample2$Surface_pressure)
sd_Surface_pressure<-sd(query_sample2$Surface_pressure)



mean_nightlight<-mean(query_sample2$nightlight)
sd_nightlight<-sd(query_sample2$nightlight)

mean_population<-mean(query_sample2$population)
sd_population<-sd(query_sample2$population)

mean_settlement<-mean(query_sample2$settlement)
sd_settlement<-sd(query_sample2$settlement)


#query_sample2$unimpervious[query_sample2$unimpervious<0]<- 0 # 小于0则为0
mean_unimpervious<-mean(query_sample2$unimpervious)
sd_unimpervious<-sd(query_sample2$unimpervious)

write.csv(query_sample2$unimpervious,'D:/Sentiment_Brazil/R_codes/NewCodes/Effect202400508/unimpervious.csv')



########################################### Heterogeneous analysis ###########################################



###################################### Supplementary Table 14 Heterogeneous Geography ################################################

model <-felm(sentiment ~ 1+ UTC500+
               #UTC500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable14_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Geography_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_1$group <- c("A")
df_Geography_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               #UTC500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Geography_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_2$group <- c("B")
df_Geography_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               #UTC500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Geography_3 <- data.frame(cbind(cfint, coef, pval))
df_Geography_3$group <- c("C")
df_Geography_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                #UTC500:ifelse(Geography == "CentralBrazil", 1, 0) + 
                UTC500:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Geography_4 <- data.frame(cbind(cfint, coef, pval))
df_Geography_4$group <- c("D")
df_Geography_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                UTC500:ifelse(Geography == "CentralBrazil", 1, 0) + 
                #UTC500:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Geography_5 <- data.frame(cbind(cfint, coef, pval))
df_Geography_5$group <- c("E")
df_Geography_5$tag <- "1"

##############################################################################

############添加另一属性值############
df_Geography_1$character <- "North Brazil"
df_Geography_2$character <- "Northeast Brazil"
df_Geography_3$character <- "Southeast Brazil"
df_Geography_4$character <- "Centralwest Brazil"
df_Geography_5$character <- "South Brazil"
## 保存数据###
#年龄画图区域
df_combine_Geography <- rbind(
  df_Geography_1, df_Geography_2, df_Geography_3, df_Geography_4,df_Geography_5)
## 保存数据###
write.csv(df_combine_Geography,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_Geography.csv')



###################################### Supplementary Table 15 Heterogeneous Geography ################################################

model <-felm(sentiment ~ 1+ UTC1000+
               #UTC1000:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC1000:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable15_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Geography_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_1$group <- c("A")
df_Geography_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(Geography == "NorthBrazil", 1, 0) + 
               #UTC1000:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC1000:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Geography_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_2$group <- c("B")
df_Geography_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               #UTC1000:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1000:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Geography_3 <- data.frame(cbind(cfint, coef, pval))
df_Geography_3$group <- c("C")
df_Geography_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC1000:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC1000:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                #UTC1000:ifelse(Geography == "CentralBrazil", 1, 0) + 
                UTC1000:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Geography_4 <- data.frame(cbind(cfint, coef, pval))
df_Geography_4$group <- c("D")
df_Geography_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC1000:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC1000:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                UTC1000:ifelse(Geography == "CentralBrazil", 1, 0) + 
                #UTC1000:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Geography_5 <- data.frame(cbind(cfint, coef, pval))
df_Geography_5$group <- c("E")
df_Geography_5$tag <- "1"

##############################################################################

############添加另一属性值############
df_Geography_1$character <- "North Brazil"
df_Geography_2$character <- "Northeast Brazil"
df_Geography_3$character <- "Southeast Brazil"
df_Geography_4$character <- "Centralwest Brazil"
df_Geography_5$character <- "South Brazil"
## 保存数据###

df_combine_Geography <- rbind(
  df_Geography_1, df_Geography_2, df_Geography_3, df_Geography_4,df_Geography_5)
## 保存数据###
write.csv(df_combine_Geography,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_Geography.csv')



###################################### Supplementary Table 16 Heterogeneous Geography ################################################

model <-felm(sentiment ~ 1+ UTC1500+
               #UTC1500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC1500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable16_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Geography_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_1$group <- c("A")
df_Geography_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               #UTC1500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               UTC1500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Geography_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Geography_2$group <- c("B")
df_Geography_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(Geography == "NorthBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
               #UTC1500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "CentralBrazil", 1, 0) + 
               UTC1500:ifelse(Geography == "SouthBrazil", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Geography_3 <- data.frame(cbind(cfint, coef, pval))
df_Geography_3$group <- c("C")
df_Geography_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC1500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC1500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                #UTC1500:ifelse(Geography == "CentralBrazil", 1, 0) + 
                UTC1500:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Geography_4 <- data.frame(cbind(cfint, coef, pval))
df_Geography_4$group <- c("D")
df_Geography_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(Geography == "NorthBrazil", 1, 0) + 
                UTC1500:ifelse(Geography == "NorthesatBrazil" | Geography == "NortheastBrazil" , 1, 0) + 
                UTC1500:ifelse(Geography == "SoutheastBrazil", 1, 0) + 
                UTC1500:ifelse(Geography == "CentralBrazil", 1, 0) + 
                #UTC1500:ifelse(Geography == "SouthBrazil", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Geography_5 <- data.frame(cbind(cfint, coef, pval))
df_Geography_5$group <- c("E")
df_Geography_5$tag <- "1"

##############################################################################

############添加另一属性值############
df_Geography_1$character <- "North Brazil"
df_Geography_2$character <- "Northeast Brazil"
df_Geography_3$character <- "Southeast Brazil"
df_Geography_4$character <- "Centralwest Brazil"
df_Geography_5$character <- "South Brazil"
## 保存数据###

df_combine_Geography <- rbind(
  df_Geography_1, df_Geography_2, df_Geography_3, df_Geography_4,df_Geography_5)
## 保存数据###
write.csv(df_combine_Geography,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_Geography.csv')





##################################################UTC 500 LCZ Supplementary Table 17 ###################################################################
model <- felm(sentiment ~ 1+ UTC500+
                #UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable17_Heterogeneous_UTC500_lcz.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_1$group <- c("A")
df_lcz5_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_2$group <- c("B")
df_lcz5_2$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_3 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_3$group <- c("C")
df_lcz5_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_4 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_4$group <- c("D")
df_lcz5_4$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_5 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_5$group <- c("E")
df_lcz5_5$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_6 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_6$group <- c("F")
df_lcz5_6$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_7 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_7$group <- c("G")
df_lcz5_7$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_8 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_8$group <- c("H")
df_lcz5_8$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_9 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_9$group <- c("I")
df_lcz5_9$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC500:ifelse(lcz == "lcz9", 1, 0) + 
                #UTC500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_lcz5_10 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_10$group <- c("J")
df_lcz5_10$tag <- "1"



############添加另一属性值############
df_lcz5_1$character <- "lcz1"
df_lcz5_2$character <- "lcz2"
df_lcz5_3$character <- "lcz3"
df_lcz5_4$character <- "lcz4"
df_lcz5_5$character <- "lcz5 "
df_lcz5_6$character <- "lcz6"
df_lcz5_7$character <- "lcz7"
df_lcz5_8$character <- "lcz8"
df_lcz5_9$character <- "lcz9"
df_lcz5_10$character <- "lcz10"


#年龄画图区域
df_combine_lcz <- rbind(
  df_lcz5_1,df_lcz5_2,df_lcz5_3,df_lcz5_4,df_lcz5_5,df_lcz5_6, df_lcz5_7, df_lcz5_8, df_lcz5_9, df_lcz5_10)

## 保存数据###

write.csv(df_combine_lcz,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_lcz.csv')



##################################################UTC 1000 LCZ SupplementaryTable18 ###################################################################



model <- felm(sentiment ~ 1+ UTC1000+
                #UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable18_Heterogeneous_UTC1000_lcz.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_1$group <- c("A")
df_lcz5_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_2$group <- c("B")
df_lcz5_2$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_3 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_3$group <- c("C")
df_lcz5_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_4 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_4$group <- c("D")
df_lcz5_4$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_5 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_5$group <- c("E")
df_lcz5_5$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_6 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_6$group <- c("F")
df_lcz5_6$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_7 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_7$group <- c("G")
df_lcz5_7$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_8 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_8$group <- c("H")
df_lcz5_8$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_9 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_9$group <- c("I")
df_lcz5_9$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1000:ifelse(lcz == "lcz9", 1, 0) + 
                #UTC1000:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_lcz5_10 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_10$group <- c("J")
df_lcz5_10$tag <- "1"



############添加另一属性值############
df_lcz5_1$character <- "lcz1"
df_lcz5_2$character <- "lcz2"
df_lcz5_3$character <- "lcz3"
df_lcz5_4$character <- "lcz4"
df_lcz5_5$character <- "lcz5 "
df_lcz5_6$character <- "lcz6"
df_lcz5_7$character <- "lcz7"
df_lcz5_8$character <- "lcz8"
df_lcz5_9$character <- "lcz9"
df_lcz5_10$character <- "lcz10"


#年龄画图区域
df_combine_lcz <- rbind(
  df_lcz5_1,df_lcz5_2,df_lcz5_3,df_lcz5_4,df_lcz5_5,df_lcz5_6, df_lcz5_7, df_lcz5_8, df_lcz5_9, df_lcz5_10)

## 保存数据###

write.csv(df_combine_lcz,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_lcz.csv')


##################################################UTC 1500 LCZ SupplementaryTable19 ###################################################################



model <- felm(sentiment ~ 1+ UTC1500+
                #UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable19_Heterogeneous_UTC1500_lcz.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_1$group <- c("A")
df_lcz5_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_2$group <- c("B")
df_lcz5_2$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_3 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_3$group <- c("C")
df_lcz5_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_4 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_4$group <- c("D")
df_lcz5_4$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_5 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_5$group <- c("E")
df_lcz5_5$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_6 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_6$group <- c("F")
df_lcz5_6$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_7 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_7$group <- c("G")
df_lcz5_7$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_8 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_8$group <- c("H")
df_lcz5_8$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_9 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_9$group <- c("I")
df_lcz5_9$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(lcz == "lcz1", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz2", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz3", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz4", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz5", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz6", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz7", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz8", 1, 0) + 
                UTC1500:ifelse(lcz == "lcz9", 1, 0) + 
                #UTC1500:ifelse(lcz == "lcz10", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_lcz5_10 <- data.frame(cbind(cfint, coef, `pval`))
df_lcz5_10$group <- c("J")
df_lcz5_10$tag <- "1"



############添加另一属性值############
df_lcz5_1$character <- "lcz1"
df_lcz5_2$character <- "lcz2"
df_lcz5_3$character <- "lcz3"
df_lcz5_4$character <- "lcz4"
df_lcz5_5$character <- "lcz5 "
df_lcz5_6$character <- "lcz6"
df_lcz5_7$character <- "lcz7"
df_lcz5_8$character <- "lcz8"
df_lcz5_9$character <- "lcz9"
df_lcz5_10$character <- "lcz10"


#年龄画图区域
df_combine_lcz <- rbind(
  df_lcz5_1,df_lcz5_2,df_lcz5_3,df_lcz5_4,df_lcz5_5,df_lcz5_6, df_lcz5_7, df_lcz5_8, df_lcz5_9, df_lcz5_10)

## 保存数据###

write.csv(df_combine_lcz,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_lcz.csv')




###################################### Supplementary Table 20 Heterogeneous Geography GDP ################################################

model <-felm(sentiment ~ 1+ UTC500+
               #UTC500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable20_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_GDP_1 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_1$group <- c("A")
df_GDP_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               #UTC500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_GDP_2 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_2$group <- c("B")
df_GDP_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               #UTC500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_GDP_3 <- data.frame(cbind(cfint, coef, pval))
df_GDP_3$group <- c("C")
df_GDP_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
                UTC500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
                UTC500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
                #UTC500:ifelse(GDP > 15000, 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_GDP_4 <- data.frame(cbind(cfint, coef, pval))
df_GDP_4$group <- c("D")
df_GDP_4$tag <- "1"

############添加另一属性值############
df_GDP_1$character <- "<5000$"
df_GDP_2$character <- "5000$~10000$"
df_GDP_3$character <- "10000$~15000$"
df_GDP_4$character <- "> 15000$"

#年龄画图区域
df_combine_gdp <- rbind(
  df_GDP_1, df_GDP_2, df_GDP_3, df_GDP_4)
## 保存数据###


write.csv(df_combine_gdp,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_gdp.csv')





###################################### Supplementary Table 21 Heterogeneous Geography GDP ################################################

model <-felm(sentiment ~ 1+ UTC1000+
               #UTC1000:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC1000:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC1000:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1000:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable21_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_GDP_1 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_1$group <- c("A")
df_GDP_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               #UTC1000:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC1000:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1000:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_GDP_2 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_2$group <- c("B")
df_GDP_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC1000:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               #UTC1000:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1000:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_GDP_3 <- data.frame(cbind(cfint, coef, pval))
df_GDP_3$group <- c("C")
df_GDP_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
                UTC1000:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
                UTC1000:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
                #UTC1000:ifelse(GDP > 15000, 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_GDP_4 <- data.frame(cbind(cfint, coef, pval))
df_GDP_4$group <- c("D")
df_GDP_4$tag <- "1"

############添加另一属性值############
df_GDP_1$character <- "<5000$"
df_GDP_2$character <- "5000$~10000$"
df_GDP_3$character <- "10000$~15000$"
df_GDP_4$character <- "> 15000$"

#年龄画图区域
df_combine_gdp <- rbind(
  df_GDP_1, df_GDP_2, df_GDP_3, df_GDP_4)
## 保存数据###


write.csv(df_combine_gdp,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_gdp.csv')



###################################### Supplementary Table 22 Heterogeneous Geography GDP ################################################

model <-felm(sentiment ~ 1+ UTC1500+
               #UTC1500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC1500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC1500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable22_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_GDP_1 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_1$group <- c("A")
df_GDP_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               #UTC1500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               UTC1500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_GDP_2 <- data.frame(cbind(cfint, coef, `pval`))
df_GDP_2$group <- c("B")
df_GDP_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
               UTC1500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
               #UTC1500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
               UTC1500:ifelse(GDP > 15000, 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_GDP_3 <- data.frame(cbind(cfint, coef, pval))
df_GDP_3$group <- c("C")
df_GDP_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(GDP > 1 & GDP <= 5000 , 1, 0) + 
                UTC1500:ifelse(GDP > 5000 & GDP <= 10000, 1, 0) + 
                UTC1500:ifelse(GDP > 10000 & GDP <= 15000, 1, 0) + 
                #UTC1500:ifelse(GDP > 15000, 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_GDP_4 <- data.frame(cbind(cfint, coef, pval))
df_GDP_4$group <- c("D")
df_GDP_4$tag <- "1"

############添加另一属性值############
df_GDP_1$character <- "<5000$"
df_GDP_2$character <- "5000$~10000$"
df_GDP_3$character <- "10000$~15000$"
df_GDP_4$character <- "> 15000$"

#年龄画图区域
df_combine_gdp <- rbind(
  df_GDP_1, df_GDP_2, df_GDP_3, df_GDP_4)
## 保存数据###


write.csv(df_combine_gdp,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_gdp.csv')





###################################### Supplementary Table 23 Heterogeneous CityScale################################################

model <-felm(sentiment ~ 1+ UTC500+
               #UTC500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable23_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_CityScale_1 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_1$group <- c("A")
df_CityScale_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               #UTC500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_CityScale_2 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_2$group <- c("B")
df_CityScale_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               #UTC500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_CityScale_3 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_3$group <- c("C")
df_CityScale_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
                UTC500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
                UTC500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
                #UTC500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_CityScale_4 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_4$group <- c("D")
df_CityScale_4$tag <- "1"


############添加另一属性值############
df_CityScale_1$character <- "Small city"
df_CityScale_2$character <- "Medium city"
df_CityScale_3$character <- "Large city"
df_CityScale_4$character <- "Megacity"

##############################################################################


#年龄画图区域
df_combine_CityScale <- rbind(
  df_CityScale_1, df_CityScale_2, df_CityScale_3, df_CityScale_4)
## 保存数据###

write.csv(df_combine_CityScale,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_CityScale.csv')




###################################### Supplementary Table 24 Heterogeneous CityScale################################################

model <-felm(sentiment ~ 1+ UTC1000+
               #UTC1000:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC1000:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC1000:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1000:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable24_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_CityScale_1 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_1$group <- c("A")
df_CityScale_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               #UTC1000:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC1000:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1000:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_CityScale_2 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_2$group <- c("B")
df_CityScale_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC1000:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               #UTC1000:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1000:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_CityScale_3 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_3$group <- c("C")
df_CityScale_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
                UTC1000:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
                UTC1000:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
                #UTC1000:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_CityScale_4 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_4$group <- c("D")
df_CityScale_4$tag <- "1"


############添加另一属性值############
df_CityScale_1$character <- "Small city"
df_CityScale_2$character <- "Medium city"
df_CityScale_3$character <- "Large city"
df_CityScale_4$character <- "Megacity"

##############################################################################


#年龄画图区域
df_combine_CityScale <- rbind(
  df_CityScale_1, df_CityScale_2, df_CityScale_3, df_CityScale_4)
## 保存数据###

write.csv(df_combine_CityScale,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_CityScale.csv')



###################################### Supplementary Table 25 Heterogeneous CityScale################################################

model <-felm(sentiment ~ 1+ UTC1500+
               #UTC1500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC1500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC1500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable25_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_CityScale_1 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_1$group <- c("A")
df_CityScale_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               #UTC1500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               UTC1500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_CityScale_2 <- data.frame(cbind(cfint, coef, `pval`))
df_CityScale_2$group <- c("B")
df_CityScale_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
               UTC1500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
               #UTC1500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
               UTC1500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_CityScale_3 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_3$group <- c("C")
df_CityScale_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(CityScale=="Small_sized_city(0-50)" , 1, 0) + 
                UTC1500:ifelse(CityScale=="Medium_sized_city(50-200)", 1, 0) + 
                UTC1500:ifelse(CityScale=="Large_sized_city(200-500)", 1, 0) + 
                #UTC1500:ifelse(CityScale=="Megacity(>500)", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_CityScale_4 <- data.frame(cbind(cfint, coef, pval))
df_CityScale_4$group <- c("D")
df_CityScale_4$tag <- "1"


############添加另一属性值############
df_CityScale_1$character <- "Small city"
df_CityScale_2$character <- "Medium city"
df_CityScale_3$character <- "Large city"
df_CityScale_4$character <- "Megacity"

##############################################################################


#年龄画图区域
df_combine_CityScale <- rbind(
  df_CityScale_1, df_CityScale_2, df_CityScale_3, df_CityScale_4)
## 保存数据###

write.csv(df_combine_CityScale,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_CityScale.csv')



##################################################UTC 500 season Supplementary  Table 26 ###################################################################



model <- felm(sentiment ~ 1+ UTC500+
                #UTC500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable26_Heterogeneous_UTC500_season.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_season_1 <- data.frame(cbind(cfint, coef, `pval`))
df_season_1$group <- c("A")
df_season_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                #UTC500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_season_2 <- data.frame(cbind(cfint, coef, `pval`))
df_season_2$group <- c("B")
df_season_2$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                #UTC500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_season_3 <- data.frame(cbind(cfint, coef, `pval`))
df_season_3$group <- c("C")
df_season_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                #UTC500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_season_4 <- data.frame(cbind(cfint, coef, `pval`))
df_season_4$group <- c("D")
df_season_4$tag <- "1"


#年龄画图区域
df_combine_season <- rbind(
  df_season_1,df_season_2,df_season_3,df_season_4)

## 保存数据###

write.csv(df_combine_season,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_season.csv')




##################################################UTC 500 season SupplementaryTable27###################################################################



model <- felm(sentiment ~ 1+ UTC1000+
                #UTC1000:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable27_Heterogeneous_UTC1000_season.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_season_1 <- data.frame(cbind(cfint, coef, `pval`))
df_season_1$group <- c("A")
df_season_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                #UTC1000:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_season_2 <- data.frame(cbind(cfint, coef, `pval`))
df_season_2$group <- c("B")
df_season_2$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                #UTC1000:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_season_3 <- data.frame(cbind(cfint, coef, `pval`))
df_season_3$group <- c("C")
df_season_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1000:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                #UTC1000:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_season_4 <- data.frame(cbind(cfint, coef, `pval`))
df_season_4$group <- c("D")
df_season_4$tag <- "1"


#年龄画图区域
df_combine_season <- rbind(
  df_season_1,df_season_2,df_season_3,df_season_4)

## 保存数据###

write.csv(df_combine_season,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_season.csv')



##################################################UTC 500 season SupplementaryTable28 ###################################################################



model <- felm(sentiment ~ 1+ UTC1500+
                #UTC1500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable28_Heterogeneous_UTC1500_season.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_season_1 <- data.frame(cbind(cfint, coef, `pval`))
df_season_1$group <- c("A")
df_season_1$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                #UTC1500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_season_2 <- data.frame(cbind(cfint, coef, `pval`))
df_season_2$group <- c("B")
df_season_2$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                #UTC1500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_season_3 <- data.frame(cbind(cfint, coef, `pval`))
df_season_3$group <- c("C")
df_season_3$tag <- "1"



model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(yearmonth == "Y2018M10" | yearmonth == "Y2018M11" | yearmonth == "Y2018M12" | yearmonth == "Y2019M10" | yearmonth == "Y2019M11" | yearmonth == "Y2019M12", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M01" | yearmonth == "Y2018M02" | yearmonth == "Y2018M03" | yearmonth == "Y2019M01" | yearmonth == "Y2019M02" | yearmonth == "Y2019M03", 1, 0) +  
                UTC1500:ifelse(yearmonth == "Y2018M04" | yearmonth == "Y2018M05" | yearmonth == "Y2018M06" | yearmonth == "Y2019M04" | yearmonth == "Y2019M05" | yearmonth == "Y2019M06", 1, 0) +  
                #UTC1500:ifelse(yearmonth == "Y2018M07" | yearmonth == "Y2018M08" | yearmonth == "Y2018M09" | yearmonth == "Y2019M07" | yearmonth == "Y2019M08" | yearmonth == "Y2019M09", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_season_4 <- data.frame(cbind(cfint, coef, `pval`))
df_season_4$group <- c("D")
df_season_4$tag <- "1"


#年龄画图区域
df_combine_season <- rbind(
  df_season_1,df_season_2,df_season_3,df_season_4)

## 保存数据###

write.csv(df_combine_season,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_season.csv')




###################################### Supplementary Table 29 Heterogeneous diffhours################################################

model <-felm(sentiment ~ 1+ UTC500+
               #UTC500:ifelse(diffhours == "00to06", 1, 0) + 
               UTC500:ifelse(diffhours == "06to09", 1, 0) + 
               UTC500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable29_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)




coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_1$group <- c("A")
df_diffhours5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(diffhours == "00to06", 1, 0) + 
               #UTC500:ifelse(diffhours == "06to09", 1, 0) + 
               UTC500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_2$group <- c("B")
df_diffhours5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(diffhours == "00to06", 1, 0) + 
               UTC500:ifelse(diffhours == "06to09", 1, 0) + 
               #UTC500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_3 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_3$group <- c("C")
df_diffhours5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC500:ifelse(diffhours == "09to12", 1, 0) + 
                #UTC500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_4 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_4$group <- c("D")
df_diffhours5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC500:ifelse(diffhours == "12to15", 1, 0) + 
                #UTC500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_5 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_5$group <- c("E")
df_diffhours5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC500:ifelse(diffhours == "15to18", 1, 0) + 
                #UTC500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_6 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_6$group <- c("F")
df_diffhours5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC500:ifelse(diffhours == "18to21", 1, 0) + 
                #UTC500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_diffhours5_7 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_7$group <- c("G")
df_diffhours5_7$tag <- "1"


############添加另一属性值############
df_diffhours5_1$character <- "00 am~06 am"
df_diffhours5_2$character <- "06 am~09 am"
df_diffhours5_3$character <- "09 am~12 am"
df_diffhours5_4$character <- "12 am~15 pm"
df_diffhours5_5$character <- "15 pm ~18 pm "
df_diffhours5_6$character <- "18 pm ~21 pm"
df_diffhours5_7$character <- "21 pm~24 pm"


#年龄画图区域
df_combine_hours <- rbind(
  df_diffhours5_1,df_diffhours5_2,df_diffhours5_3,df_diffhours5_4,df_diffhours5_5,df_diffhours5_6,df_diffhours5_7)

## 保存数据###

write.csv(df_combine_hours,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_hours.csv')



###################################### Supplementary Table 30 Heterogeneous diffhours################################################

model <-felm(sentiment ~ 1+ UTC1000+
               #UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
               UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
               UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable30_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)




coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_1$group <- c("A")
df_diffhours5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
               #UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
               UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_2$group <- c("B")
df_diffhours5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
               UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
               #UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_3 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_3$group <- c("C")
df_diffhours5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
                #UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_4 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_4$group <- c("D")
df_diffhours5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
                #UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_5 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_5$group <- c("E")
df_diffhours5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
                #UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_6 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_6$group <- c("F")
df_diffhours5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1000:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1000:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1000:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1000:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1000:ifelse(diffhours == "18to21", 1, 0) + 
                #UTC1000:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_diffhours5_7 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_7$group <- c("G")
df_diffhours5_7$tag <- "1"


############添加另一属性值############
df_diffhours5_1$character <- "00 am~06 am"
df_diffhours5_2$character <- "06 am~09 am"
df_diffhours5_3$character <- "09 am~12 am"
df_diffhours5_4$character <- "12 am~15 pm"
df_diffhours5_5$character <- "15 pm ~18 pm "
df_diffhours5_6$character <- "18 pm ~21 pm"
df_diffhours5_7$character <- "21 pm~24 pm"


#年龄画图区域
df_combine_hours <- rbind(
  df_diffhours5_1,df_diffhours5_2,df_diffhours5_3,df_diffhours5_4,df_diffhours5_5,df_diffhours5_6,df_diffhours5_7)

## 保存数据###

write.csv(df_combine_hours,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_hours.csv')




###################################### Supplementary Table 31 Heterogeneous diffhours################################################

model <-felm(sentiment ~ 1+ UTC1500+
               #UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
               UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
               UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable31_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)




coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_1$group <- c("A")
df_diffhours5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
               #UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
               UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_diffhours5_2$group <- c("B")
df_diffhours5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
               UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
               #UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
               UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
               UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
               UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
               UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_3 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_3$group <- c("C")
df_diffhours5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
                #UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)


coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_4 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_4$group <- c("D")
df_diffhours5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
                #UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_5 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_5$group <- c("E")
df_diffhours5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
                #UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
                UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_6 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_6$group <- c("F")
df_diffhours5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(diffhours == "00to06", 1, 0) + 
                UTC1500:ifelse(diffhours == "06to09", 1, 0) + 
                UTC1500:ifelse(diffhours == "09to12", 1, 0) + 
                UTC1500:ifelse(diffhours == "12to15", 1, 0) + 
                UTC1500:ifelse(diffhours == "15to18", 1, 0) + 
                UTC1500:ifelse(diffhours == "18to21", 1, 0) + 
                #UTC1500:ifelse(diffhours == "21to24", 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_diffhours5_7 <- data.frame(cbind(cfint, coef, pval))
df_diffhours5_7$group <- c("G")
df_diffhours5_7$tag <- "1"


############添加另一属性值############
df_diffhours5_1$character <- "00 am~06 am"
df_diffhours5_2$character <- "06 am~09 am"
df_diffhours5_3$character <- "09 am~12 am"
df_diffhours5_4$character <- "12 am~15 pm"
df_diffhours5_5$character <- "15 pm ~18 pm "
df_diffhours5_6$character <- "18 pm ~21 pm"
df_diffhours5_7$character <- "21 pm~24 pm"


#年龄画图区域
df_combine_hours <- rbind(
  df_diffhours5_1,df_diffhours5_2,df_diffhours5_3,df_diffhours5_4,df_diffhours5_5,df_diffhours5_6,df_diffhours5_7)

## 保存数据###

write.csv(df_combine_hours,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_hours.csv')




###################################### Supplementary Table 32 Heterogeneous MeanTemperature Interval is 2################################################

model <-felm(sentiment ~ 1+ UTC500+
               #UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable32_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_1$group <- c("A")
df_Skintemp5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_2$group <- c("B")
df_Skintemp5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC500+
               UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_3 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_3$group <- c("C")
df_Skintemp5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_4 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_4$group <- c("D")
df_Skintemp5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) +  
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_5 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_5$group <- c("E")
df_Skintemp5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
                UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_6 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_6$group <- c("F")
df_Skintemp5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_7 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_7$group <- c("G")
df_Skintemp5_7$tag <- "1"


model <- felm(sentiment ~ 1+ UTC500+
                UTC500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               #UTC500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC500", "Pr(>|t|)"]
df_Skintemp5_8 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_8$group <- c("H")
df_Skintemp5_8$tag <- "1"


############添加另一属性值############
df_Skintemp5_1$character <- "< 20°C"
df_Skintemp5_2$character <- "20°C~22°C"
df_Skintemp5_3$character <- "22°C~24°C"
df_Skintemp5_4$character <- "24°C~26°C"
df_Skintemp5_5$character <- "26°C~28°C"
df_Skintemp5_6$character <- "28°C~30°C"
df_Skintemp5_7$character <- "30°C~32°C"
df_Skintemp5_8$character <- "> 32°C"



#年龄画图区域
df_combine_Skintemp2 <- rbind(
  df_Skintemp5_1, df_Skintemp5_2, df_Skintemp5_3, df_Skintemp5_4, df_Skintemp5_5, df_Skintemp5_6, df_Skintemp5_7, df_Skintemp5_8 )
## 保存数据###

write.csv(df_combine_Skintemp2,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/UTC500_combine_Skintemp_2Interval.csv')





###################################### Supplementary Table 33 Heterogeneous MeanTemperature Interval is 2################################################

model <-felm(sentiment ~ 1+ UTC1000+
               #UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable33_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_1$group <- c("A")
df_Skintemp5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_2$group <- c("B")
df_Skintemp5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1000+
               UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_3 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_3$group <- c("C")
df_Skintemp5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_4 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_4$group <- c("D")
df_Skintemp5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) +  
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_5 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_5$group <- c("E")
df_Skintemp5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
                UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_6 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_6$group <- c("F")
df_Skintemp5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_7 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_7$group <- c("G")
df_Skintemp5_7$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1000+
                UTC1000:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1000:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               #UTC1000:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1000", "Pr(>|t|)"]
df_Skintemp5_8 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_8$group <- c("H")
df_Skintemp5_8$tag <- "1"


############添加另一属性值############
df_Skintemp5_1$character <- "< 20°C"
df_Skintemp5_2$character <- "20°C~22°C"
df_Skintemp5_3$character <- "22°C~24°C"
df_Skintemp5_4$character <- "24°C~26°C"
df_Skintemp5_5$character <- "26°C~28°C"
df_Skintemp5_6$character <- "28°C~30°C"
df_Skintemp5_7$character <- "30°C~32°C"
df_Skintemp5_8$character <- "> 32°C"



#年龄画图区域
df_combine_Skintemp2 <- rbind(
  df_Skintemp5_1, df_Skintemp5_2, df_Skintemp5_3, df_Skintemp5_4, df_Skintemp5_5, df_Skintemp5_6, df_Skintemp5_7, df_Skintemp5_8 )
## 保存数据###

write.csv(df_combine_Skintemp2,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1000_heterogeneity20240606/UTC1000_combine_Skintemp_2Interval.csv')



###################################### Supplementary Table 34 Heterogeneous MeanTemperature Interval is 2################################################

model <-felm(sentiment ~ 1+ UTC1500+
               #UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable34_Heterogeneous.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)



coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_1 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_1$group <- c("A")
df_Skintemp5_1$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,   data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_2 <- data.frame(cbind(cfint, coef, `pval`))
df_Skintemp5_2$group <- c("B")
df_Skintemp5_2$tag <- "1"


model <-felm(sentiment ~ 1+ UTC1500+
               UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
               Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
               MeanTemperature+
               settlement +nightlight +population
             |yearmonthday+userid+ CityName ,  data =query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_3 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_3$group <- c("C")
df_Skintemp5_3$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_4 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_4$group <- c("D")
df_Skintemp5_4$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) +  
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_5 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_5$group <- c("E")
df_Skintemp5_5$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
                UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2 )

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_6 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_6$group <- c("F")
df_Skintemp5_6$tag <- "1"

model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_7 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_7$group <- c("G")
df_Skintemp5_7$tag <- "1"


model <- felm(sentiment ~ 1+ UTC1500+
                UTC1500:ifelse(MeanTemperature<= 20, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>20 & MeanTemperature<= 22, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>22 & MeanTemperature<= 24, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>24 & MeanTemperature<= 26, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>26 & MeanTemperature<= 28, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>28 & MeanTemperature<= 30, 1, 0) + 
               UTC1500:ifelse(MeanTemperature>30 & MeanTemperature<= 32, 1, 0) + 
               #UTC1500:ifelse(MeanTemperature>32 , 1, 0) + 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName ,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")
pval<-summary(model)$coefficients["UTC1500", "Pr(>|t|)"]
df_Skintemp5_8 <- data.frame(cbind(cfint, coef, pval))
df_Skintemp5_8$group <- c("H")
df_Skintemp5_8$tag <- "1"


############添加另一属性值############
df_Skintemp5_1$character <- "< 20°C"
df_Skintemp5_2$character <- "20°C~22°C"
df_Skintemp5_3$character <- "22°C~24°C"
df_Skintemp5_4$character <- "24°C~26°C"
df_Skintemp5_5$character <- "26°C~28°C"
df_Skintemp5_6$character <- "28°C~30°C"
df_Skintemp5_7$character <- "30°C~32°C"
df_Skintemp5_8$character <- "> 32°C"



#年龄画图区域
df_combine_Skintemp2 <- rbind(
  df_Skintemp5_1, df_Skintemp5_2, df_Skintemp5_3, df_Skintemp5_4, df_Skintemp5_5, df_Skintemp5_6, df_Skintemp5_7, df_Skintemp5_8 )
## 保存数据###

write.csv(df_combine_Skintemp2,'D:/Sentiment_Brazil/R_codes/NewCodes/UTC1500_heterogeneity20240606/UTC1500_combine_Skintemp_2Interval.csv')




