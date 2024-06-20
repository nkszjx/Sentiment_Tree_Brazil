
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
##############################################主模型UTC 500###########################################################################


model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))




# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/Table1ResultsUTC500.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


# 获取系数和置信度
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC500"),level=0.95)
colnames(cfint) <- c("min","max")

# 计算VIF
vif_values <- vif(model)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)




#  工具变量法采用透水地表数据作为工具变量
## 先回归UTC 
model <- felm(UTC500 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)



## 工具变量模型
model <- felm(sentiment ~ 1+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid|(UTC500~unimpervious)|CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#  工具变量法 采用透水地表数据作为工具变量
model <- felm(UTC500 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)



#################################################主模型UTC 1000###########################################################################


model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable2ResultsUTC1000.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


# 获取系数和置信度
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")

# 计算VIF
vif_values <- vif(model)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)




#  工具变量法采用透水地表数据作为工具变量
## 先回归UTC 
model <- felm(UTC1000 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)



## 工具变量模型
model <- felm(sentiment ~ 1+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid|(UTC1000~unimpervious)|CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#  工具变量法 采用透水地表数据作为工具变量
model <- felm(UTC1000 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)

#################################################主模型UTC1500###########################################################################


model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable3ResultsUTC1500.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


# 获取系数和置信度
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1500"),level=0.95)
colnames(cfint) <- c("min","max")

# 计算VIF
vif_values <- vif(model)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)




#  工具变量法采用透水地表数据作为工具变量
## 先回归UTC 
model <- felm(UTC1500 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)



## 工具变量模型
model <- felm(sentiment ~ 1+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid|(UTC1500~unimpervious)|CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#  工具变量法 采用透水地表数据作为工具变量
model <- felm(UTC1500 ~ unimpervious+ 
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population|yearmonthday+userid+ CityName,  data =  query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)





####################################### UTC 分段回归 Binned estimates Supplementary Table 4. #######################################

# 对UTC500进行分段并创建因子变量
query_sample2$UTC500_segment <- cut(query_sample2$UTC500, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1.0), 
                                    labels = c("UTC500_0.1", "UTC500_0.2", "UTCUTC500_0.3", "UTC500_0.4", "UTC500_0.5"))

# 使用分段后的UTC500变量进行分段回归
model <- felm(sentiment ~ 1 + UTC500_segment+
                Precipitation + humidity + Wind + cloudcover + Surface_pressure +
                MeanTemperature + settlement + nightlight + population |
                yearmonthday + userid + CityName, data = query_sample2)

# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable4_Binned_estimates.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


# 对UTC1000进行分段并创建因子变量
query_sample2$UTC1000_segment <- cut(query_sample2$UTC1000, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1.0), 
                                    labels = c("UTC1000_0.1", "UTC1000_0.2", "UTCUTC1000_0.3", "UTC1000_0.4", "UTC1000_0.5"))

# 使用分段后的UTC1000变量进行分段回归
model <- felm(sentiment ~ 1 + UTC1000_segment+
                Precipitation + humidity + Wind + cloudcover + Surface_pressure +
                MeanTemperature + settlement + nightlight + population |
                yearmonthday + userid + CityName, data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


# 对UTC1500进行分段并创建因子变量
query_sample2$UTC1500_segment <- cut(query_sample2$UTC1500, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1.0), 
                                    labels = c("UTC1500_0.1", "UTC1500_0.2", "UTCUTC1500_0.3", "UTC1500_0.4", "UTC1500_0.5"))

# 使用分段后的UTC1500变量进行分段回归
model <- felm(sentiment ~ 1 + UTC1500_segment+
                Precipitation + humidity + Wind + cloudcover + Surface_pressure +
                MeanTemperature + settlement + nightlight + population |
                yearmonthday + userid + CityName, data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)





####################################### Supplementary Table5 Robustness_Test. #######################################


model <- felm(sentiment ~ 1+ UTC500 
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable5_Robustness_Test.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件


model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


####################################### Supplementary Table6 Robustness_Test. #######################################



model <- felm(sentiment ~ 1+ UTC1000 
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable6_Robustness_Test.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件


model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


####################################### Supplementary Table7 Robustness_Test. #######################################

model <- felm(sentiment ~ 1+ UTC1500 
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable7_Robustness_Test.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件


model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)





#################################### Supplementary Table 8 Robustness_Test ################################

query_sample2018 <- query_sample2[query_sample2$yearmonth %in% c("Y2018M01", "Y2018M02", "Y2018M03","Y2018M04","Y2018M05", "Y2018M06", 
                                                       "Y2018M07", "Y2018M08", "Y2018M09", "Y2018M10","Y2018M11", "Y2018M12"),] # yearmonth summer

query_sample2019 <- query_sample2[query_sample2$yearmonth %in% c("Y2019M01", "Y2019M02", "Y2019M03","Y2019M04","Y2019M05", "Y2019M06", 
                                                     "Y2019M07", "Y2019M08", "Y2019M09", "Y2019M10","Y2019M11", "Y2019M12"),] # yearmonth summer


# 选取2018数据进行实验

model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2018)


# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable8_Robustness_Test.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件



# 选取2019数据进行实验
model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2019)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


# 选取2018数据进行实验

model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2018)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

# 选取2019数据进行实验
model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2019)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


# 选取2018数据进行实验

model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2018)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

# 选取2019数据进行实验
model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2019)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)



######################################### Supplementary Table 9 Robustness_Test #############################

model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>50)))


# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable9_Robustness_Test.txt"
con <- file(output_file, open = "a") # 打开文件连接
model_summary <- capture.output(summary(model)) # 捕获模型摘要的输出
writeLines(model_summary, con = output_file) # 将模型摘要写入文件
# 关闭文件连接
#close(con)


model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>100)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)



model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>150)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)




model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>200)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>250)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>300)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)





######################################### Supplementary Table 10 Robustness_Test #############################

model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>50)))


# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable10_Robustness_Test.txt"
con <- file(output_file, open = "a") # 打开文件连接
model_summary <- capture.output(summary(model)) # 捕获模型摘要的输出
writeLines(model_summary, con = output_file) # 将模型摘要写入文件
# 关闭文件连接
#close(con)


model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>100)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)



model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>150)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)




model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>200)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>250)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC1000 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>300)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)



######################################### Supplementary Table 11 Robustness_Test #############################

model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>50)))


# 查看模型摘要
print(summary(model))

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable11_Robustness_Test.txt"
con <- file(output_file, open = "a") # 打开文件连接
model_summary <- capture.output(summary(model)) # 捕获模型摘要的输出
writeLines(model_summary, con = output_file) # 将模型摘要写入文件
# 关闭文件连接
#close(con)


model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>100)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)



model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>150)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)




model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>200)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>250)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


model <- felm(sentiment ~ 1+ UTC1500 +
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = subset(query_sample4, (useridtotalnumber>300)))


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)




####################################### nonlinear regression model Supplementary Table 12. #######################################


model <- felm(sentiment ~ 1+ UTC500 + UTC500_2+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))
# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/NewCodes/SupplementaryTable12_nonlinear_regression.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)

#############################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000_2+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


#############################

model <- felm(sentiment ~ 1+ UTC1500 +UTC1500_2+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                MeanTemperature+
                settlement +nightlight +population
              |yearmonthday+userid+ CityName,  data = query_sample2)
# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
close(con)


