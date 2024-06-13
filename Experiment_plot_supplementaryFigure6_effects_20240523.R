

#rm(list=ls())  # 这行代码是清空当前工作环境中的所有对象，确保你开始一个干净的工作空间

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
library(ggpubr)
library(ggplot2)
library(readxl)

##load and read data
#cr<-read_excel("D:/Sentiment_Brazil/R_codes/data01.xlsx",sheet  = 1)
#cr<-read_excel("D:/Sentiment_Brazil/R_codes/Surroundinrisk_dataset.xlsx",sheet  = 2)




######################################UTC500 读取效应值 #######################
tree_redious='500' # 根据实际情况修改半径
str0='D:/Sentiment_Brazil/R_codes/NewCodes/Effect202400508/'
input_file_ME_up= paste( str0,  'UTC', tree_redious,  '_E_upr', '.csv', sep="") # 字符串连接
input_file_ME= paste( str0,  'UTC', tree_redious,  '_E', '.csv', sep="")
input_file_ME_low= paste( str0,  'UTC', tree_redious,  '_E_lwr', '.csv', sep="") # 字符串连接

ME_up = read.csv(input_file_ME_up, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                 strip.white = FALSE)
ME = read.csv(input_file_ME, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
              strip.white = FALSE)
ME_low = read.csv(input_file_ME_low, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                  strip.white = FALSE)

# 获取均值
ME_value<-ME[,2]

# 上限值
UP<- ME_up[,2]
# 下限值
Low<- ME_low[,2]

# 获取取值UTC 0-1，1000个数值
UTC_value<-ME[,1]/1000

# 构建数据框
Data_effects_500<-cbind(UTC_value, ME_value, UP, Low)


######################################UTC1000 读取效应值 #######################
tree_redious='1000' # 根据实际情况修改半径
str0='D:/Sentiment_Brazil/R_codes/NewCodes/Effect202400508/'
input_file_ME_up= paste( str0,  'UTC', tree_redious,  '_E_upr', '.csv', sep="") # 字符串连接
input_file_ME= paste( str0,  'UTC', tree_redious,  '_E', '.csv', sep="")
input_file_ME_low= paste( str0,  'UTC', tree_redious,  '_E_lwr', '.csv', sep="") # 字符串连接

ME_up = read.csv(input_file_ME_up, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                 strip.white = FALSE)
ME = read.csv(input_file_ME, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
              strip.white = FALSE)
ME_low = read.csv(input_file_ME_low, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                  strip.white = FALSE)

# 获取均值
ME_value<-ME[,2]

# 上限值
UP<- ME_up[,2]
# 下限值
Low<- ME_low[,2]

# 获取取值UTC 0-1，1000个数值
UTC_value<-ME[,1]/1000

# 构建数据框
Data_effects_1000<-cbind(UTC_value, ME_value, UP, Low)



######################################UTC1500 读取效应值 #######################
tree_redious='1500' # 根据实际情况修改半径
str0='D:/Sentiment_Brazil/R_codes/NewCodes/Effect202400508/'
input_file_ME_up= paste( str0,  'UTC', tree_redious,  '_E_upr', '.csv', sep="") # 字符串连接
input_file_ME= paste( str0,  'UTC', tree_redious,  '_E', '.csv', sep="")
input_file_ME_low= paste( str0,  'UTC', tree_redious,  '_E_lwr', '.csv', sep="") # 字符串连接

ME_up = read.csv(input_file_ME_up, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                 strip.white = FALSE)
ME = read.csv(input_file_ME, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
              strip.white = FALSE)
ME_low = read.csv(input_file_ME_low, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                  strip.white = FALSE)

# 获取均值
ME_value<-ME[,2]

# 上限值
UP<- ME_up[,2]
# 下限值
Low<- ME_low[,2]

# 获取取值UTC 0-1，1000个数值
UTC_value<-ME[,1]/1000

# 构建数据框
Data_effects_1500<-cbind(UTC_value, ME_value, UP, Low)


############################################# 读取2018年数据###########################################################
## 数据路径

str_year='2018'

str0='D:/Sentiment_Brazil/brazil_twitter_points_2018-2022_sentiment/year'
file_str0=paste(str0, str_year, '/', sep="")
file_str=paste(str0, str_year, '/DataDrivingFiles/', sep="")

##情感数据名称

sentiment_UTC_data='brazil_twitter_points_UTC_'

input_file_name= paste(file_str0, sentiment_UTC_data, str_year, '.csv', sep="")  # 字符串连接
Lat_sentiment_TUC = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                             strip.white = FALSE)

print(nrow(Lat_sentiment_TUC))


# 定义要替换的值
old_value <- -1
new_value <- 0.25

# 使用索引将第三列中值为 old_value 的元素替换为 new_value
Lat_sentiment_TUC[, 5][Lat_sentiment_TUC[, 5] == old_value] <- NaN
Lat_sentiment_TUC[, 6][Lat_sentiment_TUC[, 6] == old_value] <- NaN
Lat_sentiment_TUC[, 7][Lat_sentiment_TUC[, 7] == old_value] <- NaN

# 获取具体值
# 获取具体值
#Lat<-Lat_sentiment_TUC[,2]
#Lon<-Lat_sentiment_TUC[,3]
sentiment<-Lat_sentiment_TUC[,4]
UTC500<-Lat_sentiment_TUC[,5]
UTC1000<-Lat_sentiment_TUC[,6]
UTC1500<-Lat_sentiment_TUC[,7]


###########################读取用户和地理数据##########################################


## 数据读取函数：
loadcsv2<-function(data_dir, input_file_name){
  
  input_file_name = paste(data_dir, input_file_name, '.csv', sep='')
  #print(paste('start a regression:',input_file_name))
  
  fea_poi_all = read.csv(file=input_file_name, 
                         header=FALSE, sep=',', fill=TRUE, blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
  return(fea_poi_all)
}

## 数据名称 list:

useridtotalnumber=paste('brazil_twitter_points_', str_year,'_useridtotalnumber_all1819', sep='')
Geography=paste('brazil_twitter_points_', str_year,'_Geography', sep='')



## 读取数据集
useridtotalnumber = loadcsv2(file_str, useridtotalnumber)
names(useridtotalnumber)[1] <- "useridtotalnumber"
Geography = loadcsv2(file_str, Geography)
names(Geography)[1] <- "Geography"


# 各个变量按列合并
query_sample0_2018 <- cbind( sentiment, Geography,
                             UTC500,UTC1000, UTC1500, useridtotalnumber)

rm(sentiment, Geography,
   UTC500,UTC1000, UTC1500, useridtotalnumber)

############################################# 读取2019年数据###########################################################

str_year='2019'

str0='D:/Sentiment_Brazil/brazil_twitter_points_2018-2022_sentiment/year'
file_str0=paste(str0, str_year, '/', sep="")
file_str=paste(str0, str_year, '/DataDrivingFiles/', sep="")

##情感数据名称

sentiment_UTC_data='brazil_twitter_points_UTC_'

input_file_name= paste(file_str0, sentiment_UTC_data, str_year, '.csv', sep="")  # 字符串连接
Lat_sentiment_TUC = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                             strip.white = FALSE)

print(nrow(Lat_sentiment_TUC))


# 定义要替换的值
old_value <- -1
new_value <- 0.25

# 使用索引将第三列中值为 old_value 的元素替换为 new_value
Lat_sentiment_TUC[, 5][Lat_sentiment_TUC[, 5] == old_value] <- NaN
Lat_sentiment_TUC[, 6][Lat_sentiment_TUC[, 6] == old_value] <- NaN
Lat_sentiment_TUC[, 7][Lat_sentiment_TUC[, 7] == old_value] <- NaN


# 获取具体值
#Lat<-Lat_sentiment_TUC[,2]
#Lon<-Lat_sentiment_TUC[,3]
sentiment<-Lat_sentiment_TUC[,4]
UTC500<-Lat_sentiment_TUC[,5]
UTC1000<-Lat_sentiment_TUC[,6]
UTC1500<-Lat_sentiment_TUC[,7]


############读取用户和地理数据#################################


## 数据读取函数：
loadcsv2<-function(data_dir, input_file_name){
  
  input_file_name = paste(data_dir, input_file_name, '.csv', sep='')
  #print(paste('start a regression:',input_file_name))
  
  fea_poi_all = read.csv(file=input_file_name, 
                         header=FALSE, sep=',', fill=TRUE, blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
  return(fea_poi_all)
}


## 数据名称 list:
useridtotalnumber=paste('brazil_twitter_points_', str_year,'_useridtotalnumber_all1819', sep='')
Geography=paste('brazil_twitter_points_', str_year,'_Geography', sep='')


## 读取数据集
useridtotalnumber = loadcsv2(file_str, useridtotalnumber)
names(useridtotalnumber)[1] <- "useridtotalnumber"
Geography = loadcsv2(file_str, Geography)
names(Geography)[1] <- "Geography"



# 各个变量按列合并
query_sample0_2019 <- cbind( sentiment, Geography,
                             UTC500,UTC1000, UTC1500, useridtotalnumber)
rm(sentiment, Geography,
   UTC500,UTC1000, UTC1500,  useridtotalnumber)



################################## 合并2018年和 2019年数据######################################################

query_sample0 <- rbind(query_sample0_2018, query_sample0_2019)


rm(query_sample0_2018, query_sample0_2019) #清空变量

## 去除空值
query_sample0<-na.omit(query_sample0)


## 去除异常值，比如 sentiment<0.4, 但是UTC>0.2; sentiment >0.85 & UTC500<0.05
query_sample0<-subset(query_sample0, !(sentiment < 0.3 & UTC500>0.2) )
query_sample0<-subset(query_sample0, !(sentiment >0.9 & UTC500<0.05))

## 所有Brazil满足要求的数据
query_sample0 = subset(query_sample0, (useridtotalnumber>100) ) # 过滤tweet文本过少的数据

Data_NorthBrazil<- query_sample0[query_sample0$Geography %in% c( "NorthBrazil"),]
Data_NorthesatBrazil<- query_sample0[query_sample0$Geography %in% c( "NorthesatBrazil","NortheastBrazil" ), ]
Data_SoutheastBrazil<- query_sample0[query_sample0$Geography %in% c( "SoutheastBrazil"), ]
Data_CentralBrazil<- query_sample0[query_sample0$Geography %in% c( "CentralBrazil"), ]
Data_SouthBrazil<-  query_sample0[query_sample0$Geography %in% c( "SouthBrazil"), ]




###############################基于构建数据绘图##########################################################


theme0<-theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 12),
              strip.text.x = element_text(size = 12),
              strip.text.y = element_text(),
              strip.background = element_rect( fill="white",colour = "black"))
########NDVI+LBW



Threshold1<-0.6
Threshold2<- -0.15

###################################### UTC500绘制效应、边际效应图和数据分布特征###########################################


ggplot(data = Data_effects_500)+
  geom_ribbon(aes(x=UTC_value, y=ME_value, ymin=Low, ymax=UP),fill="#002C5B",alpha=0.4)+
  geom_line(aes(x=UTC_value, y=ME_value),colour="#002C5B88",size=0.5)+
  #annotate("text",x=0.2,y=0.4,size=4,color="grey20",hjust=0,label=expression("Whole Brazil"))+
  facet_grid(~"UTC (500 m)", scales = "free",labeller = labeller(exposure=label_parsed))+
  #annotate("text",x=0.21,y=0.35,size=4,color="grey20",hjust=0,label=expression("" *italic(P)* " < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  scale_y_continuous(breaks=seq(-0.00, 0.08,by=0.02),
                     labels=sprintf("%.02f",seq(-0.00,0.08,0.02)))+
  labs(title = "",y="Effect on sentiment",x="")+
  theme_classic()+theme0+
  theme(axis.text.x = element_blank(),
        strip.background.x = element_rect(color = "white",fill = "#002C5B40"),
        plot.margin = margin(b=-0.2,t=0.2,r=0.2,l=0.2,unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1), ylim = c(-0.01, 0.08))->p1e500 ## 可视范围限制

print(p1e500)


ggplot(data = query_sample0)+
  #geom_boxplot(aes(x=UTC500,y=-0.5),width=0.24,fill="#002C5B44",color="#002C5B88",outlier.shape = NA)+
  geom_density(aes(x=UTC500),fill="#002C5B66",
               color="white",show.legend = F)+
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  labs(x=expression("Tree cover"),y="Kernel density (%)")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1))->p500 # 可视范围限制在 0 到 0.5 的范围内

print(p500)




###################################### UTC1000绘制效应、边际效应图和数据分布特征###########################################


ggplot(data = Data_effects_1000)+
  geom_ribbon(aes(x=UTC_value, y=ME_value, ymin=Low, ymax=UP),fill="#002C5B",alpha=0.4)+
  geom_line(aes(x=UTC_value, y=ME_value),colour="#002C5B88",size=0.5)+
  #annotate("text",x=0.2,y=0.4,size=4,color="grey20",hjust=0,label=expression("Whole Brazil"))+
  facet_grid(~"UTC (1000 m)", scales = "free",labeller = labeller(exposure=label_parsed))+
  #annotate("text",x=0.21,y=0.35,size=4,color="grey20",hjust=0,label=expression("" *italic(P)* " < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  scale_y_continuous(breaks=seq(-0.00, 0.08,by=0.02),
                     labels=sprintf("%.02f",seq(-0.00,0.08,0.02)))+
  labs(title = "",y="Effect on sentiment",x="")+
  theme_classic()+theme0+
  theme(axis.text.x = element_blank(),
        strip.background.x = element_rect(color = "white",fill = "#002C5B40"),
        plot.margin = margin(b=-0.2,t=0.2,r=0.2,l=0.2,unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1), ylim = c(-0.01, 0.08))->p1e1000 ## 可视范围限制

print(p1e1000)


ggplot(data = query_sample0)+
  #geom_boxplot(aes(x=UTC500,y=-0.5),width=0.24,fill="#002C5B44",color="#002C5B88",outlier.shape = NA)+
  geom_density(aes(x=UTC1000),fill="#002C5B66",
               color="white",show.legend = F)+
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  labs(x=expression("Tree cover"),y="Kernel density (%)")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1))->p1000 # 可视范围限制在 0 到 0.5 的范围内

print(p1000)



###################################### UTC1500绘制效应、边际效应图和数据分布特征###########################################


ggplot(data = Data_effects_1500)+
  geom_ribbon(aes(x=UTC_value, y=ME_value, ymin=Low, ymax=UP),fill="#002C5B",alpha=0.4)+
  geom_line(aes(x=UTC_value, y=ME_value),colour="#002C5B88",size=0.5)+
  #annotate("text",x=0.2,y=0.4,size=4,color="grey20",hjust=0,label=expression("Whole Brazil"))+
  facet_grid(~"UTC (1500 m)", scales = "free",labeller = labeller(exposure=label_parsed))+
  #annotate("text",x=0.21,y=0.35,size=4,color="grey20",hjust=0,label=expression("" *italic(P)* " < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  scale_y_continuous(breaks=seq(-0.0, 0.08,by=0.02),
                     labels=sprintf("%.02f",seq(-0.00,0.08,0.02)))+
  labs(title = "",y="Effect on sentiment",x="")+
  theme_classic()+theme0+
  theme(axis.text.x = element_blank(),
        strip.background.x = element_rect(color = "white",fill = "#002C5B40"),
        plot.margin = margin(b=-0.2,t=0.2,r=0.2,l=0.2,unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1), ylim = c(-0.01, 0.08))->p1e1500 ## 可视范围限制

print(p1e1500)


ggplot(data = query_sample0)+
  #geom_boxplot(aes(x=UTC500,y=-0.5),width=0.24,fill="#002C5B44",color="#002C5B88",outlier.shape = NA)+
  geom_density(aes(x=UTC1500),fill="#002C5B66",
               color="white",show.legend = F)+
  scale_x_continuous(breaks = seq(0,Threshold1,0.1),
                     labels = seq(0,Threshold1,0.1))+
  labs(x=expression("Tree cover"),y="Kernel density (%)")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 12),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 12)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )+
  coord_cartesian(xlim = c(0, Threshold1))->p1500 # 可视范围限制在 0 到 0.5 的范围内

print(p1500)

##################################合并绘图，并保存数据#######################################
aa<-ggarrange(p1e500, p500, ncol = 1,align = "v",heights = c(1,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(aa)

bb<-ggarrange(p1e1000, p1000, ncol = 1,align = "v",heights = c(1,1))
cc<-ggarrange(p1e1500, p1500, ncol = 1,align = "v",heights = c(1,1))

p1<-ggarrange(aa,bb,cc, nrow = 1,ncol = 3, labels = c("a","b","c"), align = "v" )
print(p1)

#mergeplot3 <- cowplot::plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1), rel_widths = c(1, 1)) # rel_heights 参数图形高度设置
#c(2.9, 3.2, 3.2, 2.7)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_geo_effects_UTC500_1000_1500_20240523.pdf", plot = p1, width = 300, height = 150, units = "mm",dpi=300)


