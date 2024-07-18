
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
library(readxl)

## 地图绘制函数包
library(ggplot2)
library(ggspatial)
library(sf)
library(maps)
library(sp)
library(tmap)
library(viridis) 
library(ggpmisc)
library(ggpointdensity)
library(lubridate)
library(here)
library(showtext)

# 图形合并布局函数包
library(cowplot)

library(patchwork)
# 图像读取
library(ggplot2)
library(jpeg)
library(ggpubr)
library(patchwork)
library(tiff)

#################################################################################################################################################################
#################################################################################################################################################################

# 获取巴西地图集
#Brazil<-  world_map[world_map$region %in% c( "Brazil"), ]

Brazil <- st_read("D:/Sentiment_Brazil/R_codes/NewCodes/brazil-administrative-boundary/04_limiteestadual1991.shp")


## ## ## ## ## ## ## 读取数据## ## ## ## ## ## ## ## ## ## ## 

str0='D:/Sentiment_Brazil/R_codes/NewCodes/'


file_CITY='Brazil_citiy_Information'

input_gini= paste( str0, file_CITY , '.csv', sep="") # 字符串连接
Mat = read.csv(input_gini, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE) 

# 创建数据框架
anno_df <- data.frame(lon = Mat$Lon,
                      lat = Mat$Lat,
                      cityarea = Mat$CityScale)

#转换对象
anno_sf <- st_as_sf(anno_df, coords = c("lon", "lat"),crs = 4326)

anno_sf$cityarea <- factor(anno_sf$cityarea, levels = c("Small_sized_city(0-50)", "Medium_sized_city(50-200)", "Large_sized_city(200-500)", "Megacity(>500)"))

## ## ## ## ## ## ## # 绘制地图## ## ## ## ## ## ## ## ## ## ## 

#####
ggplot() +
  geom_sf(data = Brazil)+ # 绘制底图
  ggtitle("Federative Republic of Brazil") +  # 标题
  theme_minimal() +  # 使用简洁的主题
  theme(plot.title = element_text(hjust = 0.5),  # 标题居中
        axis.text = element_text(size = 15),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),   # 设置坐标轴文字的大小
        legend.text = element_text(size = 15),  # 设置图例文字的大小
        legend.title = element_text(size = 15)) +  # 设置图例标题的大小
  geom_sf(data = anno_sf, aes(fill=cityarea),shape=21,colour='black',size = 3, stroke=0.55)+  #添加散点注释
  scale_size(range = c(0.1,4))+ # 调整注释大小
  xlab("Longitude") +   # X轴标签
  ylab("Latitude")+ # Y轴标签
  
  scale_fill_manual(
    values = c("Small_sized_city(0-50)" = "gray", 
               "Medium_sized_city(50-200)" = "#129333", 
               "Large_sized_city(200-500)" = "blue", 
               "Megacity(>500)" = "#F90000"),
    name = "City level", # 设置图例标题
    labels = c(expression("Small City (0~50" ~ km^2 ~ ")"),
               expression("Medium City (50~200" ~ km^2 ~ ")"),
               expression("Large City (200~500" ~ km^2 ~ ")"),
               expression("Megacity (>500" ~ km^2 ~ ")")) # 设置图例标签
               )+  
  annotation_scale(width_hint=0.2, location = "bl", pad_x = unit(4.5, "in"), pad_y = unit(0.3, "in"), style='ticks', text_cex=1.5) + # 比例尺
  annotation_north_arrow(location = "tr", which_north = "false",
                         style = north_arrow_fancy_orienteering(text_size = 15))->map_Brazil# 设置指北针文字的大小


print(map_Brazil)



ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_cityscle.pdf", plot = map_Brazil, width = 300, height = 240, units = "mm",dpi=300)

#################################################################################################################################
############################################################################################################################################################################








