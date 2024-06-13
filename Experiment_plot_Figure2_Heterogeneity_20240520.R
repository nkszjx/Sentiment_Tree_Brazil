


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



######################################## 读取数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_hours'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_hours= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)



Newlabnames_hours<-c("00 am~06 am", "06 am~09 am", "09 am~12 am", "12 am~15 pm", "15 pm ~18 pm ","18 pm ~21 pm", "21 pm~24 pm")
########################################################################################################################### 


# 创建柱状图+误差图
combine_hoursplot <- ggplot(df_combine_hours, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -2, size = 5, color = "red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different time periods", x = "", y = "Coefficient of UTC") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 30, hjust = 0.85), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  scale_x_discrete(labels= Newlabnames_hours)+
  scale_y_continuous(limits = c(0, 0.18),  # 设置y轴范围
                     expand = expansion(mult = c(-0.6, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.18, by = 0.01))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_hoursplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_hours_UTC500.pdf", plot = combine_hoursplot, width =150, height = 120, units = "mm",dpi=300)


######################################## 读取数据 Geography##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_Geography'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_Geography= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)



Newlabnames_Geography<-c("North Brazil", "Northeast Brazil", "Southeast Brazil", "Centralwest Brazil", "South Brazil")
########################################################################################################################### 


# 创建柱状图+误差图
combine_Geographyplot <- ggplot(df_combine_Geography, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -1, size = 5, color = "red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different regions", x = "", y = "Coefficient of UTC") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 25, hjust = 0.85), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  scale_x_discrete(labels= Newlabnames_Geography)+
  scale_y_continuous(limits = c(0, 0.24),  # 设置y轴范围
                     expand = expansion(mult = c(-0.35, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.24, by = 0.02))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_Geographyplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Geography_UTC500.pdf", plot = combine_Geographyplot, width =150, height = 120, units = "mm",dpi=300)




######################################## 读取数据 Skintemp_2Interval ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_Skintemp_2Interval'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_Skintemp_2Interval= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)




Newlabnames_Skintemp_2Interval<-c("< 20°C", "20°C~22°C", "22°C~24°C", "24°C~26°C", "26°C~28°C", "28°C~30°C", "30°C~32°C", "> 32°C" )


####################################################################################################################

# 创建柱状图+误差图
combine_Skintemp_2Intervalplot <- ggplot(df_combine_Skintemp_2Interval, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -3, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different temperatures", x = "", y = "Coefficient of UTC") +
  #geom_vline(xintercept = 1.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 2.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 3.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 4.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 5.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 6.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 7.5, linetype = 1, size =0.5, color="grey80") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 38, hjust = 0.9), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#e5cce5", "#0e8585","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_Skintemp_2Interval)+
  scale_y_continuous(limits = c(0, 0.14),  # 设置y轴范围
                     expand = expansion(mult = c(-0.80, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.14, by = 0.005))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_Skintemp_2Intervalplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Skintemp_2Interval_UTC500.pdf", plot = combine_Skintemp_2Intervalplot, width =200, height = 150, units = "mm",dpi=300)







######################################## 读取lon数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_lon'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_lon= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)



Newlabnames_lon<-c("70°W~ 65°W", "65°W~ 60°W", "60°W~ 55°W", "55°W~ 50°W", "50°W~ 45°W", "45°W~ 40°W", "40°W~ 35°W", "35°W~ 30°W")




# 创建柱状图+误差图
combine_lonplot <- ggplot(df_combine_lon, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -2, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different longitudes", x = "", y = "Coefficient of UTC") +
  #geom_vline(xintercept = 1.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 2.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 3.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 4.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 5.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 6.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 7.5, linetype = 1, size =0.5, color="grey80") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 35, hjust = 0.9), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_lon)+
  scale_y_continuous(limits = c(0, 0.24),  # 设置y轴范围
                     expand = expansion(mult = c(-0.3, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.24, by = 0.02))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_lonplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_lon_UTC500.pdf", plot = combine_lonplot, width =210, height = 100, units = "mm",dpi=300)




######################################## 读取lat数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_lat'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_lat=read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)



Newlabnames_lat<-c("> 0°", "-5°S~0°", "-10°S~-5°S", "-15°S~ -10°S", "-20°S~ -15°S", "-25°S~ -20°S", "-30°S~ -25°S", "-35°S~ -30°S")




# 创建柱状图+误差图
combine_latplot <- ggplot(df_combine_lat, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -1, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different latitudes", x = "", y = "Coefficient of UTC") +
  #geom_vline(xintercept = 1.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 2.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 3.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 4.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 5.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 6.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 7.5, linetype = 1, size =0.5, color="grey80") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 35, hjust = 0.9), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_lat)+
  scale_y_continuous(limits = c(0, 0.18),  # 设置y轴范围
                     expand = expansion(mult = c(-0.1, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.18, by = 0.02))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_latplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_lat_UTC500.pdf", plot = combine_latplot, width =210, height = 100, units = "mm",dpi=300)




######################################## 读取数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_gdp'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_gdp= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)




Newlabnames_gdp<-c( "<5000$", "5000$~10000$", "10000$~15000$", "> 15000$")
########################################################################################################################### 


# 创建柱状图+误差图
combine_gdpplot <- ggplot(df_combine_gdp, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -1, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different GDP levels", x = "", y = "Coefficient of UTC") +
  #geom_vline(xintercept = 1.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 2.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 3.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 4.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 5.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 6.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 7.5, linetype = 1, size =0.5, color="grey80") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 28, hjust = 0.9), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_gdp)+
  scale_y_continuous(limits = c(0, 0.175),  # 设置y轴范围
                     expand = expansion(mult = c(-0.6, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.175, by = 0.01))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_gdpplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_gdp_UTC500.pdf", plot = combine_gdpplot, width =150, height = 100, units = "mm",dpi=300)







######################################## 读取数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_CityScale'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_CityScale= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

Newlabnames_CityScale<-c( "Small city", "Medium city", "Large city", "Megacity")

########################################################################################################################### 


# 创建柱状图+误差图
combine_CityScaleplot <- ggplot(df_combine_CityScale, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -2, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different city sale levels", x = "", y = "Coefficient of UTC") +
  #geom_vline(xintercept = 1.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 2.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 3.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 4.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 5.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 6.5, linetype = 1, size =0.5, color="grey80") +
  #geom_vline(xintercept = 7.5, linetype = 1, size =0.5, color="grey80") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 38, hjust = 0.9), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#0F9E61", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_CityScale)+
  scale_y_continuous(limits = c(0, 0.23),  # 设置y轴范围
                     expand = expansion(mult = c(-0.4, 0)), # 设置图形bar显示
                     breaks = seq(0, 0.23, by = 0.02))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_CityScaleplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_CityScale_UTC500.pdf", plot = combine_CityScaleplot, width =150, height = 100, units = "mm",dpi=300)









###############################################自适应图形标签位置设置-图像合并 输出##########################################################################


# 设置每个图形单独的标签位置
toprow <- cowplot::plot_grid(
  cowplot::plot_grid(combine_lonplot, labels = "a", label_size = 18, label_x = 0.1, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_Geographyplot, labels = "b", label_size = 18, label_x = 0.15, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_gdpplot, labels = "c", label_size = 18, label_x = 0.18, label_y = 1.0, scale = 1),
  nrow = 1, ncol = 4, rel_widths = c(1, 0.65, 0.55, 0.05)
)

bottomrow <- cowplot::plot_grid(
  cowplot::plot_grid(combine_CityScaleplot, labels = "d", label_size = 18, label_x = 0.18, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_hoursplot, labels = "e", label_size = 18, label_x = 0.13, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_Skintemp_2Intervalplot, labels = "f", label_size = 18, label_x = 0.14, label_y = 1.0, scale = 1),
  nrow = 1, ncol = 3, rel_widths = c(0.6, 0.9, 1)
)

mergeplot <- cowplot::plot_grid(
  toprow, bottomrow, 
  ncol = 1, 
  align = "v", 
  rel_heights = c(1, 1, 1), 
  rel_widths = c(1, 1, 1)
)

print(mergeplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_lon_geo_city_gdp_hour_skintemp.pdf", plot = mergeplot, width = 360, height = 320, units = "mm",dpi=600)




