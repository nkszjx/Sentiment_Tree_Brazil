


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
            vjust = -2.0, size = 5, color = "red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different regions", x = "", y = "Coefficient of UTC") +
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
  scale_x_discrete(labels= Newlabnames_Geography)+
  scale_y_continuous(limits = c(-0.00, 0.07),  # 设置y轴范围
                     expand = expansion(mult = c(0.00, 0)), # 设置图形bar显示
                     breaks = seq(-0.00, 0.07, by = 0.01))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_Geographyplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Geography_UTC500.pdf", plot = combine_Geographyplot, width =150, height = 120, units = "mm",dpi=300)







######################################## 读取数据 lcz  ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_lcz'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_lcz= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

#df_combine_lcz <- subset(df_combine_lcz, !(character %in% c("lcz7", "lcz10") ) )
#rm(subset_data)



Newlabnames_lcz<-c( "Compact high-/midrise", "Compact lowrise", 
                          "Open highrise", "Open midrise", "Open lowrise", "Large lowrise", "Sparsely built", "Heavy Industry", "Others" )

########################################################################################################################### 


# 创建柱状图+误差图
combine_lczplot <- ggplot(df_combine_lcz, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -1.5, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different local climate zones", x = "", y = "Coefficient of UTC") +
  theme_classic() +
  # 添加断轴标记
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = "white")), xmin = -Inf, xmax = Inf, ymin = 0.055, ymax = 0.095) + # 覆盖区域
  annotate("segment", x = -Inf, xend = Inf, y = 0.055, yend = 0.055, linetype = "dashed") + # 第一道断线
  annotate("segment", x = -Inf, xend = Inf, y = 0.095, yend = 0.095, linetype = "dashed") + # 第二道断线
  
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 25, hjust = 0.9), # 设置 X 轴标签字体位置
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
  scale_x_discrete(labels= Newlabnames_lcz)+
  scale_y_continuous(limits = c(-0.000, 0.225),  # 设置y轴范围
                     expand = expansion(mult = c(-0.0, 0)), # 设置图形bar显示
                     breaks = seq(-0.000, 0.225, by = 0.025))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_lczplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_LCZ_UTC500.pdf", plot = combine_lczplot, width =150, height = 100, units = "mm",dpi=300)





############################使用手动截断法并且自定义Y轴标签，以解决数据中的一个数值明显偏大的问题 ##############################

# 设置阈值以截断数据
threshold <- 0.1
shrink_factor <- 0.1 # 缩放因子

# 处理数据，将超过阈值的数据进行缩放
df_combine_lcz$coef_truncated <- ifelse(df_combine_lcz$coef > threshold, 
                                        threshold + (df_combine_lcz$coef - threshold) * shrink_factor, 
                                        df_combine_lcz$coef)
df_combine_lcz$max_truncated <- ifelse(df_combine_lcz$max > threshold, 
                                       threshold + (df_combine_lcz$max - threshold) * shrink_factor, 
                                       df_combine_lcz$max)
df_combine_lcz$min_truncated <- ifelse(df_combine_lcz$min > threshold, 
                                       threshold + (df_combine_lcz$min - threshold) * shrink_factor, 
                                       df_combine_lcz$min)
# 自定义Y轴标签
custom_y_labels <- function(breaks) {
  sapply(breaks, function(x) {
    if (x < threshold) {
      return(x)
    } else {
      return(threshold + (x - threshold) / shrink_factor)
    }
  })
}

# 创建柱状图+误差图
combine_lczplot <- ggplot(df_combine_lcz, aes(x = group, y = coef_truncated, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min_truncated, ymax = max_truncated), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -1.5, size = 5, color = "red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, color = "black") +  # 在图中添加一条水平直线
  labs(title = "Different local climate zones", x = "", y = "Coefficient of UTC") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),  # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15), # y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 25, hjust = 0.9), # 设置 X 轴标签字体位置
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
        legend.key.size = unit(18, "pt")) +
  scale_fill_manual(values = c("#4682B4", "#002C5B66", "#0682B4", "#0F9E61", "#0682B4", "#1112B4"), guide = "none") + # 不需要显示图例legend， guide= "none"
  scale_x_discrete(labels = Newlabnames_lcz) +
  scale_y_continuous(limits = c(0, 0.125),  # 设置y轴范围
                     expand = expansion(mult = c(0, 0)), # 设置图形bar显示
                     breaks = c(seq(0, 0.1, by = 0.02), 0.1, 0.125), # 设置 Y 轴的间隔
                     labels = custom_y_labels)

# 打印柱状图
print(combine_lczplot)



ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_LCZ_UTC500.pdf", plot = combine_lczplot, width =150, height = 100, units = "mm",dpi=300)








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
            vjust = -2.0, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
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
        axis.text.x = element_text(size = 15, color = 'black', angle = 22, hjust = 0.9), # 设置 X 轴标签字体位置
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
  scale_y_continuous(limits = c(-0.000, 0.055),  # 设置y轴范围
                     expand = expansion(mult = c(-0.2, 0)), # 设置图形bar显示
                     breaks = seq(-0.000, 0.055, by = 0.005))  # 设置 Y 轴的间隔
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
            vjust = -3.0, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
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
  scale_fill_manual(values = c("#4682B4","#002C5B66","#0682B4","#0F9E61", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
  #labels = c("UTC500", "UTC1000", "UTC1500"),
  #guide = guide_legend(title = NULL, 
  #nrow = 1,
  #label.theme = element_text(size = 15, 
  #vjust = 0.5, 
  #margin = margin(r = 5, unit = "pt")))) +
  scale_x_discrete(labels= Newlabnames_CityScale)+
  scale_y_continuous(limits = c(-0.000, 0.06),  # 设置y轴范围
                     expand = expansion(mult = c(-0.2, 0)), # 设置图形bar显示
                     breaks = seq(-0.000, 0.06, by = 0.005))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_CityScaleplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_CityScale_UTC500.pdf", plot = combine_CityScaleplot, width =150, height = 100, units = "mm",dpi=300)




######################################## 读取数据 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/UTC500_heterogeneity20240606/'
file_name1='UTC500_combine_season'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
df_combine_season= read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

Newlabnames_season<-c( "Spring", "Summer", "Autumn", "Winter")

########################################################################################################################### 


# 创建柱状图+误差图
combine_seasonplot <- ggplot(df_combine_season, aes(x = group, y = coef, fill = factor(tag))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(width = 0.7), width = 0.3, size = 1.0) +
  geom_text(aes(label = ifelse(pval > 0.05, "", ifelse(pval > 0.01, "*", ifelse(pval > 0.001, "**", "***")))), 
            vjust = -2.5, size = 5, color="red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different seasons", x = "", y = "Coefficient of UTC") +
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
  scale_x_discrete(labels= Newlabnames_season)+
  scale_y_continuous(limits = c(-0.000, 0.04),  # 设置y轴范围
                     expand = expansion(mult = c(-0.2, 0)), # 设置图形bar显示
                     breaks = seq(-0.000, 0.04, by = 0.005))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_seasonplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_season_UTC500.pdf", plot = combine_seasonplot, width =150, height = 100, units = "mm",dpi=300)





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
            vjust = -1, size = 5, color = "red") +  # 添加显著性，并调整文本标签位置和大小
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  labs(title = "Different time periods", x = "", y = "Coefficient of UTC") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 28, hjust = 0.85), # 设置 X 轴标签字体位置
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
  scale_y_continuous(limits = c(-0.005, 0.085),  # 设置y轴范围
                     expand = expansion(mult = c(-0.1, 0)), # 设置图形bar显示
                     breaks = seq(-0.005, 0.085, by = 0.01))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_hoursplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_hours_UTC500.pdf", plot = combine_hoursplot, width =150, height = 120, units = "mm",dpi=300)




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
  scale_y_continuous(limits = c(-0.000, 0.04),  # 设置y轴范围
                     expand = expansion(mult = c(-0.25, 0)), # 设置图形bar显示
                     breaks = seq(-0.000, 0.04, by = 0.005))  # 设置 Y 轴的间隔
# 打印柱状图
print(combine_Skintemp_2Intervalplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Skintemp_2Interval_UTC500.pdf", plot = combine_Skintemp_2Intervalplot, width =200, height = 150, units = "mm",dpi=300)




###############################################自适应图形标签位置设置-图像合并 输出##########################################################################


# 设置每个图形单独的标签位置
toprow <- cowplot::plot_grid(
  cowplot::plot_grid(combine_Geographyplot, labels = "a", label_size = 18, label_x = 0.15, label_y = 1.02, scale = 1),
  cowplot::plot_grid(NaN, labels = "", label_size = 18, label_x = 0.1, label_y = 1.0, scale = 1),
  cowplot::plot_grid( combine_lczplot , labels = "b", label_size = 18, label_x = 0.12, label_y = 1.02, scale = 1),
  nrow = 1, ncol = 3, rel_widths = c( 0.68, 0.1, 1)
)


midmrow <- cowplot::plot_grid(
  cowplot::plot_grid(combine_gdpplot, labels = "c", label_size = 18, label_x = 0.22, label_y = 1.01, scale = 1),
  cowplot::plot_grid(NaN, labels = "", label_size = 18, label_x = 0.1, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_CityScaleplot, labels = "d ", label_size = 18, label_x = 0.2, label_y = 1.01, scale = 1),
  cowplot::plot_grid(NaN, labels = "", label_size = 18, label_x = 0.15, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_seasonplot, labels = "e", label_size = 18, label_x = 0.23, label_y = 1.01, scale = 1),
    nrow = 1, ncol = 5, rel_widths = c(1, 0.1, 1, 0.1, 1)
)



bottomrow <- cowplot::plot_grid(
  cowplot::plot_grid(combine_hoursplot, labels = "f", label_size = 18, label_x = 0.16, label_y = 1.01, scale = 1),
  cowplot::plot_grid(NaN, labels = " ", label_size = 18, label_x = 0.13, label_y = 1.0, scale = 1),
  cowplot::plot_grid(combine_Skintemp_2Intervalplot, labels = "g", label_size = 18, label_x = 0.13, label_y = 1.01, scale = 1),
  nrow = 1, ncol = 3, rel_widths = c(0.9, 0.001, 1)
)

mergeplot <- cowplot::plot_grid(
  toprow, midmrow, bottomrow, 
  ncol = 1, 
  align = "v", 
  rel_heights = c(1, 1, 1), 
  rel_widths = c(1, 1, 1)
)

print(mergeplot)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_lon_geo_sea_city_gdp_hour_skintemp.pdf", plot = mergeplot, width = 300, height = 340, units = "mm",dpi=600)




