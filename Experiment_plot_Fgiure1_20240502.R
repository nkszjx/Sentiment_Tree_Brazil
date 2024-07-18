
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

## ## ## ## ## ## ## 读取数据## ## ## ## ## ## ## ## ## ## ## 



str0='D:/Sentiment_Brazil/R_codes/NewCodes/gini_style_data/'

path01='year2019Month11day16/'

file_tree='brazil_twitter_points_2019_11_d16_1000_tree'
file_sentiment='brazil_twitter_points_2019_11_d16_sentiment'

input_tree= paste( str0, path01, file_tree , '.csv', sep="") # 字符串连接
tree = read.csv(input_tree, header=FALSE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

input_sentiment= paste( str0, path01, file_sentiment , '.csv', sep="") # 字符串连接
sentiment = read.csv(input_sentiment, header=FALSE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)


# 创建数据框架
data <- data.frame(tree = tree[, 1], sentiment = sentiment[, 1])

# 绘制散点图
#mapping=aes(x= tree ,y=sentiment)

ggplot(data, mapping=aes(x= tree ,y=sentiment))+
  geom_pointdensity(aes(x=tree,y=sentiment))+ # 密度图
  scale_color_viridis(option = "inferno", name = "Counts") +  # 修改颜色映射的图例名称
  geom_smooth(method = 'lm',#线性回归
              formula = 'y ~ x',
              #formula = y ~ poly(x, 2),
              se=T,#添加置信区间，默认就是T
              lwd=1,#线条宽度
              color = "#d33333", #拟合曲线颜色
              fill = "#445376")+  #置信区间颜色
  stat_poly_eq(
    formula = formula(y ~ x),  # 
    aes(label = paste(..eq.label.., sep = "~~~")),
    parse = TRUE,
    label.x = 0.96, label.y = 0.65, #设置位置
    size=5)+
  stat_cor(method='spearman',#使用斯皮尔曼等级相关系数
           label.x = 0.45, label.y = 0.52, #相关系数和P值位置，不设置也行
           label.sep = "\n",#P值和相关性系数换行
           size=5)+
  labs(title='Relationship between \n UTC coverage and sentiment')+ #主标题
  xlab("UTC coverage") +   # X轴标签
  ylab("Sentiment")+ # Y轴标签
  #theme_classic()+
  theme(plot.title = element_text(size=15,hjust = 0.5), #设置主标题大小为20，位置居中
        axis.text = element_text(size = 15, color="black"), # 设置坐标轴文字大小
        axis.title = element_text(size = 15), # 设置坐标轴标题文字大小
        legend.text = element_text(size = 15), # 设置图例文字的大小
        legend.title = element_text(size = 15), # 设置图例标题文字的大小
        panel.border = element_rect(color = "black", fill = NA, size = 1), # 添加图形边框
        panel.background = element_rect(fill = "white"), # 设置背景为白色)
        legend.position = c(0.85, 0.23))+
        scale_x_continuous(breaks = seq(0, 0.6, by = 0.1))-> treesentiment

print(treesentiment)


#ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_tree_sentiment.pdf", plot = treesentiment, width = 300, height = 200, units = "mm",dpi=600)

############################################gini style data #####################################################

file_gini='gini_style_tree'

input_gini= paste( str0, path01, file_gini , '.csv', sep="") # 字符串连接
gini = read.csv(input_gini, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE) 

# 生成 1到100的数组
numbers <- seq(1, 100, by = 1)/100
# 创建数据框架
data2 <- data.frame(ID=numbers, tree0_01 = gini[, 1], 
                    tree01_02 = gini[, 2], tree02_03 = gini[, 3], tree03_10 = gini[, 4])

# 绘制曲线, 
share <- ggplot(data2) +
  geom_line(aes(x = ID, y = tree0_01, color = "tree(0~0.1)"), linewidth = 1.5) +
  geom_line(aes(x = ID, y = tree01_02, color = "tree(0.1~0.2)"), linewidth = 1.5) + # 添加另一条曲线
  geom_line(aes(x = ID, y = tree02_03, color = "tree(0.2~0.3)"), linewidth = 1.5) + # 添加另一条曲线
  geom_line(aes(x = ID, y = tree03_10, color = "tree(>0.3)"), linewidth = 1.5) + # 添加另一条曲线
  ggtitle("Sentiment cumulative curves \n of different UTC coverage") + 
  xlab("Sentiment") + 
  ylab("Cumulative share of total population (×100%)") + 
  scale_color_manual(name = "Curves", 
                     values = c("tree(0~0.1)" = "#333333", 
                                "tree(0.1~0.2)" = "#129333", 
                                "tree(0.2~0.3)" = "#D38000", 
                                "tree(>0.3)" = "#F90000"),
                     breaks = c("tree(0~0.1)", "tree(0.1~0.2)", "tree(0.2~0.3)", "tree(>0.3)"),  # 设置图例标签和颜色
                     labels = c("UTC Coverage (0~0.1)",
                       "UTC Coverage (0.1~0.2)",
                       "UTC Coverage (0.2~0.3)",
                       "UTC Coverage (>0.3)"))+
  theme_minimal() +  # 使用简洁的主题
  theme_classic() +  # 坐标轴
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), # 设置主标题大小为20，位置居中
    axis.text = element_text(size = 15, color = "black"), # 设置坐标轴文字大小
    axis.title = element_text(size = 15), # 设置坐标轴标题文字大小
    legend.text = element_text(size = 15), # 设置图例文字的大小
    legend.title = element_text(size = 15), # 设置图例标题文字的大小
    legend.position = c(0.35, 0.8),
    panel.grid.major = element_line(color = "grey"), # 添加主要网格线
    panel.grid.minor = element_blank(), # 不添加次要网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1), # 添加图形边框
    legend.background = element_rect(fill = "transparent"), # 设置图例背景为透明
    panel.background = element_rect(fill = "white")  # 设置背景为白色
  ) + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置 X 轴刻度
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # 设置 Y 轴刻度

print(share)
  

#ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_gini_share.pdf", plot = share, width = 300, height = 200, units = "mm",dpi=600)


################### 合并图形 ###############################



cc <-  cowplot::plot_grid(treesentiment, share, nrow = 2, ncol = 1, labels = c("a","b"),
                          label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1))
print(cc)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_tree_sentiment_gini_share.pdf", plot = cc, width = 250, height = 300, units = "mm",dpi=600)


#################################################################################################################################################################
#################################################################################################################################################################

# 获取巴西地图集
#Brazil<-  world_map[world_map$region %in% c( "Brazil"), ]

Brazil <- st_read("D:/Sentiment_Brazil/R_codes/NewCodes/brazil-administrative-boundary/04_limiteestadual1991.shp")



## ## ## ## ## ## ## 读取数据## ## ## ## ## ## ## ## ## ## ## 

str0='D:/Sentiment_Brazil/R_codes/Tree-Gini/'
file_name='Brazil_citiy_treecover_gini'
input_file= paste( str0, file_name , '.csv', sep="") # 字符串连接
Mat = read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
# 创建数据框架
anno_df <- data.frame(lon = Mat[, 2],
                      lat = Mat[, 3],
                      Treecover = Mat[, 4],
                      Gini= Mat[, 5])

#转换对象
anno_sf <- st_as_sf(anno_df, coords = c("lon", "lat"),crs = 4326)


## ## ## ## ## ## ## # 绘制地图## ## ## ## ## ## ## ## ## ## ## 

#####
ggplot() +
  geom_sf(data = Brazil)+ # 绘制底图
  ggtitle("Federative Republic of Brazil") +  # 标题
  theme(plot.title = element_text(hjust = 0.5),  # 标题居中
        axis.text = element_text(size = 15),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),   # 设置坐标轴文字的大小
        legend.text = element_text(size = 15)) +  # 设置图例文字的大小
  geom_sf(data = anno_sf, aes(fill=Treecover,size=Gini),shape=21,colour='black',stroke=0.25)+  #添加散点注释
  scale_size(range = c(0.1,4))+ # 调整注释大小
  xlab("Longitude") +   # X轴标签
  ylab("Latitude")+ # Y轴标签
  scale_fill_viridis(option = "inferno")+
  annotation_scale(width_hint=0.2, location = "bl", style='ticks') + # 比例尺
  annotation_north_arrow(location = "tr", which_north = "false",
                         style = north_arrow_fancy_orienteering(text_size = 15))->map_Brazil# 设置指北针文字的大小


print(map_Brazil)




####################################################################################

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil.pdf", plot = map_Brazil, width = 300, height = 300, units = "mm",dpi=600)


#如果 name 列的值等于 "Roraima"，则在 target 列中相应的行被赋值为 "Research Area"；否则，被赋值为 "Mainland Brazil"

Brazil2 <- Brazil %>%
  mutate(Regions = case_when(
    nome == "Roraima" | nome == "Rondônia" | nome == "Acre"| nome == "Amazonas"| nome == "Pará" 
    | nome=="Amapá" | nome=="Tocantins"
    ~ "North Brazil",
    nome == "Maranhão" | nome == "Piauí" | nome == "Ceará"| nome == "Rio Grande do Norte"| 
      nome == "Paraíba" | nome=="Alagoas" | nome=="Sergipe"| nome=="Bahia"| nome=="Pernambuco"
    ~ "Northeast Brazil",
    nome == "Minas Gerais"| nome=="Espírito Santo"| nome=="Rio de Janeiro"| nome=="São Paulo"
    ~ "Sourtheast Brazil",
    nome=="Paraná"| nome=="Santa Catarina" | nome=="Rio Grande do Sul"  
    ~ "South Brazil",
    TRUE ~ "Centralwest Brazil"
  ))




ggplot() +
  geom_sf(data = Brazil2, aes(fill=Regions))+ # 绘制底图
  scale_fill_manual(values = c("North Brazil" = "#FFFFFF","Northeast Brazil"="#EEEEEE", 
                               "South Brazil"="#666666", "Sourtheast Brazil"="#CCCCCC", 
                               "Centralwest Brazil" = "#AAAAAA")) +  # 手动指定颜色映射
  ggtitle("Federative Republic of Brazil") +  # 标题
  theme_minimal() +  # 使用简洁的主题
  geom_histogram(data = anno_sf, aes(x = Treecover), fill = "blue", alpha = 0.5, binwidth = 0.05) +  # 添加 Treecover 的直方图
  geom_histogram(data = anno_sf, aes(x = Gini), fill = "red", alpha = 0.5, binwidth = 0.05) +
  #使用ggnewscale产生一个新的fill映射，否则会覆盖报错。
  ggnewscale::new_scale_fill()+
  geom_sf(data = anno_sf, aes(fill=Treecover,size=Gini),shape=21,colour='black',stroke=0.25)+  #添加散点注释
  scale_fill_viridis(option = "inferno", name = "Tree Coverage") +  # 修改颜色映射的图例名称
  scale_size_continuous(name = "Gini Coeficients") +   # 修改大小映射的图例名称
  scale_size(range = c(0.1,5))+ # 调整注释大小
  xlab("Longitude") +   # X轴标签
  ylab("Latitude")+ # Y轴标签
  theme(plot.title = element_text(size = 18, hjust = 0.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),   # 设置坐标轴文字的大小
        legend.text = element_text(size = 15), # 设置图例文字的大小
        legend.title = element_text(size = 15), # 设置图例标题文字的大小
        legend.position = "right") +  
  annotation_scale(width_hint=0.2, location = "bl", pad_x = unit(4.5, "in"), pad_y = unit(0.3, "in"), style='ticks', text_cex=1.5) + # 比例尺
  annotation_north_arrow(location = "tr", which_north = "false",
                         style = north_arrow_fancy_orienteering(text_size = 15))->map_Brazil# 设置指北针文字的大小


print(map_Brazil)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil.pdf", plot = map_Brazil, width = 300, height = 240, units = "mm",dpi=300)


############################################ 添加直方图#####################################

# 添加 Treecover 的直方图
ggplot() +
  geom_histogram(data = anno_sf, aes(x = Treecover), fill = "#cccccc", color = "#222222", alpha = 1, binwidth = 0.01)+
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  xlab("Tree coverage") +   # X轴标签
  ylab("Counts")+ # Y轴标签
  #ggtitle("Historgram of tree coverage") +  # 标题
  theme(plot.title = element_text(size = 15, hjust = 0.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.05)) ->his_tree  
print(his_tree)



# 添加 Treecover 的直方图
ggplot() +
  geom_histogram(data = anno_sf, aes(x = Gini), fill = "#cccccc", color = "#222222", alpha = 1, binwidth = 0.01) +
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  xlab("Gini coeficients") +   # X轴标签
  ylab("Counts")+ # Y轴标签
  #ggtitle("Historgram of Gini coeficients") +  # 标题
  theme(plot.title = element_text(size = 15, hjust = 0.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.05)) ->his_gini    # 设置坐标轴文字的大小
print(his_gini)


############################################ 读取遥感图片##############################################################################

img_str='D:/Sentiment_Brazil/R_codes/NewCodes/Image_DATA/'

name_lab1='Brasilia_Label_sub_white.jpg'
name_img1='Brasilia_sub.jpg'
name_lab2 ='SaoPaulo_Label_sub_white.jpg'
name_img2='SaoPaulo_sub.jpg'

input_img1= paste( img_str, name_img1, sep="") # 字符串连接
input_lab1= paste( img_str, name_lab1, sep="")

input_img2= paste( img_str, name_img2, sep="") # 字符串连接
input_lab2= paste( img_str, name_lab2, sep="")



#图片导入
img1<-readJPEG(input_img1)
p1<-ggplot()+background_image(img1)+theme_void()
#print(p1)

lab1<-readJPEG(input_lab1)
p2<-ggplot()+background_image(lab1)+theme_void()
#print(p2)

img2<-readJPEG(input_img2)
p3<-ggplot()+background_image(img2)+theme_void()
#print(p3)
lab2<-readJPEG(input_lab2)
p4<-ggplot()+background_image(lab2)+theme_void()
#print(p4)


#image<-p1+p2+p3+p4+plot_layout(ncol=4,nrow = 1, heights = c(1,1,1) )
#print(image)




################################################合并图形###############################################################
#aa<-ggarrange(his_tree,his_gini,ncol = 1,align = "v",heights = c(1,1))
#print(aa)


aa <-  cowplot::plot_grid(map_Brazil,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))
#print(aa)

# label_x = 0.1, label_y = 1.0: 设置标签的相对位置，这里表示标签位于图形的左上角。

bb <-  cowplot::plot_grid(p1, p2, p3, p4,nrow = 1, ncol = 4, labels = c("b", "c", "d", "e"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.15, rel_widths = c(1, 1))

#print(bb)

cc <-  cowplot::plot_grid (treesentiment, share, nrow = 2, ncol = 1, labels = c("f","g"),
                          label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1))
#print(cc)


p1 <- cowplot::plot_grid(aa, bb, nrow = 2,ncol = 1, align = "h", rel_heights = c(3.5, 1), rel_widths = c(3.5, 1)) # rel_heights 参数图形高度设置
#print(p1)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Brazilmap_tree.pdf", plot = p1, width = 300, height = 300, units = "mm",dpi=300)

# 假设 aa 是一个 ggplot 对象
#aa_small <- aa + coord_fixed(ratio = 0.5)

p2 <- cowplot::plot_grid(p1, cc, nrow = 1,ncol = 2, align = "h", rel_heights = c(2.0, 0.9), rel_widths = c(2.0, 1)) # rel_heights 参数图形高度设置
#print(p2)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_Brazilmap_tree_sentiment_gini_share.pdf", plot = p2, 
       width = 350, height = 260, units = "mm",dpi=300)


#################################################################################################################################
############################################################################################################################################################################








