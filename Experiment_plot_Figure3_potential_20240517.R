
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

# 加载世界地图
world_map <- map_data("world")
# 绘制地图
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "white")->map1

print(map1)

# 获取巴西地图集
#Brazil<-  world_map[world_map$region %in% c( "Brazil"), ]

Brazil <- st_read("D:/Sentiment_Brazil/R_codes/NewCodes/brazil-administrative-boundary/04_limiteestadual1991.shp")

#######################################读取树木覆盖度和gini统计数据#######################################


str0='D:/Sentiment_Brazil/R_codes/NewCodes/Tree-Gini/'
file_name='Brazil_citiy_treecover_gini'
input_file= paste( str0, file_name , '.csv', sep="") # 字符串连接
Mat = read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)
# 创建数据框架
anno_df <- data.frame(lon = Mat[, 2],
                      lat = Mat[, 3],
                      Treecover = Mat[, 4],
                      Gini= Mat[, 5])

anno_df2<- anno_df

#转换对象
anno_sf <- st_as_sf(anno_df, coords = c("lon", "lat"),crs = 4326)


######################################## 绘制地图########################################

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


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil.pdf", plot = map_Brazil, width = 300, height = 300, units = "mm",dpi=600)




######################################## 读取不同树木暴露情况下的人口比例 ##############################################################
str1='D:/Sentiment_Brazil/R_codes/NewCodes/different_region_treecover_populationpercent/'
file_name1='Brazil_500m_population'
input_file= paste( str1, file_name1 , '.csv', sep="") # 字符串连接
tree_population = read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

# 创建数据框架
anno_treepopulaton <- data.frame(lon = tree_population[, 2],
                                 lat = tree_population[, 1],
                                 Treecover1 = tree_population[, 3],
                                 Treecover2 = tree_population[, 3]+tree_population[, 4])

anno_treepopulaton2<- anno_treepopulaton

#转换对象
anno_treepopulaton <- st_as_sf(anno_treepopulaton, coords = c("lon", "lat"),crs = 4326)



######################################## 读取不同树木暴露情况下的人口数目######################################

file_name2='population_500_1000_1500'
input_file= paste( str1, file_name2 , '.csv', sep="") # 字符串连接
population_file = read.csv(input_file, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", strip.white = FALSE)

# 创建数据框架
populaton <- data.frame(    poptree00_01 = population_file[, 2],
                            poptree01_02 = population_file[, 3],
                            poptree02_03 = population_file[, 4],
                            poptree03_04 = population_file[, 5],
                            poptree04_05 = population_file[, 6],
                            poptree05_06 = population_file[, 7],
                            poptree06_07 = population_file[, 8])


# 转置数据框并转换回数据框
populaton <- as.data.frame(t(populaton))
# 添加 tag 列,全为1
populaton$tag1 <- 1
populaton$tag2 <- 2
populaton$tag3 <- 3
# 添加 letters 列
letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
populaton$letters <- letters[1:nrow(populaton)]

#### 重新调整数据框,并给数据框列重命名
new_colnames <- c("populaton", "tag", "group")
subpopulaton01 <- data.frame( populaton[,1],populaton[,4], populaton[,7])
colnames(subpopulaton01) <- new_colnames
subpopulaton02 <- data.frame( populaton[,2],populaton[,5], populaton[,7])
colnames(subpopulaton02) <- new_colnames
subpopulaton03 <- data.frame( populaton[,3],populaton[,6], populaton[,7])
colnames(subpopulaton03) <- new_colnames

# 按行方向合并数据框
populaton_f<- rbind(subpopulaton01, subpopulaton02, subpopulaton03)

# 创建数据框架
populaton_rate <- data.frame(
  poptree00_01 = population_file[, 2]/population_file[, 12],
  poptree01_02 = population_file[, 3]/population_file[, 12],
  poptree02_03 = population_file[, 4]/population_file[, 12],
  poptree03_04 = population_file[, 5]/population_file[, 12],
  poptree04_05 = population_file[, 6]/population_file[, 12],
  poptree05_06 = population_file[, 7]/population_file[, 12],
  poptree06_07 = population_file[, 8]/population_file[, 12])


# 转置数据框并转换回数据框
populaton_rate <- as.data.frame(t(populaton_rate))
# 添加 tag 列,全为1
populaton_rate$tag1 <- 1
populaton_rate$tag2 <- 2
populaton_rate$tag3 <- 3
# 添加 letters 列
letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
populaton_rate$letters <- letters[1:nrow(populaton_rate)]

#### 重新调整数据框,并给数据框列重命名
new_colnames <- c("populaton_rate", "tag", "group")
subpopulaton01 <- data.frame( populaton_rate[,1],populaton_rate[,4], populaton_rate[,7])
colnames(subpopulaton01) <- new_colnames
subpopulaton02 <- data.frame( populaton_rate[,2],populaton_rate[,5], populaton_rate[,7])
colnames(subpopulaton02) <- new_colnames
subpopulaton03 <- data.frame( populaton_rate[,3],populaton_rate[,6], populaton_rate[,7])
colnames(subpopulaton03) <- new_colnames

# 按行方向合并数据框
populaton_rate_f<- rbind(subpopulaton01, subpopulaton02, subpopulaton03)




# 获取前7行数据
populaton_f7 <- populaton_f[1:7, ]
populaton_rate_f7 <- populaton_rate_f[1:7, ]



Newlabnames<-c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7")

####################################绘制不同buffer 500m 1000m 1500m 下的直方图（bar）################################################


# 最大值，用于缩放
max_value_populaton_f <- max(populaton_f$populaton)
# 将最大值按比例缩放，以便第二个 y 轴的范围变为 0 到 0.8
Th=0.65
scaling_factor <- Th / max_value_populaton_f

# 创建柱状图
ggplot(populaton_f, aes(x = group, y = populaton, fill = as.factor(tag))) +
  # 绘制柱状图
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  
  # 绘制 populaton_rate_f 的折线图
  geom_line(data = populaton_rate_f, aes(x = group, y = populaton_rate * max(populaton_f$populaton)/Th, group = tag, color = as.factor(tag)), 
            position = position_dodge(width = 0.7), size = 1) +
  geom_point(data = populaton_rate_f, aes(x = group, y = populaton_rate * max(populaton_f$populaton)/Th, group = tag, color = as.factor(tag)), 
             position = position_dodge(width = 0.7), size = 2) +
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  # 设置图例和标签
  scale_fill_discrete(name = "Tag") +
  scale_color_discrete(name = "Tag") +
  labs(title = "", x = "Tree cover", y = "Popoluation") +
  #facet_wrap(~ tag)+
  theme_minimal()+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        text = element_text(family = 'Helvetica', size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),  # 设置图例文字大小
        legend.position = c(0.98, 1), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_x_discrete(labels= Newlabnames)+
  scale_fill_manual(name = "",
                    labels=c("UTC 500m", "UTC 1000m", "UTC 1500m"),
                    values = c("#f2f2f2","#C0C0C0","#828383"), 
                    guide = "legend", # 添加legend
  )+
  scale_color_manual(name = "",
                     labels=c("UTC 500m", "UTC 1000m", "UTC 1500m"),
                     values = c("#D02222","#308014","#33a1c9"), 
                     guide = "legend", # 添加legend
  ) +
  scale_y_continuous(limits = c(0, 100000000), breaks = seq(0, 100000000, by = 20000000), 
                     sec.axis = sec_axis(~ . * scaling_factor, name = "Populaton Rate ", breaks = seq(0, 0.8, by = 0.1)))-> mapbar
#sec.axis = sec_axis(~./max(populaton_f$populaton), name = "Populaton Proportion"))-> mapbar 

#scale_fill_manual( values = c("#002C5B66","#0682B4","#D46d51", "#0682B4", "#1112B4"), guide = "none")+ # 不需要显示图例legend， guide= "none"
#scale_y_continuous(limits = c(0, 100000000),  # 设置y轴范围
#expand = expansion(mult = c(0, 0)), # 设置图形bar显示
#breaks = seq(0, 100000000, by = 10000000))-> mapbar  # 设置 Y 轴的间隔
# 打印柱状图
print(mapbar)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_population_UTC_ALL.pdf", plot = mapbar, width =200, height = 100, units = "mm",dpi=600)


#############################################只绘制 UTC 500m 下的直方图和比例折线 ##################################################
# 创建柱状图
ggplot(populaton_f7, aes(x = group, y = populaton, fill = as.factor(tag))) +
  # 绘制柱状图
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  
  # 绘制 populaton_rate_f 的折线图
  geom_line(data = populaton_rate_f7, aes(x = group, y = populaton_rate * max(populaton_f$populaton)/Th, group = tag, color = as.factor(tag)), 
            position = position_dodge(width = 0.7), size = 1) +
  geom_point(data = populaton_rate_f7, aes(x = group, y = populaton_rate * max(populaton_f$populaton)/Th, group = tag, color = as.factor(tag)), 
             position = position_dodge(width = 0.7), size = 2) +
  geom_hline(yintercept = 0, linetype = 2, size =0.5, color="black") +  # 在图中添加一条水平直线
  # 设置图例和标签
  scale_fill_discrete(name = "Tag") +
  scale_color_discrete(name = "Tag") +
  labs(title = "", x = "Tree cover", y = "Popoluation") +
  #facet_wrap(~ tag)+
  theme_minimal()+
  #theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), # 大标题水平位置hjust和字体大小size
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black', angle = 20, hjust = 0.8), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black', angle = 0, hjust = 0.5),
        axis.line.x = element_line(size = 1.0),
        axis.line.y = element_line(size = 1.0),
        text = element_text(family = 'Helvetica', size = 13),
        panel.border = element_rect(color = "black", fill = NA, size = 1), # 添加图形边框
        legend.title = element_blank(),
        legend.text = element_text(size = 15),  # 设置图例文字大小
        legend.position = c(0.98, 0.98), # 设置图例位置为右上角的相对位置
        legend.justification = c(1, 1), # 设置图例对齐方式为右上方
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),  # 设置图例边框为透明
        legend.margin = margin(2, 2, 2, 2),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0.3, 'cm'),
        legend.key.size = unit(18, "pt"))+
  scale_x_discrete(labels= Newlabnames)+
  scale_fill_manual(name = "",
                    labels=c("UTC 500m", "UTC 1000m", "UTC 1500m"),
                    values = c("#4682B4", "#C0C0C0","#f2f2f2","#C0C0C0","#828383"), 
                    guide = "legend", # 添加legend
  )+
  scale_color_manual(name = "",
                     labels=c("UTC 500m", "UTC 1000m", "UTC 1500m"),
                     values = c("#D02222","#308014","#33a1c9"), 
                     guide = "legend", # 添加legend
  ) +
  scale_y_continuous(limits = c(0, 100000000), breaks = seq(0, 100000000, by = 20000000), 
                     sec.axis = sec_axis(~ . * scaling_factor, name = "Populaton Proportion ", breaks = seq(0, 0.8, by = 0.1)))-> mapbar500
# 打印柱状图
print(mapbar500)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_population_UTC500.pdf", plot = mapbar500, width =150, height = 100, units = "mm",dpi=600)




#######################################################################################################################################################
#############################################################对 Brazil地图进行地理分区###############################################################

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



############################################ 绘制不同树木覆盖情况下人口暴露比例 tree cover <0.1 #####################################


ggplot() +
  geom_sf(data = Brazil2, aes(fill=Regions))+ # 绘制底图
  scale_fill_manual(values = c("North Brazil" = "#FFFFFF","Northeast Brazil"="#EEEEEE", 
                               "South Brazil"="#888888", "Sourtheast Brazil"="#CCCCCC", 
                               "Centralwest Brazil" = "#AAAAAA")) +  # 手动指定颜色映射
  ggtitle("Proportion of population in each city \n with tree cover (500m) exposure levels below 0.1 in Brazil") +  # 标题
  theme_minimal() +  # 使用简洁的主题
  #使用ggnewscale产生一个新的fill映射，否则会覆盖报错。
  ggnewscale::new_scale_fill()+
  geom_sf(data = anno_treepopulaton, aes(fill=Treecover1,size= 1 ),shape=21,colour='black',stroke=0.25, show.legend = c(size = FALSE))+  #添加散点注释
  scale_fill_viridis(option = "inferno", name = "Proportion", guide = guide_colorbar(barheight = unit(5, "cm"))) +  # 修改颜色映射的图例名称
  scale_size(range = c(0.1,4))+ # 调整注释大小
  xlab("Longitude") +   # X轴标签
  ylab("Latitude")+ # Y轴标签
  theme(plot.title = element_text(size = 18, hjust = 0.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),   # 设置坐标轴文字的大小
        #legend.position = "none"  # 设置图例位置为 "none"，即隐藏图例
        legend.text = element_text(size = 15), # 设置图例文字的大小
        legend.title = element_text(size = 15), # 设置图例标题文字的大小
        legend.position = "right"
        ) +  
  annotation_scale(width_hint=0.2, location = "bl", pad_x = unit(4.5, "in"), pad_y = unit(0.3, "in"), 
                   style='ticks', text_cex=1.5) + # 比例尺
  annotation_north_arrow(location = "tr", which_north = "false",
                         style = north_arrow_fancy_orienteering(text_size = 15))->map_Brazil_population_tree1# 设置指北针文字的大小


print(map_Brazil_population_tree1)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil_population_tree1.pdf", plot = map_Brazil_population_tree1, width = 300, height = 240, units = "mm",dpi=300)






############################################ 绘制不同树木覆盖情况下人口暴露比例 tree cover <0.2 #####################################

ggplot() +
  geom_sf(data = Brazil2, aes(fill=Regions))+ # 绘制底图
  scale_fill_manual(values = c("North Brazil" = "#FFFFFF","Northeast Brazil"="#EEEEEE", 
                               "South Brazil"="#888888", "Sourtheast Brazil"="#CCCCCC", 
                               "Centralwest Brazil" = "#AAAAAA")) +  # 手动指定颜色映射
  ggtitle("Proportion of population in each city \n with tree cover (500m) exposure levels below 0.2 in Brazil") +  # 标题
  theme_minimal() +  # 使用简洁的主题
  #使用ggnewscale产生一个新的fill映射，否则会覆盖报错。
  ggnewscale::new_scale_fill()+
  geom_sf(data = anno_treepopulaton, aes(fill=Treecover2,size= 1 ),shape=21,colour='black',stroke=0.25, show.legend = c(size = FALSE))+  #添加散点注释
  scale_fill_viridis(option = "inferno", name = "Proportion", guide = guide_colorbar(barheight = unit(5, "cm"))) +  # 修改颜色映射的图例名称
  scale_size(range = c(0.1,4))+ # 调整注释大小
  xlab("Longitude") +   # X轴标签
  ylab("Latitude")+ # Y轴标签
  theme(plot.title = element_text(size = 18, hjust = 0.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),   # 设置坐标轴文字的大小
        legend.text = element_text(size = 15), # 设置图例文字的大小
        legend.title = element_text(size = 15), # 设置图例标题文字的大小
        legend.position = "right") +  
  annotation_scale(width_hint=0.2, location = "bl", pad_x = unit(4.5, "in"), pad_y = unit(0.3, "in"), 
                   style='ticks', text_cex=1.5) + # 比例尺
  annotation_north_arrow(location = "tr", which_north = "false",
                         style = north_arrow_fancy_orienteering(text_size = 15))->map_Brazil_population_tree2# 设置指北针文字的大小


print(map_Brazil_population_tree2)


ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil_population_tree2.pdf", plot = map_Brazil_population_tree2, width = 300, height = 240, units = "mm",dpi=300)


####################################### 合并不同覆盖率下人口分布的图##################################################

aa <-  cowplot::plot_grid(map_Brazil_population_tree1,map_Brazil_population_tree2, nrow = 1, ncol = 2, labels = c("a","b"),
                          label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1))
print(aa)

ggsave("D:/Sentiment_Brazil/R_codes/fig_map_Brazil_population_tree12.pdf", plot = aa, width = 600, height = 250, units = "mm",dpi=300)








######################################################## 添加直方图########################################################

# 添加 Treecover <0.1 的直方图
ggplot() +
  geom_histogram(data = anno_treepopulaton, aes(x = Treecover1), fill = "#cccccc", 
                 color = "#222222", alpha = 1, binwidth = 0.01)+
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  labs(title = "Tree coverage <0.1", x = "Population Proportion", y = "Number of cities") +
  #ggtitle("Historgram of tree coverage") +  # 标题
  theme(plot.title = element_text(size = 15, hjust = 0.9, vjust = -9.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black'), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black'))+
        #panel.border = element_rect(color = "black", fill = NA, size = 1)) # 添加图形边框)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) ->his_tree1  
print(his_tree1)



# 添加 Treecover <0.2 的直方图
ggplot() +
  geom_histogram(data = anno_treepopulaton, aes(x = Treecover2), 
                 fill = "#cccccc", color = "#222222", alpha = 1, binwidth = 0.01) +
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  labs(title = "Tree coverage <0.2", x = "Population Proportion", y = "Number of cities") +
  theme(plot.title = element_text(size = 15, hjust = 0.9, vjust = -9.5),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15),
        axis.title.y = element_text(vjust = 2, size = 15),# y 轴标题垂直位置vjust和字体大小size
        axis.title.x = element_text(vjust = 2, size = 15), # x轴标题垂直位置vjust和字体大小size
        axis.text.x = element_text(size = 15, color = 'black'), # 设置 X 轴标签字体位置
        axis.text.y = element_text(size = 15, color = 'black'))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) ->his_tree2    # 设置坐标轴文字的大小
print(his_tree2)


################################################合并图形###############################################################

aa <-  cowplot::plot_grid(map_Brazil_population_tree1, map_Brazil_population_tree2,nrow = 2, ncol = 1, labels = c("a","b"),
                          label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1))
print(aa)


bb <-  cowplot::plot_grid(his_tree1, his_tree2, mapbar500, nrow = 3, ncol = 1, labels = c("c","d","e"),
                          label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1, 1))
print(bb)


#cc <-  cowplot::plot_grid(mapbar500, nrow = 1, ncol = 1, labels = c("e"),
                          #label_size = 18, scale=1,label_x = 0.1, label_y = 1.0, rel_widths = c(1, 1))
#print(cc)

cc <-  cowplot::plot_grid(aa, bb, nrow = 1, ncol = 2, rel_widths = c(1, 0.65))
print(cc)

ggsave("D:/Sentiment_Brazil/R_codes/NewCodes/fig_map_Brazil_histtree_population.pdf", plot = cc, width = 400, height = 400, units = "mm",dpi=300)




