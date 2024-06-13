# Sentiment_Tree_Brazil


# The Urban Tree Canopy Cover in Brazil

This is the R codes for our paper: [author = Jianhua Guo, Anna Kruspe, and Xiao Xiang Zhu, Title: The Effect of Trees on Urbanites’ Daily Sentimental Expressions].
To better service Brazil’s urban ecosystem, we developed a semi-supervised deep learning method, which is able to learn semantic segmentation knowledge from both labeled and unlabeled images, to segment urban trees from high spatial resolution remote sensing images. The approach attains significant improvement over existing methods, especially when trained with limited labeled samples. Using this approach, we created 0.5 m fine-scale tree canopy products for 472 cities in Brazil and made them freely available to the community ([UTB dataset1](https://nkszjx.github.io/projects/UTB.html)). 
![](/figure/TreeSeg_Network.png)


## Results
### Tree segmentation results
![](/figure/tree.png)

### Urban tree canopy cover in Brazil
![](/figure/Graphical.png)

## Package pre-requisites
The codes running environment are required. 

```
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
```

## Acknowledgement

Our ideas may partly come from the paper:
[Surrounding greenness is associated with lower risk and burden of low birth weight in Iran]([https://github.com/speedinghzl/Pytorch-Deeplab](https://www.nature.com/articles/s41467-023-43425-6#:~:text=By%20involving%20~4%20million%20Iranian,risks%20of%20LBW%20and%20TLBW.))
[Air pollution lowers Chinese urbanites’ expressed happiness on social media]([https://github.com/hfslyc/AdvSemiSeg](https://www.nature.com/articles/s41562-018-0521-2))
[Temperature impacts on hate speech online: evidence from 4 billion geolocated tweets from the USA](https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(22)00173-5/fulltext)
[Urbanites’ mental health undermined by air pollution](https://www.nature.com/articles/s41893-022-01032-1)
## Citation

```
@article{GUO20231,
title = {Nationwide urban tree canopy mapping and coverage assessment in Brazil from high-resolution remote sensing images using deep learning},
journal = {ISPRS Journal of Photogrammetry and Remote Sensing},
volume = {198},
pages = {1-15},
year = {2023},
issn = {0924-2716},
doi = {https://doi.org/10.1016/j.isprsjprs.2023.02.007},
url = {https://www.sciencedirect.com/science/article/pii/S0924271623000461},
author = {Jianhua Guo, Qingsong Xu, Yue Zeng, Zhiheng Liu, and Xiao Xiang Zhu},
}
```
```


