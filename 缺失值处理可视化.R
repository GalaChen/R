#data frame模拟 
 
 set.seed(888)
 #set.seed()用于设定随机数种子，一个特定的种子可以产生一个特定的伪随机序列，这个函数的主要目的，是让你的模拟能够可重复出现
 #可以简单地理解为括号里的数只是一个编号而已，例如set.seed(100)不应将括号里的数字理解成“一百”，而是应该理解成“编号为一零零的随机数发生”
 age<-round(abs(rnorm(200,mean = 67,sd = 19)))
 sex<-rbinom(200,1,0.45)
 sex_miss_tag<-rbinom(200,1,0.3) #设置缺失值，缺失率为30%
 sex[sex_miss_tag == 1]<-NA
 #sex_miss_tag为1的值设定为缺失值NA
 sex[sex == 1]<-'male'
 sex[sex == 0]<-'female'
 lac<-round(abs(rnorm(200,mean=3,sd=4)),1)
 lac_miss_tag<-rbinom(200,1,0.3)
 lac[lac<=3 & lac_miss_tag ==1 ]<-NA
 #lac小于3的时候，病人情况通常稳定，临床实践中可不测定乳酸值。故此时的NA可能意味着病人情况稳定
 wbc<-round(abs(rnorm(200,mean=12,sd=4)),1)
 wbc_miss_tag<-rbinom(200,1,0.3)
 wbc[wbc_miss_tag == 1]<-NA
 crp<-round(abs(rnorm(200,mean=50,sd=100)),1)
 crp_miss_tag<-rbinom(200,1,0.4)
 crp[wbc<=12 & crp_miss_tag == 1]<-NA
 #wbc,即白细胞，其小于12时可认为没有感染，故无需测量crp
 data<-data.frame(age,sex,lac,wbc,crp)
 
#探索缺失数据样式
 ########################     数字展示    #######################
 library(mice)
 md.pattern(data)
 #展示的结果解释如下：
 #1代表非缺失值，0代表缺失值
 #左侧边栏数据代表该类组合的总数
 #右侧边栏代表该类组合中，缺失值的个数
 #最下面的数字代表每个变量包含的缺失值个数
 
 #######################     图像展示     #######################
 library(VIM)
 matrixplot(data)
 #红色表示缺失值，灰度条表示非缺失值，且友深到浅表示数值由高到低
 #wbc和CRP可以看出二者有逻辑关系：WBC较高的时候，CRP无缺失值
 barMiss(data)
 #横轴将变量分为区段，纵轴表示数量。红色表示缺失值
 #每一个bar的红色区域表示，该变量在这一区间范围内为完整，其它变量至少有一个变量缺失的组合的数量；
 #蓝色表示该变量该范围内，所有变量都完整的数量 
 #最右侧missing的含义(以乳酸为例)
 #红色表示乳酸和其它变量至少有一个变量有缺失的各种组合类型的变量
 #蓝色表示乳酸有缺失，其它变量无任何缺失的组合类型的数量
 aggr(data,numbers = TRUE, prop = FALSE)
 #第一个参数表明操作对象
 #第二个参数表示在图例最右侧是否显示对应的数字
 #第三个参数表示在图例最左侧是否显示比例
 #左边的图直接显示每个变量的缺失值个数，右边的图则显示各组合数量，想当于md.pattern(data)的图示版
 marginplot(data[c('wbc','crp')],pch=c(20),col=c('green','red','blue'))
 #第一个参数表示需要比较的变量，第二个参数表示点的大小，第三个参数表示颜色
 #两个数字分别代表各自变量缺失的数目
 #以wbc为例
 #红点表示crp缺失时，wbc的分布情况
 #红色的箱型图表示crp缺失时，wbc的分布情况统计，可见均值较小
 #绿色的箱型图则表示crp完整时，wbc的分布情况统计，可见均值较大
 #由此说明，wbc较小时容易出现crp缺失（这与临床实际情况相符）
 marginmatrix(data) 
 #上一个code的全景版
 spineMiss(data) 
 #与之前的barMiss相似，只是纵轴改为比例刻度
 scattmatrixMiss(data)
 #红色表示缺失，蓝色表示无缺失
 
 #######################     相关性矩阵     #######################
 shadow<-as.data.frame(abs(is.na(data)))
 #产生影子矩阵
 #影子矩阵的缺失值用1表示，非缺失值用0表示
 #as.data.frame是检查操作对象是否为data frame,如果不是则强制转换
 miss_shadow<-shadow[,which(unlist(lapply(shadow, sum))!=0)] 
 #将shadow中的分一列进行求和，取求和不为0的列作为missShadow变量
 round(cor(miss_shadow),3)
 #cor进行相关性分析
 #数值表示两个变量间的相关系数，数值较小说明一个变量的缺失与另一个变量的缺失不相关
 
 round(cor(data[!names(data)%in%c('sex')],
 miss_shadow,use = 'pairwise.complete.obs'),3)
 #这一code用于计算某一变量的缺失，是否会受到其它数值的影响
 #  a %in% b 判别式，判别b是否存在于a，返回值为true或false
 # data[]其括号内为true与false时，只显示true对应的列,从而排除sex变量
 # cor的前两个参数是两组数据，其大小相等，第三个参数是计算方法
 # 所得数据，负数表示负相关，横行表示缺失变量，纵列表示连续性数值变量
 #crp的缺失与wbc的数值大小呈负相关，crp的缺失在低水平的wbc容易出现
 #cor用于连续型数值变量，故需要先排除sex变量
 
 #方差：体现的是一组数据的波动情况，值越小波动越小。 
 #协方差：两种不同数据的方差，体现两组数据的变化趋势如何，正值变化趋势一致，负值变化趋势相反，0不相关。 
 #相关系数：两组不同数据的相关程度，取值范围[-1，1]，越接近与0越不相关
 #相关系数为0时却不意味着两组数据独立
 #相关系数是两个变量之间的线性关联的一个度量，不一定有因果关系的含义。 
 
 #cor函数计算的是列与列间的相关系数
 #cov函数计算的是列与列的协方差