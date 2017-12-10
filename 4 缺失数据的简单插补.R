 #############################    建立模拟缺失数据集   #################################
 set.seed(12)
 
 sex<-rbinom(150,1,0.45)
 sex[sex == 1]<-'male'
 sex[sex == 0]<-'female'
 sex_miss_tag<-rbinom(150,1,0.3) 
 sex_miss<-ifelse(sex_miss_tag == 1, NA, sex)
 
 map<-round(abs(rnorm(150,mean=70,sd=30)))
 map<-ifelse(map<=40, map+30, map) #可能会出现异常小的值，故而需要校正一下
 
 lac<-rnorm(150,mean = 5, sd = 0.7)-map*0.04
 lac<-abs(round(lac,1))
 lac_miss_tag<-rbinom(150,1,0.3)
 lac_miss<-ifelse(lac_miss_tag == 1, NA, lac)
 
 data<-data.frame(sex_miss,map,lac_miss)
 
 #############################    缺失值的探索   #################################
 sd(lac_miss,na.rm = TRUE) #标准差
 summary(lac_miss) #统计并呈现 最小值，最大值，四分位数，NA个数
 
 library(car)
 scatterplot(lac~map|lac_miss_tag,lwd=2,
                     main='Scatter Plot of lac vs. map by #missingness',
                     xlab = 'Mean Arterial Pressure (mmHg)',
                     ylab = 'Lactate (mmol/L)',
                     legend.plot = TRUE,
                     id.method = 'identify',
                     boxplots = 'xy')
 #lac~map表示纵坐标与横坐标
 #lac_miss_tag指明分组的依据，lwd是线的宽度
 #id.method是说明可以交互式识别图中的散点
 #boxplots规定箱式图的画法，可单选，什么都不填则为不绘制箱式图
 #具体的情况可以看Help
 
 #散点图中可以发现，lac的缺失值均匀分布，拟合曲线也近似相似，说明其为随机缺失，与MAP无关
 
 #############################    均数插补   #################################
 lac_mean<-round(ifelse(is.na(lac_miss),mean(lac_miss,na.rm = TRUE),lac_miss),1)
 scatterplot(lac_mean~map|lac_miss_tag,lwd=2,
             main='Scatter Plot of lac vs. map by #missingness',
             xlab = 'Mean Arterial Pressure (mmHg)',
             ylab = 'Lactate (mmol/L)',
             legend.plot = TRUE,
             id.method = 'identify',
             boxplots = 'xy')
 #此时，所有NA的点都分布在平局线上，离散程度被低估
 
 #############################    众数插补   ################################# 
 mode_of_lac <- as.numeric(names(table(lac_miss, useNA = 'no')))[which.max(table(lac_miss, useNA = 'no'))]
 
 lac_mode<-round(ifelse(is.na(lac_miss),mode_of_lac,lac_miss),1)
 
 scatterplot(lac_mode~map|lac_miss_tag,lwd=2,
             main='Scatter Plot of lac vs. map by #missingness',
             xlab = 'Mean Arterial Pressure (mmHg)',
             ylab = 'Lactate (mmol/L)',
             legend.plot = TRUE,
             id.method = 'identify',
             boxplots = 'xy')
 
 #############################    中位数插补   ################################# 
 lac_median<-round(ifelse(is.na(lac_miss),median(lac_miss,na.rm = TRUE),lac_miss),1)
 scatterplot(lac_median~map|lac_miss_tag,lwd=2,
             main='Scatter Plot of lac vs. map by #missingness',
             xlab = 'Mean Arterial Pressure (mmHg)',
             ylab = 'Lactate (mmol/L)',
             legend.plot = TRUE,
             id.method = 'identify',
             boxplots = 'xy')
 
 #############################    回归插补   #################################
 fit<-lm(lac_miss~map, data = data)
 #根据lac_miss与map的关系，得到二者的线性拟合函数
 lac_pred<-predict(fit,newdata=data)
 #根据之前的拟合函数，由原有的map预测新的lac_miss
 lac_regress<-round(ifelse(is.na(lac_miss),lac_pred,lac_miss),1)
 scatterplot(lac_regress~map|lac_miss_tag,lwd=2,
             main='Scatter Plot of lac vs. map by #missingness',
             xlab = 'Mean Arterial Pressure (mmHg)',
             ylab = 'Lactate (mmol/L)',
             legend.plot = TRUE,
             id.method = 'identify',
             boxplots = 'xy')
 #此时的NA完全落在回归线上，虽与实际情况相似，但缺乏随机误差，仍不够完美
 #这样就人为地增加了map与lac之间的相关性
 
 #############################    回归插补 增加随机误差   ################################
 library(mice)
 imp<-mice(data[,2:3],method = 'norm.nob',m = 1,
                        maxit = 1, seed = 77)
 #norm.nob表示用线性回归来获得斜率，截距和残差，同时得到预测值；
 #m表示只处理一套数据，因为mice可实现多套数据的计算；
 #maxit=1表示迭代次数
 #该code得到带随机误差的预测值
 lac_stoc<-complete(imp,action = 1,include = FALSE)$lac_miss
 #complete可根据mice获得的结果去替代NA值；
 #第一个参数是mice返回的结果，action取值1表示返回第一套数据，取值2表示返回第二套数据，以此类推
 #include表示是否返回原始数据，默认为false
 scatterplot(lac_stoc~map|lac_miss_tag,lwd=2,
             main='Scatter Plot of lac vs. map by #missingness',
             xlab = 'Mean Arterial Pressure (mmHg)',
             ylab = 'Lactate (mmol/L)',
             legend.plot = TRUE,
             id.method = 'identify',
             boxplots = 'xy')
 ###此时得到的曲线与实际情况十分接近！！！！ 
 
 #############################    Indicator Method   ################################
 #对于分类变量：建立新的类别——‘缺失’；eg, gender:female,male,missing
 #连续变量：
 #1)建立新变量xfill：X中缺失变量设定为1个常数（0）
 #2)建立indicator变量，X变量中的缺失值用‘indicator’数值代替
 #3）将指示变量和xfill同时纳入回归模型
 #eg
      # wbc     wbcfill     wbc.ind
      # 1.2     1.2         0
      # 7       7           0
      # NA      0           1
      # 8       8           0 
      # 16      16          0
      # NA      0           1
 
 
 ##################    连续测量资料：连续值之间存在相关性  ###########################
 matMissing <- matrix(
   c(NA,1.8,NA,2.3,2.2,NA,1.4,NA,NA,1.1,
     9.4,8.4,NA,9.6,7.7,NA,8.1,NA,7.9,NA,
     3.1,NA,4,3.3,3.1,3.4,2.4,3,NA,2.1,
     5.1,4,5.6,NA,NA,4.1,4.4,NA,NA,6.2
   ),4,byrow=TRUE)
   matMissing
 library(longitudinalData)
 matplot(t(imputation(matMissing,'crossMean')),
                     type='b',ylim=c(0,10),
                     lty = 1,col = 1,main = 'crossMean',
                     ylab = 'Lactate values (mmol/L)'
         )
 matlines(t(matMissing),type = 'o', col = 2,lwd = 3,pch = 16,lty = 1)
 #crossMean：replace missing value with mean of values observed at that time
 #其它的插值方法详见BCT文献包第4篇文献
