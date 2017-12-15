#### 单变量分析 #####

 ##########################   模拟练习数据   ###########################
 set.seed(12)
 gender<-factor(rbinom(100,1,0.4),levels = c(0,1),labels = c('male','female'))
 #第一个参数返回分类数据；
 #第二个参数对分类数据进行数值定义
 #第三个参数对分类数据进行描述
 trt<-factor(rbinom(100,1,0.5),levels = c(0,1),labels = c('treat','control'))
 diagnosis<-factor(rbinom(100,3,0.5),levels = c(0,1,2,3),labels = c('heart failure','renal dysfunction',
                                                                    'ards','trauma'))
 #rbinom中的3和0.5可以这么理解，3表示进行了3次实验，最终可得到四种可能的结果，0.5是每次实验所得结果的概率
 age<-rnorm(100,mean = 67,sd = 20)
 wbc<-round(exp(rnorm(100,mean = 9,sd = 0.8)))
 data<-data.frame(gender,trt,diagnosis,age,wbc)
 
 head(data)
 
 ##########################   1 判断连续变量是否为正态分布   ###########################
 par(mfrow=c(1,2))
 hist(data$age)
 hist(data$wbc)
 #hist做变量数据的柱状图，注意柱状图的纵坐标是频数
 #此为图形方法观察数据是否符合正态分布
 
 library(moments)
 moments::agostino.test(data$age)
 #注意其假设为‘数据有偏斜’，即p<0.05则说明数据偏斜，不符合正态分布
 moments::agostino.test(data$wbc)
 #以上为连续变量的偏度(skewness)检验
 #skewness>0,则数据向实数轴左侧偏斜，即大部分数据集中在左侧；
 #skewness<0,则数据向实数轴右侧偏斜，即大部分数据集中在右侧；
 
 moments::anscombe.test(data$age)
 moments::anscombe.test(data$wbc)
 #以上为连续变量的分度(kurtosis)检验
 #kurtosis越大，则图像越尖，反之则越圆钝
 #正态分布的kurtosis约为3，故检验假设为kurtosis是否为3
 
 ##########################   2 统计描述   ###########################
 summary(age)
 sd(age)
 summary(wbc)
 table(diagnosis) #分类计数
 prop.table(table(diagnosis)) #分类比例
 
 tapply(data$wbc,data$trt,summary)
 
 table(data$diagnosis,data$trt) 
 #第二个参数是分类依据
 prop.table(table(data$diagnosis,data$trt),margin = 2)
 #prop.table计算频率
 #margin=1表示每个数据相对于所在行数据总和的频率
 #margin=2则表示相对于列的频率
 
 ##########################   3 统计推断   ###########################
 wilcox.test(wbc~trt,data = data)
 #非正态分布的单因素秩和检验
 #wbc~trt表示以trt为分组变量，wbc为目标变量进行t-test检验
 #p<0.05证明两组有显著性差异，反之则没有
 chisq.test(table(data$diagnosis,data$trt))
 #四格表数据卡方检验
 #运算前提是先把四格表做好
 t.test(age~trt,data=data)
 #连续正态分布变量的t检验
 #p<0.05表示两组有显著性差异
 
 ##########################   4 自动作图   ###########################
 #####正态分布连续变量数据
 overall_age<-paste(round(mean(data$age),1),"±",round(sd(data$age),1))
 #paste函数是将各参数转为字符串后再连接到一起；
 #该代码的目的是获得age变量总体的平均值和标准差
 trt_age<-paste(round(t.test(age ~ trt,data=data)$estimate[1],1), 
                "±",
                round(tapply(data$age,data$trt, sd)[1],1))
 #tapply，其第一个参数是目标变量，第二个参数是分组依据，第三个参数是操作函数
 #注意其均值是采用的t-test检验结果，t-test返回值的变量名为estimat，为vector
 #此处的均值也可以用tapply(age, trt, mean)[1]求得
 contrl_age<-paste(round(t.test(age ~ trt,data=data)$estimate[2],1),
                   "±",
                   round(tapply(data$age,data$trt, sd)[2],1))
 age_p<-round(t.test(age ~ trt, data=data)$p.value,2)
 #t-test的p-value求取方法
 row_age<-c(overall_age,trt_age,contrl_age,age_p)
 #将所得数据排列在同一行
 
 
 
 ####非正态分布连续变量
 overall_wbc<-paste(summary(data$wbc)[3],"(",summary(data$wbc)[2],"~",summary(data$wbc)[5],")")
 wbc_trt_all<-tapply(data$wbc, data$trt,summary)$treat
 wbc_control_all<-tapply(data$wbc, data$trt,summary)$control
 trt_wbc<-paste(wbc_trt_all[3],"(",wbc_trt_all[2],"~",wbc_trt_all[5],")")
 #非正态分布用四分位数及中位数描述
 contrl_wbc<-paste(wbc_control_all[3],"(",wbc_control_all[2],"~",wbc_control_all[5],")")
 wbc_p<-round(wilcox.test(wbc ~ trt, data=data)$p.value,2)
 row_wbc<- c(overall_wbc,trt_wbc,contrl_wbc,wbc_p)
 
 ####分类变量
 overall_gender<-paste(table(data$gender)[1],"(",prop.table(table(data$gender))[1]*100,"%",")")
 trt_gender<-paste(table(data$gender,data$trt)[1],
                   "(",round(prop.table(table(data$gender,data$trt),margin=2)[1],3)*100,"%",")")
 #四格表所求频数和频率按从上至下，从左往右的顺序进行排列
 #注意频率求取方法及百分数表示方法
 contrl_gender<-paste(table(data$gender,data$trt)[3],
                      "(",round(prop.table(table(data$gender,data$trt),margin=2)[3],3)*100,"%",")")
 p_gender<-round(chisq.test(table(data$gender,data$trt))$p.value,2)
 row_gender<-c(overall_gender,trt_gender,contrl_gender,p_gender)
 
 table<-rbind(row_age,row_wbc,row_gender)
 #rbind表示将各行内容结合到一起形成表格；
 #类似的，cbind就是将各列内容结合到一起形成表格
 
 row.names(table)<-c('Age(years)','Gender(male,%)','WBC/uL')
 table_pub<-rbind(c('overall','Treatment','Control','p value'),table)
 
 table_pub
 