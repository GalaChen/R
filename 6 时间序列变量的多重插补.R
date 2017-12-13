####时间序列数据多重插补#####

 ###########################   时间序列数据概述   ###########################
 
 #时间序列数据：在时间上逐渐变化，前后变量之间存在一定关系
 #例如：
 #手术麻醉中血压、心率等生命体征的变化；
 #脓毒症休克患者每天或每小时血乳酸水平；
 #空气中每日PM2.5浓度值
 
 ##########################   模拟练习数据   ###########################
 set.seed(1234)

 id<-rep(1:15,each=10)
 #rep函数可重复复制数据，第一个参数是复制的目标，第二个参数是复制的次数；
 #如果第二个参数为数字，则代表第一个参数整体复制多少次；
 #如果第二个参数为each=，则代表第一个参数的每一个数字复制多少次；
 #详情对比rep(1:10,15)和rep(1:10,each=15)
 time<-rep(1:10,15)
 
 map_raw<-abs(round(rnorm(150,mean=50,sd=25)))
 map<-round(ifelse(map_raw>=30,map_raw,map_raw+50))
 
 lac<-round(3-map*0.06+rnorm(150,mean=0,sd=0.4)-0.4*time*time+4.3*time,1)
 lac_miss_tag<-rbinom(150, 1, 0.3)
 lac_miss<-ifelse(lac_miss_tag==1,NA,lac)
 
 age<-rep(round(abs(rnorm(15, mean = 65, sd = 19))),each=10)
 
 data<-data.frame(id,time,age,map,lac_miss)
 
 head(data,12)

 library(VIM)
 aggr(data,numbers = TRUE, prop = FALSE)
 
 ##########################   数据插补   ###########################
 a_out<-Amelia::amelia(data,m=5,ts='time',cs='id')
 #第一个参数指明操作的数据框
 #第二个参数指明有几套数据
 #第三个参数指明用于区别连续型时间变量的column number或者是变量名
 #第四个参数指明用于分段计算的分段变量名或者column number
 
 summary(a_out)
 #结果解读
 #Rows after Listwise Deletion:  105 表示删除带缺失数据的行以后，有105行数据
 #Rows after Imputation:  150 表示经过插值运算后，得到150行完整的数据
 #Patterns of missingness in the data:  2 表示有2中类型的缺失数据,即aggr所得的统计图表现
 #Fraction Missing for original variables：各变量缺失数据的比例
 
 ##########################   查看插补效果   ###########################
 Amelia::tscsPlot(a_out,cs=c(3,4,5,6),var = 'lac_miss')
 #第二个参数指明查看的病人；
 #第三个参数指明查看的变量
 #结果解释：
 #红色表示插补的数据及其置信区间，由图可见，其置信区间比较宽泛
 
 ##########################   考虑时间因素   ###########################
 a_out2<-Amelia::amelia(data,m=5,ts='time',cs='id',polytime = 2)
 Amelia::tscsPlot(a_out2,cs=c(3,4,5,6),var='lac_miss')
 #polytime表示由时间来进行插值计算时，其阶数大小，取值在0-3之间，0表示常数，1表示线性关系，依次类推
 #此时插补结果来看，置信区间较之前小，但平滑度不够好
 
 ##########################   Lags and Leads   ###########################
 a_out3<-Amelia::amelia(data,m=5,ts='time',cs='id',lags='lac_miss',leads='lac_miss')
 Amelia::tscsPlot(a_out3,cs=c(3,4,5,6),main = 'with lags and leads',var='lac_miss')
 #这一算法是考虑插值数据点前后的值，
 #此时平滑性较之前好，但置信区间相对较大
 
 ##########################   加入先验信息   ###########################
 #有时根据经验或文献结果可知，某个缺失值变量有一定取值范围
 #如文献报道，lac均值为3的患者容易存活，而且其标准差为1.2
 data[data$id == '6',]
 pr<-matrix(c(53,57,60,5,5,5,3,3,3,1.2,1.2,1.2),nrow = 3,ncol = 4)
 pr
 #用matrix构建先验矩阵，第一个参数传递矩阵需要的所有数值，二、三参数定义行与列的参数
 #注意矩阵是从左往右，先上后下进行index排序
 a_out_pr<-Amelia::amelia(data,ts='time',cs='id',priors=pr)
 #注意这里的先验信息用参数priors传递
 Amelia::tscsPlot(a_out_pr,cs=c(3,4,5,6),main = 'with pr',var='lac_miss')
 
 ##########################   根据取值范围加入先验信息   ###########################
 #假设第6例患者，其取值范围在3-5之间
 bds<-matrix(c(5,3,5),nrow = 1,ncol = 3)
 bds
 
 a_out_bds<-Amelia::amelia(data,ts='time',cs='id',bounds=bds,max.resample = 1000)
 #bounds参数传递取值范围；max.resample为计算次数
 Amelia::tscsPlot(a_out_bds,cs=c(3,4,5,6),main = 'with bounds',var='lac_miss')
 
 ##########################   插补值诊断：密度曲线   ###########################
 
 par(mfrow=c(2,2))
 Amelia::compare.density(a_out, var = "lac_miss",main="without polynomials of time",legend = FALSE)
 Amelia::compare.density(a_out2, var = "lac_miss",main="with polynomials of time",legend = FALSE)
 Amelia::compare.density(a_out_pr, var = "lac_miss",main="lags and leads",legend = FALSE)
 Amelia::compare.density(a_out_bds, var = "lac_miss",main="bounds of 3-5",legend = FALSE)
 #对比原来的值与插补值是否存在系统性差异
 
 ##########################   插补值诊断：over-imputation   ###########################
 par(mfrow=c(2,2))
 Amelia::overimpute(a_out,var = 'lac_miss', main = 'without polynomials of time')
 Amelia::overimpute(a_out2, var = "lac_miss",main="with polynomials of time")
 Amelia::overimpute(a_out_pr, var = "lac_miss",main="lags and leads")
 Amelia::overimpute(a_out_bds, var = "lac_miss",main="bounds of 3-5")
 #绿色的点表示计算的插值，绿色的线条表示置信区间
 #若绿色的点均位于对角线上，则证明插值计算较为可靠；反之，若离得较远，则偏差较大
 #方框下面的颜色代表缺失数据的比例，本例中，缺失数据在0.3，故都为绿色
 
 
 
 
 # imp1<-a_out$imputations[[1]]
 # imp2<-a_out$imputations[[2]]
 # imp3<-a_out$imputations[[3]]
 # imp4<-a_out$imputations[[4]]
 # imp5<-a_out$imputations[[5]]
 # library(ggplot2)
 # p<- ggplot(data =data[31:60,], aes(x = time, y = lac_miss, group = id))
 # p+
 # geom_point(data=imp1[31:60,],aes(colour="red"))+
 # geom_line(data=imp1[31:60,])+
 # geom_point(data=imp2[31:60,],aes(colour="red"))+
 # geom_line(data=imp2[31:60,])+
 # geom_point(data=imp3[31:60,],aes(colour="red"))+
 # geom_line(data=imp3[31:60,])+
 # geom_point(data=imp4[31:60,],aes(colour="red"))+
 # geom_line(data=imp4[31:60,])+
 # geom_point(data=imp5[31:60,],aes(colour="red"))+
 # geom_line(data=imp5[31:60,])+
 # geom_point(data=data[31:60,])+
 # facet_grid(. ~ id)
 
 > 