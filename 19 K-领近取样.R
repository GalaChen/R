###19 K-邻近取样###
 ##########################  数据模拟   ###########################
 set.seed(888)
 df1 <- data.frame(x1=runif(200,0,100), x2=runif(200,0,100)) #runif获取最小到最大值之间的随机数
 df1 <- transform(df1, y=1+ifelse(100 - x1 - x2 + rnorm(200,sd=10) < 0, 0, 
                                  ifelse(100 - 2*x2 + rnorm(200,sd=10) < 0, 1,    2)))
 # transform的作用：为原数据框添加新的列，改变原变量列的值，通过赋值NULL删除列变量
 # y有三种取值：0，1，2
 df1$y<-as.factor(df1$y)
 #将y转为factor
 df1$tag<-c(rep("train",150),rep("test",50))
 #将数据的前150行定位’train‘数据，后50行定位‘test’数据
 
 library(ggplot2)
 qplot(x1,x2,data = df1, colour = y, shape = tag)
 #观察数据的分布
 
 ##########################  运行KNN算法   ###########################
 library(class)
 train<-df1[1:150,1:2]  #取出训练集的数据
 train.label<-df1[1:150,3]   #取出训练集的分类结果
 test<-df1[151:200,1:2]     
 test.label<-df1[151:200,3]
 pred<-knn(train=train,test=test,cl=train.label,k=6)
 #knn返回的是对test数据集里观察对象的预测
 #cl参数明确具有准确分类的数据集的分类结果
 #k确定所需要的类别数目
 
 ##########################  拟合结果评估   ###########################
 library(gmodels)
 CrossTable(x = test.label, y = pred,prop.chisq=FALSE)
 #X为观察的分类，y为预测的分类，，二者进行比价得出四格表
 #返回的两个表，第一个是对每一个格子内容的描述，第二个是预测与观测值的对比
 
 ##########################  拟合准确性的评估   ###########################
 table<-CrossTable(x = test.label, y = pred,prop.chisq=TRUE)
 tp1<-table$t[1,1]
 tp2<-table$t[2,2]
 tp3<-table$t[3,3]
 #对角线上数据的个数，也就是真阳性数据的个数
 tn1<-table$t[2,2]+table$t[2,3]+table$t[3,2]+table$t[3,3]
 tn2<-table$t[1,1]+table$t[1,3]+table$t[3,1]+table$t[3,3]
 tn3<-table$t[1,1]+table$t[1,2]+table$t[2,1]+table$t[2,2]
 #真阴性数据的个数
 fn1<-table$t[1,2]+table$t[1,3]
 fn2<-table$t[2,1]+table$t[2,3]
 fn3<-table$t[3,1]+table$t[3,2]
 #假阴性数据的个数
 fp1<-table$t[2,1]+table$t[3,1]
 fp2<-table$t[1,2]+table$t[3,2]
 fp3<-table$t[1,3]+table$t[2,3]
 #假阳性数据的个数
 accuracy<-(((tp1+tn1)/(tp1+fn1+fp1+tn1))+((tp2+tn2)/(tp2+fn2+fp2+tn2))+((tp3+tn3)/(tp3+fn3+fp3+tn3)))/3
 #每一类别，真阳性和真阴性的总数/该类别所有数据的个数，其结果之和再除以3进行平均
 accuracy
 
 ##########################  敏感性和特异性的评估   ###########################
 sen1<-tp1/(tp1+fn1)
 #敏感性
 sp1<-tn1/(tn1+fp1)
 #特异性
 sen1
 sp1
 
 ##########################  多分类数据AUC曲线   ###########################
 library(pROC)
 multiclass.roc(response=test.label, predictor=as.ordered(pred))
 # as.ordered返回的有序factor
 # AUC为0.92，说明预测效果较好；0.5表示该模型无预测作用，1表示预测作用佳
 
 ##########################  Kappa一致性检验   ###########################
 library(psych)
 cohen.kappa(x=cbind(test.label,pred))
 #一般来说，Cohen's kappa系数分布在-1到1之间
 #若Cohen's kappa系数小于0，说明观察一致率小于机遇一致率，在实际研究中很少出现
 #若Cohen's kappa系数等于0，说明观察一致率等于机遇一致率，结果完全由机遇因素导致
 #若Cohen's kappa系数大于0，说明研究对象之间存在一定的一致性，Cohen's kappa系数越接近1，一致性越大。
 
 ##########################  调整K值对knn模型预测准确性的影响   ########################### 
 accuracyCal<-function(N) {
   accuracy<-1
   for (x in 1:N) {
     pred<-knn(train=train,test=test,cl=train.label,k=x)
     table<- table(test.label,pred)
     tp1<-table[1,1]
     tp2<-table[2,2]
     tp3<-table[3,3]
     tn1<-table[2,2]+table[2,3]+table[3,2]+table[3,3]
     tn2<-table[1,1]+table[1,3]+table[3,1]+table[3,3]
     tn3<-table[1,1]+table[1,2]+table[2,1]+table[2,2]
     fn1<-table[1,2]+table[1,3]
     fn2<-table[2,1]+table[2,3]
     fn3<-table[3,1]+table[3,2]
     fp1<-table[2,1]+table[3,1]
     fp2<-table[1,2]+table[3,2]
     fp3<-table[1,3]+table[2,3]
     accuracy<-c(accuracy, (((tp1+tn1)/(tp1+fn1+fp1+tn1))+((tp2+tn2)/(tp2+fn2+fp2+tn2))+((tp3+tn3)/(tp3+fn3+fp3+tn3)))/3)
   }
   return(accuracy[-1])
 }
 #再进行一次之前的准确率计算公式，汇编成函数   
 
 library(TeachingDemos)
 qplot(seq(1:150),accuracyCal(150),xlab="k values",ylab="Average accuracy",geom = c("point","smooth"))
 #geom表示图形的类别，point表示散点图，smooth表示平滑曲线
 subplot(
   plot(seq(1:30),accuracyCal(30), col=2,xlab='', ylab='',cex.axis=0.8),
   x=grconvertX(c(0,0.75), from='npc'),
   y=grconvertY(c(0,0.45), from='npc'),
   type='fig', pars=list( mar=c(0,0,1.5,1.5)+0.1) )
 #subplot表示在已有的图形上再添加新的图形
 