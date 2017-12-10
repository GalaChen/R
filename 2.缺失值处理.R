#缺失值处理
 lactate<-c(0.2, 3.6, 4.2, NA, 6.1, 2.5)
 is.na(lactate)
 which(is.na(lactate)) #is.na可以寻找NA值，which返回index
 
 x1<-c(1,4,3,NA,7,8,NA,7,7)
 x1 < 0  #当变量中存在NA，并进行逻辑判断时，NA不判别，而是以NA体现 
 x1 == NA #NA不可以和0进行比较，NA也不可以和其它数值进行比较
 is.na(x1) <- which(x1==7) #函数is.na可以用于创建NA值
 
#带有缺失值的计算
 mean(lactate)
 sum(lactate)
 var(lactate)
 sd(lactate)
 
 mean(lactate,na.rm = TRUE) #na.rm作为逻辑判据，确定是否在计算前去除NA值
 sd(lactate,na.rm = TRUE)
 
#回归方程里的缺失值处理
 ptid<-c(1,2,3,4,5,6)
 data_test = data.frame(ptid,lactate)
 
 model.omit<-lm(ptid ~ lactate, data=data_test, na.action = na.omit)
 #lm为线性回归函数，‘~’前后分别表示因变量和自变量，data明确需要操作的data frame, 
 #na.action 用于明确对NA值进行的操作，na.omit表示删去缺失值
 model.exclude<-lm(ptid ~ lactate, data=data_test, na.action = na.exclude)
 #na.exclude表计算中示去除NA值，但数据展现时依旧显示NA
 
 resid(model.omit)#计算残差
 resid(model.exclude)
 
 fitted(model.omit)#计算拟合值
 fitted(model.exclude)

#带有缺失值的数据框
 sex<-c('m','f',NA,'f','m','m') #NA在不同的变量类型中，其表现形式也有所不同
 lactate<-c(0.2,3.3,4.5,NA,6.1,5.6)
 data_modify<-data.frame(ptid,sex,lactate)
 data_modify
 na.fail(data_modify)
 #na.fail可判定变量中是否有缺失值，如果有NA则返回报错语句；如果没有NA，则返回变量的内容
 #注意na.fail与is.na区别！！！
 #is.na是对变量里的每一个值进行逻辑判断，而na.fail则是对变量进行整体判断
 
 na.omit(data_modify)
 #省略变量中的缺失值
 
 complete.cases(data_modify)
 #对于data frame，确定每一个rou是否完整。没有NA为TRUE，有NA为False
 
 complete_data<-data_modify[complete.cases(data_modify),]
 complete_data
 #对data frame自身进行操作，使data frame完成判定条件后展现出来
 na.fail(complete_data)

#缺失值的寻找定位
 is.na(data_modify)
 #再啰嗦一下
 #is.na对每一个值进行判定，而na.fail则返回整体的情况，只确定是否有NA
 which(is.na(data_modify))
 #which返回判定条件为真的index
 #注意data frame是从第一列开始，先自上而下，再从左至右进行排序
 unique(unlist(lapply(data_modify,function (x) which(is.na(x)))))
 #lapply(x,FUN)lapply的返回值是和一个和X有相同的长度的list对象，
 #这个list对象中的每个元素是将函数FUN应用到X的每一个元素。
 #其中X为List对象（该list的每个元素都是一个向量）
 #其他类型的对象会被R通过函数as.list()自动转换为list类型。
 #此处，data_modify中的每一列为一个向量进行操作，返回的值为向量中对应的index
 
 #unlist是将list全部展开为数值
 #unique去除变量中的重复值

#删除缺失超过10%的变量
 missing_percent<-unlist(lapply(data_modify, function(x) sum(is.na(x))))/nrow(data_modify)
 #注意sum在这里只针对判定条件为TRUE的因素进行计算
 #nrow及ncol返回data frame行与列的数量
 missing_percent
 data_tenth <- data_modify[,missing_percent<=0.1]
 data_tenth
 #根据判定条件，实现只显示缺失小于10%的factor
 #data frame[i,j]表示取第i行第j列的元素；i，j也可以用条件判定来替代
 
#有意义的缺失值
 data_discrete<-data.frame(ptid=c(1,2,3,4,5),
                           lactate=c('low',NA,'moderate',NA,'high'),
                           death=c('y','y',NA,'n','y'))
 data_discrete
 na.fail(data_discrete)
 
 #乳酸值为分类变量，根据临床实践经验，乳酸值的缺失是由于患者病情稳定而不需要抽动脉血气
 #因为乳酸是动脉血气里的一个项目
 #此处如果将缺失值的病人删除会损失大量的信息
 
 library(tree)
 new_data_discrete<-na.tree.replace(data_discrete)
 na.fail(new_data_discrete)
 #na.tree.replace 针对的是分类字符型变量
 #可在data frame中添加一个新的名为NA的level，以此避免针对NA的运算对NA level发生作用
 
 
 library(gam)
 new_data_discrete_continueNum<-na.gam.replace(data_modify)
 new_data_discrete_continueNum
 na.fail(new_data_discrete_continueNum)
 #gam对连续变量的NA进行操作，用平均值来替代NA值，
 #字符型变量中的NA也会被去字符化
