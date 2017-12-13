####多重插补####

 ###########################   多重插补概述   ###########################
 # 多重插补过程如下：
 # 含有缺失数据的数据框→→mice()→→插补后数据框→→with()→→统计量估计→→pool()→→最终统计量
 # 具体来说：
 # 1.建立回归模型，以含缺失值的变量为应变量，其它变量为自变量；
 # 2.将其他变量代入回归模型来预测缺失值
 # 3.所得结果带有不确定性，需要多重插补

 ##########################   缺失数据框的模拟   ###########################
 set.seed(8888)
 sex_raw<-rbinom(150,1,0.45)
 sex_raw[sex_raw==1]<-'male'
 sex_raw[sex_raw==0]<-'female'
 sex_miss_tag<-rbinom(150,1,0.3)
 sex<-ifelse(sex_miss_tag == 1, NA, sex_raw)
 
 map<-round(abs(rnorm(150,mean=70,sd=30)))
 map<-ifelse(map<=40, map+30, map)
 
 lac_raw<-rnorm(150,mean = 5, sd = 0.7)-map*0.04
 lac_raw<-abs(round(lac_raw,1))
 lac_miss_tag<-rbinom(150,1,0.3)
 lac<-ifelse(lac_miss_tag == 1, NA, lac_raw)
 
 mort<-rbinom(150,1,0.25)
 mort[mort==1]<-'dead'
 mort[mort==0]<-'alive'
 
 data<-data.frame(sex,map,lac,mort)
 
 library(VIM)
 aggr(data,numbers = TRUE, prop = FALSE)
 
 ##########################   缺失数据多重插补   ###########################
 library(mice)
 imp<-mice(data,seed=8888) #注意加上seed，以实现code的可重复性

 #imp结果如下：
 imp
 # Multiply imputed data set
 # Call:
 #   mice(data = data, seed = 8888)
 # Number of multiple imputations:  5   多重计算的次数，默认为5，即最后得到5套插值结果，在结果中变现为5列
 # Missing cells per column:            每一变量（即每一列）缺失数据的统计结果
 #   sex  map  lac mort           
 # 46    0   51    0 
 # Imputation methods:                  缺失值插补的方法：sex用logistic回归，lac用预测均值匹配（Predictive mean matching ）
 #   sex      map      lac     mort 
 # "logreg"       ""    "pmm"       "" 
 # VisitSequence:                       访问顺序
 #   sex lac 
 # 1   3 
 # PredictorMatrix:                     预测矩阵：‘1’代表使用了列变量去预测行变量，‘0’代表否。预测矩阵可更改
 #   sex map lac mort                     
 # sex    0   1   1    1
 # map    0   0   0    0
 # lac    1   1   0    1
 # mort   0   0   0    0
 # Random generator seed value:  8888   sedd序号，保证数据的可重复性
 
 ##########################   缺失数据多重插补----改变预测自变量   ###########################
 predmatrix<-1-diag(1,ncol(data)) #diag将matrix的对角线改为1，第一个参数是希望添加的数
 predmatrix[c(2,4),]<-0           #将第二、四行改为0
 predmatrix[1,3]<-0               #将第1行，第3列的数改为0，由此获得期望到的预测矩阵，也就是lac不去预测sex
 
 imp_nolac<-mice(data,predictorMatrix = predmatrix,seed = 888,print=FALSE)
 imp_nolac
 
 ##########################   查看插补的数据   ###########################
 head(imp$imp$lac,10)
 #mice返回的数据为list，其子数据中含有一个同名数据框，
 head(complete(imp,action = 3),10)
 #complete是mice包里的函数，可将插值填补会原始data frame中，action参数指明所得插值结果的第几列
 
 ##########################   插补后的单变量分析   ###########################
 tttest<-with(imp,t.test(lac~mort))
 #with函数的作用在一个从data构建出的环境中运行R表达式
 #这样，在表达式里我们就不需要再注明用到的变量来自于data了
 #t.test是进行t检验
 tttest
 #根据之前算出的5套不同的插值，会得到5套t检验结果
 #结果解释
 #用mort作分组变量，分别比较死亡组和存活组的lac是否有统计学差异
 #95%的置信区间指的是t值的置信区间
 
 ##########################   插补后的回归模型分析   ###########################
 fit<-with(imp,glm(mort~sex+map+lac,family = binomial))
 #glm是广义线性模型，其既包括普通线性模型，又包括logistic回归模型
 #此处的因变量是mort，二分类变量，故family选用‘binomial’
 #familys是猫叔模型中使用的误差分布计算方法
 fit
 #所得的回归系数中，sex2的解释是指sex为二分类变量，故其分为sex1与sex2
 
 ##########################   综合回归模型的结果   ###########################
 pool_fit<-pool(fit)
 #pool的作用是将所得的多套回归模型结果进行合并
 round(summary(pool_fit),2)
 #结果解释：
 #est是综合5套数据得到的均值
 #se是standard error，t是t检验的t值，df为自由度，Pr lo 95为置信区间
 #nmis 缺失数据数量
 #fmi Fraction of missing information，即缺失数据所占的比例
 #Lambda:因数据缺失导致的变异占总变异的比例；
 #由于5套数据中的回归系数不同，由数据缺失导致的变异比例即为lambda值
 