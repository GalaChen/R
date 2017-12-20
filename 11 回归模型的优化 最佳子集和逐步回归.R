###回归模型的优化：最佳子集和逐步回归###
 library(MASS)
 head(birthwt)
 # low：出生体重小于2.5Kg的指示变量
 # age：母亲的年龄
 # lwt：母亲最后一次月经时的体重
 # race：母亲的种族；1为白人，2为黑人，3为其它
 # smoke：怀孕期间是否抽烟
 # ptl：之前早产的次数
 # ht：是否有高血压病史
 # ui：是否出现子宫刺激表现
 # ftv：前三月进行体检的次数
 # bwt：出生时的体重(g)
 
 ##########################   逐步回归法：从完全模型开始   ###########################
 full<-glm(low~.,family = binomial,data = birthwt)
 #~.表示对除low以外的所有变量
 summary(full)
 
 #逐步回归
 step_outcome<-stepAIC(full,trace = FALSE)
 step_outcome
 #最终会显示的剩余的自变量与因变量
 #trace选择是否显示迭代的过程
 
 #前向回归法
 forward<-stepAIC(full, direction = 'forward', trace = FALSE)
 forward
 #由于上述方法使用完全模型进行前向逐步回归，所以没有额外的变量可以加入，最终的模型即为完全模型
 #前向回归法可以以空模型开始，即模型中只包含截距
 
 #后向回归
 backward<-stepAIC(full, direction = 'backward', trace = FALSE)
 backward
 
 #逐步回归默认用AIC准则选择变量，也可选用BIC或SBC准则选择变量
 #此时，参数中增加一项为k=log(nrow(object))
 
 #参数scope指定纳入模型中的变量范围
 scope<-stepAIC(full,scope = list(lower=~smoke+age,upper=full),trace = FALSE)
 scope 
 #下层模型含有最少的变量，上层模型含有最多变量
 #如果scope为单一公式，则指定为上层模型，下层模型为空
 #如果scope缺失，则原始模型为上层模型
 
 #更加复杂的模型
 step_outcome2<-stepAIC(full,~.^2+I(scale(age)^2)+I(scale(lwt)^2),trace=FALSE)
 #^表示指定程度的交互
 #I()表示其内部的所有元素均以其算术的方式进行解读
 #scale()表示对目标变量进行标准化处理，即减去均值后再除以标准差
 #~.^2+I(scale(age)^2)+I(scale(lwt)^2)是上层模型的公式，由于单一公式，故下层模型为空
 step_outcome2
 
 ##########################   最佳子集   ###########################
 #假设有m个可供选择的变量x1,x2,…，xm,
 #由于每个自变量都有入选和不入选两种情况，
 #这样y关于这些自变量的所有可能的回归方程就有2m-1个
 #我们的目的是在这所有可能子集当中找到最佳模型
 
 library(bestglm)
 args(bestglm)
 #对于bestglm函数所需要的参数
 #Xy为含有独立变量和响应变量的数据框，最后一列必须为应变量
 #在bestglm，用于指定应变量和自变量的公式不可用，故Xy的顺序很重要
 #实际操作中，可以将应变量转移至最后一列，同时将该新数据框指定一个新的名字
 #logistic回归中，family设定为binomial，默认为线性回归的高斯分布（gaussian）
 #IC的值可以设定为AIC，BIC，BICg，BICq，LOOCV和CV
 
 #整理数据框
 birthwt_move<-birthwt[,-1]
 birthwt_move$low<-birthwt$low
 
 #将分类变量转换为哑变量
 library(dummies)
 race<-data.frame(dummy(birthwt$race)[,c(1,2)])
 ftv<-data.frame(dummy(birthwt$ftv)[,c(2,3)])
 birthwt_dummy<-birthwt[,-c(1,4,9)]
 low<-birthwt$low
 birthwt_dummy<-cbind(birthwt_dummy,race,ftv,low)
 
 bestglm(birthwt_dummy,IC = 'BIC', family = binomial)
 #注意一定要先转换为哑变量在进行计算