###回归模型的建立：目的性建模###
 ##########################   模拟练习数据   ###########################
 set.seed(888)
 age<-abs(round(rnorm(1000,mean = 67,sd = 14)))
 lac<-abs(round(rnorm(1000,mean = 5,sd = 3),1))
 gender<-factor(rbinom(1000,1,0.6),labels = c('male','female'))
 wbc<-abs(round(rnorm(1000,mean = 10,sd = 3),1))
 hb<-abs(round(rnorm(1000,mean = 120,sd = 40)))
 z<-0.1*age-0.02*hb+lac-10
 pr = 1/(1+exp(-z))
 y = rbinom(1000,1,pr)
 mort<-factor(rbinom(1000,1,pr),labels = c('alive','die'))
 
 data<-data.frame(age,gender,lac,wbc,hb,mort)
 summary(data)

 ##########################   单变量分析   ###########################
 univariable_age<-glm(mort~age, family = binomial)
 summary(univariable_age)
 #建立回归模型之前需先进行单变量分析
 #单变量分析选择p值小于0.05的变量纳入回归模型；
 #也可通过经验判断选择纳入回归模型的变量
 
 univariable_gender<-glm(mort~gender, family = binomial)
 univariable_wbc<-glm(mort~wbc, family = binomial)
 univariable_hb<-glm(mort~hb, family = binomial)
 univariable_lac<-glm(mort~lac, family = binomial)
 
 ##########################   多变量模型比较   ###########################
 model1<-glm(mort~lac+hb+wbc+age, family = binomial)
 summary(model1)
 model2<-glm(mort~lac+hb+age, family = binomial)
 summary(model2)
 #可以发现，剔除wbc后各变量回归系数均有统计学意义
 #剔除某一变量后，若其它变量的回归系数变化大于20%或15%，可认为该变量是一个混杂因素
 #混杂因素即使没有统计学差异也不能随便被排除
 #若某一变量既没有统计学差异，又不是混杂因素，则可以将其从模型中排除掉
 delta_coef<-abs((coef(model2)-coef(model1)[-4])/coef(model1)[-4])
 #coef可以提取model中的回归系数，coef[-4]表示去除第四个回归系数后返回升序的回归系数
 #以上公式表示个变量回归系数的变化率
 round(delta_coef,3)
 
 ##########################   似然比检验对比多变量模型   ###########################
 library(lmtest)
 lrtest(model1,model2)
 #检验结果的p>0.05则说明两个模型没有显著性差异，从而说明是否剔除变量对结果无影响
 
 ##########################   方差分析对比多变量模型   ###########################
 anova(model1,model2,test = 'Chisq')
 #p>0.05则说明两个模型没有显著性差异，从而说明是可剔除多余变量
 
 ##########################   图示法判断线性假设是否成立   ###########################
 
 #如果散点图呈现非线性关系，则需要使用其他方法进行拟合
 #如二次方或三次方的函数，分数多样式和样条函数来建立模型
 par(mfrow = c(2,2))
 scatter.smooth(age,log(pr/(1-pr)),cex = 0.5)
 scatter.smooth(lac,log(pr/(1-pr)),cex = 0.5)
 scatter.smooth(hb,log(pr/(1-pr)),cex = 0.5)
 scatter.smooth(wbc,log(pr/(1-pr)),cex = 0.5)
 #cex是各个scatter的半径
 #log(pr/(1-pr)是对拟合模型的结果进行logist变换，从而使变换后的值与自变量呈线性关系
 
 ##########################   协变量间的交互   ###########################
 
 #根据经验，hb对死亡率的影响在某种程度上依赖age
 model_interaction<-glm(mort~lac+hb+age+hb:age,data=data,family=binomial)
 summary(model_interaction)
 #hb:age表示二者的交互关系；
 #统计结果发现，hb：age无统计学差异，说明二者无交互关系
 
 lrtest(model2,model_interaction)
 #似然比检验发现，两个模型无统计学差异
 
 ##########################   交互的多种表达方式即意义   ###########################
 model_interaction_1<-glm(mort~lac*hb*age,data=data,family=binomial)
 summary(model_interaction_1)
 #这相当于y~a+b+c+a:b+a:c+b:c+a:b:c；
 #即*可以得到所有可能的交互组合
 
 model_interaction_2<-glm(mort~(lac+hb+age)^2,data=data,family=binomial)
 summary(model_interaction_2)
 #这相当于y~a+b+c+a:b+a:c+b:c；
 #指数限定了交互的阶数
 
 ##########################   交互的可视化   ###########################
 newdata<-data.frame(hb=rep(seq(from=40,to=150),length.out=100,4),lac=mean(lac),age=rep(c(20,40,60,80),100))
 #seq得到等差数列，length.out限定了重复数据的总长度，最后一个参数限定了数列重复的次数
 #这一行代码重新构造了数据，总共100个病人的数据
 newdata1 <- cbind(newdata, predict(model_interaction,newdata = newdata, type = "link",se = TRUE))
 #将各参数以列的形式合并在一起
 #第二个参数是根据之前得到的拟合模型mort~lac+hb+age+hb:age，用现有的数据获得死亡率的拟合值
 #se = TRUE是返回标准差
 #type = 'link'返回fit值
 #predict返回的拟合值存储于‘fit’列中,标准差存储于‘se.fit'列
 newdata1 <- within(newdata1, {
   age<-factor(age)
   PredictedProb <- plogis(fit)
   LL <- plogis(fit - (1.96 * se.fit))
   UL <- plogis(fit + (1.96 * se.fit))
 })
 #将拟合值与标准差进行logist变换
 #plogis函数的目的是进行概率化处理，即将连续数据经logist变换转化为0-1的概率值
 library(ggplot2)
 ggplot(newdata1,
                 aes(x=hb,y=PredictedProb))+geom_ribbon(aes(ymin=LL,
                 ymax=UL, fill=age),alpha = 0.2)+geom_line(aes(colour = age),
                 size = 1)
 #aes()函数是ggplot2中的映射函数, 所谓的映射即为数据集中的数据关联到相应的图形属性过程中一种对应关系
 #第一个aes产生图形的背景区域，明确图形的横、纵坐标
 #geom_ribbon产生丝带图,也就是曲线上下间周围的色块（不包含色块中间的曲线），表示每个数据点的置信区间
 #geom_line产生的是色块中间的曲线，size表示曲线的宽度
 #aes(colour=age)表示以age作为分组条件，变换不同颜色；之前的代码已将age转换为factor
 #alpha调整geom_ribbon色块的透明度
 
 #结果解读
 #随年龄增加，死亡率增加；同一年龄层，随hb增加死亡率降低
 #若曲线无相交，则表明变量无交互作用
 ##########################   评估模型拟合度   ###########################
 #Hosmer-Lemeshow检验可对logistc回归进行拟合度评估
 library(ResourceSelection)
 hoslem.test(model2$y, fitted(model2))
 #第一个参数是模型中的实际值，第二个参数是根据模型拟合的值
 #若p>0.05，则说明二者之间没有差异，从而说明拟合效果很好
 
 ##########################   图示法观察拟合度   ###########################
 Predprob<-predict(model2,type="response")
 #type＝response，表示输出结果预测响应变量为1的概率
 plot(Predprob,jitter(as.numeric(mort),0.5),cex=0.5,ylab="Jittered mortality outcome")
 #jitter的目的是为了使数据抖动后交错
 #由于mort是二分类变量，数据点集中到1或者2，抖动后使得数据点分开，利于观察
 #所得数据与之前模拟的一致，证明拟合度较佳
 library(Deducer)
 rocplot(model2,AUC = TRUE)
 #ROC曲线
 library(lattice)
 histogram(Predprob,mort)
 #柱状图

 