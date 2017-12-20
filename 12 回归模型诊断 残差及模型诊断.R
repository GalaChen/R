###回归模型的诊断：残差及模型诊断###
 library(car)
 str(Mroz)
 
 #残差
 #残差的种类很多，如普通残差(ordinary residual),皮尔森残差(Pearson residual)，学生化残差(Studentized residual)
 #残差反应拟合值和观测值之间的不同，是各种回归模型诊断方法的基础统计量
 
 ##########################   拟合模型及整体评价   ###########################
 mroz_mod<-glm(lfp~k5+k618+age+wc+hc+lwg+inc,family = binomial,Mroz)
 residualPlots(mroz_mod)
 #结果解读
 #皮尔森残差和lwg之间有一个趋势，它们之间存在非线性关系;线性关系其趋势线近似为直线
 #这一code可指导变量种类（哪些变量纳入模型）和形式（线性vs非线性）的选择
 
 #二次方程改进模型
 mroz_mod2<-glm(lfp~k5+k618+age+wc+hc+lwg+I(lwg^2)+inc,family = binomial,Mroz)
 #lwg加二次项，二次项前面记得添加I(),表示依照其数学含义进行运算
 residualPlots(mroz_mod2)
 #皮尔森残差和lwg的关系近似直线，效果比之前显著改善
 
 #边际图
 marginalModelPlots(mroz_mod)
 #参数为拟合好的模型
 #蓝色是观测值，红色是拟合值
 #目的是观察拟合值与观测值之间拟合情况
 #如图所示，lwg效果较差，其拟合曲线与观测曲线相差较大，说明拟合方式不对
 marginalModelPlots(mroz_mod2)
  
 ##########################   个别观察值评价   ###########################
 
 #以上的方法是针对模型的整体拟合情况进行评估，还需要对个别观察值进行评价
 
 #outlier 离群值
 #定义：即在其协变量模式中，有异常反应的观察值
 #例如：年龄超80岁，且伴有循环衰竭（低血压）和肾脏衰竭的患者极易死亡；
 #但此时如果发现具有这些特征的患者没有死亡，那它就是离群值
 #离群值可能对模型拟合会有重要的影响。离群值可以用学生化残差进行检验
 outlierTest(mroz_mod)
 
 #leverage 杠杆值
 # 定义：远离协变量模式（回归因子空间）平均值的观察值
 # 例如：参加高考的学生年龄在18到22岁之间。在这个列子中，76岁的应试者就具有较大的杠杆作用
 # 杠杆值可以用帽子（hat）值来统计描述。每一个观察值的帽子值都可以用hatvalues功能获得
 
 influenceIndexPlot(mroz_mod,id.n = 3)
 #这个code对多个统计量进行展示;
 #id.n表示取最大的前多少个数据在图中标识
 #图中标识的是第几个病人
 
 #influence 影响值
 #定义：如果某一个值的删除对回归模型系数的估算会产生本质的改变，该观测值即被称为影响值
 #影响值可以被认为是杠杆值和离群值的综合产物
 #库克距离是影响值的一种表示方法
 
 influencePlot(mroz_mod,col = 'red', id.n = 3)
 #结果解读
 #该code会显示三个结果，即outlier，leverage和influence
 #id.n=3表示对每一个结果均显示最大的3个，如果不重复则会出现9个值
 #圆圈越大，表示该值的影响越大
 
 #剔除119号进行模型更新
 mroz_mod119<-update(mroz_mod,subset = c(-119))
 compareCoefs(mroz_mod,mroz_mod119)
 #删除119前后，原始模型和新模型回归系数的变化非常小 
 #因此119不是影响值