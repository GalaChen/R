###中介分析###
 ##########################   模拟练习数据   ###########################
 set.seed(888)
 treat_flg<-rbinom(1000,1,0.3)
 crp<-round(abs(40*treat_flg+rnorm(1000,mean = 100,sd = 30)),1)
 lp<-10*treat_flg+0.02*crp+(rnorm(1000,-5,2))  #以treat和crp构建回归方程
 link_lp<-exp(lp)/(1+exp(lp))                 #logistic变换
 mort<-(runif(1000)<link_lp)                 #由于mortality是二分类变量，故通过判别得到true和False
                                            #runif是获得最小值和最大值之间的分布，二、三参数为min和max，默认为0与1
 df<-data.frame(crp,treat_flg,mort)
 
 head(df)
 
 ##########################   中介分析   ###########################
 library(mediation)
 model_m<-lm(crp~treat_flg,data = df[1:100,])
 #crp对treat_flg做线性拟合，仅使用前100行数据
 model_y<-glm(mort~treat_flg+crp,family = binomial,data = df[1:100,])
 #glm是广义线性模型，lm是线性模型；
 #广义线性模型其因变量为非正态分布；线性模型因变量与随机误差为正态分布
 #family用于描述其随机误差的分布模型
 med_out<-mediate(model_m,model_y,treat = 'treat_flg',mediator = 'crp',robustSE = TRUE, sims = 100)
 #第一个参数为中介变量，第二个参数为因变量
 #sims为模拟次数，默认为1000
 #treat为描述干预手段的变量
 #mediator为其本意
 
 summary(med_out)
 
 #结果解读
 #ACME：average causal mediation effect
 #ADE：average direct effect 
 #重点关注ACME(average), ADE(average)和Prop. Mediated (average)
 #这三个数据分别说明了中介效应均值，直接效应均值以及中介效应在总效应中所占的比值
 plot(med_out)
 #plot(med_out)展示的是结果中的ACME (treated)，ADE (treated)，Total Effect
 
 ##########################   敏感性分析   ###########################
 #confunder是中介变量与结果变量之间的一个变量，它同时对中介变量和结果变量产生影响
 #中介分析的前提假设是没有confounder
 #敏感性分析的目的就是研究confunder多大程度上影响中介变量
 
 probit_y<-glm(mort~treat_flg+crp,family = binomial(probit),data = df[1:100,])
 #binomial(probit)表示常规（normal）的二项分布
 med_out1<-mediate(model.m = model_m, model.y = probit_y, treat = 'treat_flg', 
                   mediator = 'crp', robustSE = TRUE, sims = 100)
 #robustSE ?????
 sens_out<-medsens(med_out1,rho.by = 0.1,effect.type = 'indirect', sims = 100)
 #rho.by表示递增步长
 #effecttype表示分析的是哪种效应
 #操作变量为mediat分析的结果
 par(mfrow = c(2,1))
 plot(sens_out, sens.par = 'rho', main = 'Mortality')
 
 #结果解读
 #横轴为rho值，其与曲线相交的绝对值越大，说明混杂效应越明显，需要极大的混杂效应才能反转中介效应，
 #进而说明该中介效应越明显
 #但目前没有明显的界限值能说明该中介效应可靠或不可靠