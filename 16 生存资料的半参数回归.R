###生存资料的半参数回归###

 ##########################  概  述   ###########################
 # 参数模型：可估计生存时间，例如根据一些变量预测某肿瘤患者的生存时间（中位生存，5年生存率，10年生存率）
 # 半参数模型用于估计某个参数对生存时间的影响：例如了解放疗手段对肿瘤生存期的影响（相对指标HR）
 
 library(survival)
 str(ovarian)
 
 ##########################  构建半参数模型   ###########################
 library(survival)
 cph.ovarian<-coxph(Surv(futime, fustat)~rx+age, ovarian)
 #Surv函数构建生存数据框并作为因变量，rx和age为自变量
 #ovarian是数据来源处
 summary(cph.ovarian) 
 #结果解读
 #coef为变量系数，exp（coef)为HR值
 #Rsquare表示45.7%的变异可以用纳入的变量进行解释
 #Likelihood ratio test表示纳入变量与完全不纳入之间是否有差异
 
 ##########################  分层计算   ###########################
 cph.ovarian.str<-coxph(Surv(futime,fustat)~rx+strata(age>60), ovarian)
 #strata是分层函数，
 #此时纳入的变量只有rx
 summary(cph.ovarian.str)
 #coef为回归系数，exp(coef)为相对风险比
 #回归系数为正数，则相应的自变量具有增加风险的效果
 #？？？？？？？？？？？？究竟分层后如何进行比较

 strata_fit<-survfit(cph.ovarian.str,newdata=data.frame(rx=c(1,2)))
 summary(strata_fit)
 #结果以年龄为依据分为2组，每组有两个survival，分别代表rx=1或2时的survival
 plot(strata_fit,xlab="Survival time",ylab="Survival",lty=c(1,1,2,2),col=c(1,2,1,2))
 legend(850,0.5,
        legend=c("rx=1,age<60",
                 "rx=2,age<60",
                 "rx=1,age>60",
                 "rx=2,age>60"),
        col=c(1,2,1,2),lty=c(1,1,2,2))
 
 ##########################  变量重要程度可视化  ###########################
 library(rankhazard)
 cph_full<-coxph(Surv(futime, fustat)~rx+age+resid.ds+ecog.ps, ovarian,x=TRUE)
 rankhazardplot(cph_full,data=ovarian)
 #纵坐标为相对风险，横坐标为变量取值，分别对应四分位及中位数#
 #相同横坐标取值的情况下，图形最上方的变量是最重要的变量，或者说，越往上其重要程度越大
 #图中各点的值可在结果栏中找到
 
 ##########################  比例风险模型   ###########################
 
 #假设检验
 cox.zph(cph.ovarian)
 #结果中，p>0.05则满足假设，即所有的变量都是成比例的
 #比例模型使用的前提是曲线之间不能相交
 
 #假设检验可视化
 par(mfrow=c(2,1))
 plot(cox.zph(cph.ovarian))
 #经过转化后的各变量随时间未发生明显改变，说明比例模型假设成立
 
 #建立模型的假设: log-hazard与连续变量之间为线性关系
 library(splines)
 library(smoothHR)
 hr.plot<- smoothHR(data=ovarian, coxfit=cph_full)
 plot(hr.plot, predictor="age", prob=0, conf.level=0.95)
 #logHR与age成线性关系，从而说明假设检验成立
 
 #Attribute fraction funtion
 #该函数考虑到了危险因素在自然人群中的存在率
 library(paf)
 par(mfrow=c(1,2))
 paf.adj<-paf(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss,
                data=lung, cov=c('age'))
 paf.pop<-paf(Surv(time, status)~age, data=lung,cov=c('age'))
 plot(paf.pop,ylab="Population attributable fraction function")
 plot(paf.adj,ylab="Adjusted attributable fraction function")
 
 #paf.adj表示校正之后的数据
 #实线表示模型本身，虚线表示置信区间