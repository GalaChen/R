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
 #strata表示分层，所谓的分层即将strata的参数所涉及的变量从模型中去掉；
 #此处表示所建立的模型只针对年龄小于60岁的患者，即排除了年龄大于60岁的患者；
 #若此处为strata(age)，则表示完全去掉age这个自变量；
 #由于rx的回归系数为负数，HR值小于1，故可解读为rx在小于60岁的患者为危险因素；

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
 
 #Cox回归，有时也称为cox比例风险模型（这个名称更能体现它的内容和意义），尽管应用广泛，但也不是说任何生存数据都可以用它来分析；
 #它有一个重要的前提假设，即等比例风险（Porportional hazards），它表示某因素对生存的影响在任何时间都是相同的，不随时间的变化而变化。
 #如某基因对肿瘤的影响，不管是第一年、第二年、……，对肿瘤的危险都是相同的
 #只有满足这一条件，才能应用Cox回归，所以cox回归有的也称之为cox比例风险模型，而且有时候比例风险模型比cox回归更通用
 #因为比例风险模型有顾名思义的意味，从其名称就知道什么意思。
 
 #假设检验
 cox.zph(cph.ovarian)
 #结果中，p>0.05则满足假设，即所有的变量都是成比例的
 #比例模型使用的前提是曲线之间不能相交
 #验证cox回归等比例风险的方法有多种
 #第一种是图示法，绘制该因素在不同状态下的生存曲线图，如果生存曲线不交叉，表明等比例风险成立，否则提示等比例不成立
 #第二种可通过在模型中增加该变量与时间的交互作用项，如果交互作用项有统计学意义，则表明该变量在不同时间的作用不同，也就是说不满足等比例风险假设
 #第三种是利用Schoenfeld残差，如果Schoenfeld残差与时间t无明显的变化趋势，即Schoenfeld残差与时间t无关，则提示符合等比例风险假设。
 
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
 #人群归因分值（Population Attributable Fraction, PAF）；
 #又称人群归因危险度百分比（Population Attributable Risk Proportion, PAR%）
 #PAF是定量描述暴露危险因素对人群致病作用大小的统计指标
 #表示总人群中某疾病归因于某种因素引起的发病（或死亡）占总人群全部发病（或死亡）的比例
 #也可理解为消除某危险因素后可使人群中该病的发病（或死亡）降低的比重
 #计算公式定义为(P(D=1)-P(D=1|Z=0))/P(D=1),D为疾病状态（如死亡），Z是暴露因素（0表示未暴露）
 library(paf)
 par(mfrow=c(1,2))
 paf.adj<-paf(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss,
                data=lung, cov=c('age'))
 #cov传递的参数即为公式中的暴露因素
 paf.pop<-paf(Surv(time, status)~age, data=lung,cov=c('age'))
 plot(paf.pop,ylab="Population attributable fraction function")
 plot(paf.adj,ylab="Adjusted attributable fraction function")
 
 #paf.adj表示校正之后的数据
 #实线表示模型本身，虚线表示置信区间
 #paf.pop只针对age建模，paf.adj则是通过对多个自变量进行建模，age是其中一个自变量
 #paf.adj相较于paf.pop，其起始点更低且下降更平缓，说明age在多个协变量中的作用不如之前明显
 