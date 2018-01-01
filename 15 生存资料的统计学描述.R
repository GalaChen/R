###生存资料的统计描述###

 ##########################   载入数据   ###########################
 library(survival)
 str(lung)
 head(heart)
  
 ##########################   创建生存数据   ###########################
 lung_sur<-Surv(lung$time,lung$status)
 #如果只设置两个参数，默认为时间和事件状态
 heart_sur<-Surv(heart$start,heart$stop,heart$event)
 #如果有三个命名参数，则默认为time1\time2\event
 #Surv的参数形式如下，type表示删失的类型
 # Surv(time,time2,event,
 #      type = c('right','left','interval','counting','interval2','mstate'),
 #      origin = 0)
 
 ##########################   非参数模型：Kaplan-Meier   ###########################
 lung_fit<-survfit(lung_sur~1) #1的目的是抹去所有参数
 plot(lung_fit,xlab = 'Days', ylab = 'Proportion of subjects')
 #虚线为置信区间
 summary(lung_fit)
 #time表示时间（天），n.risk表示剩余人数
 #n.event表示该时间点下发生的事件数目
 #survival表示存活人数的比率
 
 ## 置信区间计算   
 library('km.ci')
 a<-km.ci(lung_fit,conf.level = 0.95,tl = NA, tu = NA, method = 'loghall')
 #tl与tu表示时间段，即low与up
 plot(a,lty=2, lwd=2, col = 4)
 lines(lung_fit,lwd=2,lty = 1, col = 2)
 lines(lung_fit,lwd = 1,lty = 4,conf.int = T, col = 3)
 linetype<-c(1,2,4)
 legend(600,.9,c('Kaplan-Meier','Hall-Wellner','Pointwise'),lty = (linetype))
 
 
 
 ##########################   生存曲线分层   ###########################
 lung_fit_strata<-survfit(lung_sur~ph.ecog,lung)
 #针对ph.ecog进行拟合
 plot(lung_fit_strata,lty = 1:4,col = 1:4, xlab = 'Days', ylab = 'Proportion of subjects')
 legend(700,.9,c('ph.karno=0',
                 'ph.karno=1',
                 'ph.karno=2',
                 'ph.karno=3'),
        lty =1:4,col = 1:4)
 #lty表示线的类型，col表示线的颜色
 #700，.9表示x与y的坐标
 
 ##########################   Nelson-Aalen 生存模型   ###########################
 aalen_fit<-survfit(coxph(lung_sur~1),type = 'aalen')
 #coxph表示建立cox生存模型
 summary(aalen_fit)
 
 plot(aalen_fit,col='red',lwd=1,lty=1)
 lines(lung_fit,lwd=1,lty = 1)
 legend(600,.9,c('Nelson-Aalen','Kaplan-Meier'),lty = c(1,1),col = c('red','black'))
 #Nelson-Aalen与Kaplan-Meier都是非参数生存模型，不考虑混杂因素的情况下，对不同条件下的生存时间分布进行分析
 #Nelson-Aalen是累计风险函数，而K-M是有限的生存回归函数
 #累积风险函数表示生存时间为T时，研究对象中已发生事件的概率
 ##########################   生存曲线比较   ###########################
 survdiff(lung_sur~ph.ecog,lung)
 #用lung_sur的数据以ph.ecog为分组依据进行分组比较
 #所得结果p<0.05说明各组之间具有明显差异
 
 ##########################   参数模型拟合：Weibull model   ###########################
 par_wei<-survreg(lung_sur~1,dist = 'w')
 #dist=w表示服从weibull拟合
 par_wei
 
 kappa<-par_wei$scale
 lambda<-exp(-par_wei$coeff[1])
 zeit<-seq(from=0,to=1100,length.out = 1000)
 s<-exp(-(lambda*zeit)^kappa)
 h<-lambda^kappa*kappa*zeit^(kappa-1)
 par(mfrow=c(2,1))
 plot(zeit,h,xlab = 'Days', ylab = 'h(t)')
 plot(zeit,s,xlab = 'Days', ylab = 's(t)')
 #参数拟合，其曲线显得较为光滑
 #当生存时间的分布符合一定的分布规律时，模型拟合需要使用weibull分布、指数分布等等