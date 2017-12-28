###18 临床预测模型及评分体系构建###
 ##########################  数据模拟   ###########################
 library(dummies) 
 set.seed(666) 
 n <- 1500 
 lac<- round(rnorm(n,mean=5,sd=1.1),1) 
 age<- round(rnorm(n,mean=67,sd=10)) 
 smoking<- as.factor(sample(x=c("never","ever","smoking"), 
                            size=n, 
                            replace=TRUE, 
                            prob=c(0.5,0.3,0.2))) 
 smoking<-relevel(smoking,ref="never")  #设定factor的参考值，指定以never作为参考变量
 gender<- as.factor(sample(x = c("male","female"), 
                           size = n, 
                           replace = TRUE,    #replace=TRUE  表示可以随机重复抽取
                           prob = c(60,40))) 
 lp<-cbind(1,dummy(smoking)[,-1]) %*% c(0.07,1.5,3.2)+   # %*%为矩阵乘法，c(0.07,1.5,3.2)是每个因子的回归系数
   cbind(1, dummy(gender)[, -1]) %*% c(0.07,1.5)- 
   0.2*age+0.003*age^2+ 
   3*lac-0.25*lac^2-11 
 pi.x<- exp(lp) /(1 + exp(lp)) 
 mort.y <- rbinom( n = n, size = 1, prob = pi.x) 
 df <- data.frame(mort.y, smoking, gender,lac,age) 
 df$dataset<-sample(x=c("train","validate"),    
                    size=n,
                    replace=TRUE, 
                    prob=c(0.75,0.25)) 
 #dataset将整个data分成两个不同的亚组
 
 ##########################  LOESS曲线——根据数据绘制散点图及趋势线   ###########################
 df.cont<- df[df$dataset=="train", c("mort.y","lac","age")] 
 df.cat<- df[df$dataset=="train", c("smoking","gender")] 
 #将train亚组中的数值变量和分类变量区别开，[]内类似if语句，前一判断为真时，选取后面所列的变量
 ymark<-seq(0,1,0.1)   #y轴坐标轴的数值
 library(ggplot2) 
 for(var in names(df.cont)[-1]){ 
   xvar<-seq(min(df.cont[,var]),       #xvar定义横轴坐标轴数值，length.out定义需展示的数值个数
             max(df.cont[,var]), 
             length.out=10) 
   mypath <- file.path("E:/MIMICIII for SCI/R Test", 
                       paste(paste("plot",var,sep = "_"), 
                             "pdf", sep = ".")) 
   pdf(file=mypath) #file.path定义存储路径，pdf定义存储格式为pdf，从而使每次的作图能够自动保存
   gg<-ggplot(df.cont, aes(x=df.cont[,var],y=mort.y))+      #确定横轴与纵轴展示的变量
     geom_jitter(size=1, alpha=0.2,height=0.05)+            #将数据点抖动展开，从而便于观察，否则各数据点均集中于两条线上
     stat_smooth(method="loess",colour="blue", size=1.5)+   #绘制拟合曲线，方法为loess，loess是非参数的绘制方法，其不需要任何参数，故可以完全通过数据得到其走行趋势
     xlab(var)+                                   #x轴的标题
     ylab("Probability of Death")+                #y轴的标题
     theme_bw()+                                  #确定显示的主题，_bw是明暗主题
     geom_hline(yintercept=ymark,col="red")+      #为图形添加水平参考线，参考线的间隔参照ymark，颜色为red
     scale_y_continuous(breaks=ymark)+            #为连续变量添加小刻度，不添加此语句，则最终只显示最小，最大及中间值
     geom_vline(xintercept=xvar,col="green",alpha=0.5)+    #为图形添加垂直参考线，参考线的间隔参照横轴间隔 
     scale_x_continuous(breaks=round(xvar,2)) 
   print(gg) 
   dev.off() #关掉图形界面
 } 
 #ggplot绘图方式是一层一层的添加，也就是按照参数中的语句，一句句执行
 #loess曲线的目的，一方面是为了观察数据的走行趋势，另一方面是为了将死亡率相差不多的一段截点勾选出来，从而确定变量的分段
 
 ##########################  选择截点和参考基准   ###########################
 agecut<-c(50,60,70,75,80) 
 ageb<-"[40,50]" 
 laccut<-c(3,3.7,4.7,6.6) 
 lacb<-"[1.5,3]" 
 #根据所得图形选出截点和基准参考值
 cont.to.cat<-data.frame(id=c(1:nrow(df.cont))) 
 #将连续变量转换为分类变量存储于数据框中，数据框中的行数与df.cont相同，并构建出id变量
 for (var in names(df.cont)[-1]) { 
   cat<-cut(df.cont[,var],     #cut的目的是将连续变量分段
            breaks=c(min(df.cont[,var]),   #breaks确定截点数据，需包括最小值和最大值
                     get(paste(var,"cut",sep="")),   #paste返回变量名后，通过get函数获得变量的内容
                     max(df.cont[,var])),include.lowest= TRUE)   #include.lowest表示需当最小值也为截点时，最小值需要显示出来
   cat<-relevel(cat,ref=get(paste(var,"b",sep="")))  #根据之前定义的base，重新定义base数据
   cont.to.cat<-cbind(cont.to.cat,cat)       #将二者以列的形式合并到一起
 } 
 
 ##########################  计算每个level的分数   ###########################
 df.cont.to.cat<-cont.to.cat[,-1] # 去掉第一个变量，即id
 names(df.cont.to.cat)<-names(df.cont)[-1] #为新的数据框命名
 df.final<-cbind(cbind(df.cat,df.cont.to.cat), #将分类后的变量数据与原来的mort.y合并
                 mort.y=df.cont$mort.y) 
 mod<-glm(mort.y~., 
          df.final, family="binomial") #构建广义线性模型，因变量为mort.y，自变量为除mort.y以外的所有其它变量，构建logistics回归
 score<-round(coef(mod)[-1]) #去掉截距，并且保留整数
 score.cat<-score[1:3] #将分类变量保存至scor.cat
 score.cont<-score[4:length(score)] #将连续变量保存至score.cont
 #所谓的评分score，其实就是模型中的回归系数
 #没有的变量默认为0分
 
 ##########################  为每一位病人计算其连续变量得分   ###########################
 library(stringr) 
 var.cont<-as.character(1)            #字符’1‘赋值给变量var.cont
 for(var in names(score.cont)){ 
   var.red<-sub("(\\(|\\[)[0-9]+.*", "", var)   
   var.cont<-c(var.cont,var.red) 
 } 
 #将score中变量名后面紧跟的其它字符去掉
 var.cont<-unique(var.cont)[-1]      #执行完上一布操作后，变量名很多都有重复，因此需要将这些变量名唯一化
 for(var in var.cont){ 
   df[,paste(var,"points",sep=".")]<-as.numeric(NA) 
 }
 # df数据框中添加新的两列变量，用于记录lac和age的得分
 for (var in names(score.cont)){ 
   var.red<-sub("(\\(|\\[)[0-9]+.*", "", var) #提取每一level的变量名age/lac
   var.low<-as.numeric(str_extract(var,'(?<=(\\(|\\[))[0-9]+\\.*[0-9]*(?=\\,)')) #提取连续变量，每一个分类范围内的最小值与最大值；
   var.upper<-as.numeric(str_extract(var,'(?<=\\,)[0-9]+\\.*[0-9]*(?=\\])'))      #如"lac(3,3.7]" 最小值为3，最大值为3.7
   df[,paste(var.red,"points",sep=".")]<-ifelse( 
     df[,var.red]<=var.upper&df[,var.red]>=var.low, 
     score[var],df[,paste(var.red,"points",sep=".")]) 
 } 
 # 计算每个病人的得分
 # 如果病人相应的值在该变量范围内，则记录该范围内的得分，否则就为空
 # str_extract是提取某种类型的字符串
 for(var in var.cont){ 
   df[,paste(var,"points",sep=".")]<-ifelse( 
     is.na(df[,paste(var,"points",sep=".")]), 
     0,df[,paste(var,"points",sep=".")] 
   ) 
 } 
 #将df数据框中所有的NA改为0
 ##########################  为每一位病人计算其分类变量得分   ###########################
 var.cat<-names(df.cat)   #取分类变量的变量名
 for(var in var.cat){ 
   df[,paste(var,"points",sep=".")]<-as.numeric(NA) 
 } 
 #为分类变量添加两列，记为‘var.points'
 for (var in var.cat){ 
   score.var<-score.cat[grep(var,names(score.cat))] 
   #grep匹配两个参数，然后返回第一个参数在第二个参数中的index
   #score.var存储内容为score.cat中该index下相应的内容
   names(score.var)<-sub(var,"",names(score.var)) 
   for(i in 1:(length(levels(df[,var]))-1)){ 
     df[,paste(var,"points", sep=".")]<-ifelse( 
                 df[,var]==names(score.var)[i]& 
                   is.na(df[,paste(var,"points",sep=".")]), 
                 score.var[i], 
                 df[,paste(var,"points",sep=".")] 
               ) 
   } 
 } 
 # 针对每个变量的每个level，如果df中该变量的值与score.var的值相同，则记录其对应的score，否则为NA
 for(var in var.cat){ 
   df[,paste(var,"points",sep=".")]<-ifelse( 
     is.na(df[,paste(var,"points",sep=".")]), 
     0,df[,paste(var,"points",sep=".")] 
   ) 
 } 
 
 ##########################  为每一位病人计算其得分总和   ###########################
 df$score<-rowSums(df[,grepl("\\.+points",names(df))])
 head(df[,7:11])
 
 ##########################  将score替换为死亡概率并与实际死亡人数进行比较   ###########################
 glmod<-glm(mort.y~score, 
            df[df$dataset=="train",], 
            family="binomial") 
 #构建广义线性模型
 newx<-seq(min(df[df$dataset=="train",]$score), 
           max(df[df$dataset=="train",]$score)) 
 #根据score构建一个涵盖所有分数的list
 prd<-predict(glmod, 
              newdata=data.frame(score=newx), 
              type="response", 
              se.fit=T) 
 #以新的评分作为自变量进行预测计算
 count<-as.matrix(table(cut(df[df$dataset=="train",]$score, 
                            breaks=seq(min(df[df$dataset=="train",]$score), 
                                       max(df[df$dataset=="train",]$score)), 
                            include.lowest = T), 
                        df[df$dataset=="train",]$mort.y)) 
 # 一分为一个区间，计数每个区间内死亡与存活的人数各有多少
 # table函数可进行计数
 
 ##########################  根据score与实际结局概率的关系作图   ###########################
 par(mar=c(5,4,4,5)+.1)       #设置作图边际
 barplot(t(count), 
         main="Scores versus probability of death", 
         xlab="Scores", 
         ylab="Observed number of patients", 
         space=0, 
         col=c("yellow","lightblue")) 
 #barplot绘制柱状图，由于其目的就是计数每个bar，故数据对象为count
 legend("topleft",fill=c("yellow","lightblue",NA), 
        lty = c(NA,NA,1),lwd=c(NA,NA,2), 
        legend=c("Survivors","Non-survivors", 
                 "Predicted Prob."), 
        col=c("black"), 
        border = c("black","black",NA)) 
 #绘制图例说明
 par(new=TRUE) #在原图基础上添加新图
 plot(prd$fit~newx, 
      type="l",col="black", 
      lwd=2,xaxt="n",yaxt="n", 
      xlab="",ylab="") 
 #绘制概率曲线，type为line
 #注意此时的坐标轴标题均为空，即xaxt=‘n'
 polygon(c(rev(newx), newx), 
         c(rev(prd$fit+1.96*prd$se.fit), 
           prd$fit-1.96*prd$se.fit), 
         col = adjustcolor('grey80',alpha=0.5), 
         border = NA) 
 #绘制置信区间
 #为同时将两个上下两条置信区间曲线绘制上去，故设置回文数字
 lines(newx, prd$fit+1.96*prd$se.fit,
       lty = 'dashed', col = 'red') 
 lines(newx, prd$fit-1.96*prd$se.fit, 
       lty = 'dashed', col = 'red') 
 #绘制上、下两条置信区间的曲线
 axis(4) 
 #绘制右侧的坐标轴，其计数为从下方开始，顺时针计数
 mtext("Predicted probability of death", 
       side=4,line=3) 
 #最右边的轴的标题
 
 #结果解读
 #可看见左边几乎全为survivor，右边几乎全为Non-survivor
 #bar与曲线相似，故拟合较好
 
 ##########################  检验评分系统   ###########################
 
 #需要对拟合出来的模型进行检验，检验的数据来源于rms包中的数据
 
 library(rms) 
 ddist <- datadist(df) 
 options(datadist='ddist') 
 #明确原始数据的分布特征
 f.score<-lrm(mort.y~score, 
              df[df$dataset=="train",], 
              x=TRUE,y=TRUE) 
 # 用之前的数据再次拟合，注意拟合数据集为‘train’
 phat.score<-predict(f.score, 
                     df[df$dataset=="validate",], 
                     type="fitted") 
 #根据f.score,用validate数据集进行计算
 v.score<-val.prob(phat.score, 
                   df[df$dataset=="validate",]$mort.y, 
                   m=20) 
 # val.prob -- Validate Predicted Probabilities 用于检测预测模型的可行性
 # 第一个参数输入预测出来的概率
 # 第二个参数输入观测的二分类结果
 # m = 20 表示横轴递进的步长
 
 # 结果解读
 # ideal所代表的线是最理想的曲线
 # logistics calibration是根据logistics回归校准的曲线
 # nonparametric 是无参数输入情况下直接反映的数据趋势
 # Grouped observation是实际值的分组观测值，就是前面的m=20所分的组
 # 左上角有些统计学数据，其中ROC可以看出预测效果，slope是预测曲线斜率，其越接近于1则拟合效果越好
 