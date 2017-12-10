pao2<-round(rnorm(100,mean=300,sd=30))

fio2<-round(rnorm(100,mean=0.5,sd=0.1),2)
gcs<-round(rnorm(100,mean=80,sd=20)/10)
map<-round(rnorm(100,mean=65,sd=15))
dop<-round(rnorm(100,mean=10,sd=3),1)
dob<-rbinom(100,1,0.4)
epi<-round(rnorm(100,mean=10,sd=3)/100,2)
nor<-round(rnorm(100,mean=10,sd=3)/100,2)
bilirubin<-round(rnorm(100,mean=80,sd=20),1)
platelet<-round(rnorm(100,mean=180,sd=50))
cr<-round(rnorm(100,mean=150,sd=34))
uo<-rnorm(100,mean=1000,sd=500)
uo<-round(ifelse(uo>0,uo,-uo))

data_test<-data.frame(pao2,fio2,gcs,map,dop,dob,epi,nor,bilirubin,platelet,cr,uo)

#round 四舍五入，逗号后的参数表示取几位小数，/表示所得数据除以相应的参数（即小数点左移）
#rnorm 产生随机数，括号内的参数依次为数据量，均值和标准差
#rbinom 产生n个b(size,prob)的二项分布随机数
#ifelse 括号内参数表示“判别条件，Yes返回的结果，No返回的结果”

#生成新变量
###################          方法一           ######################################
###################       呼吸系统评分        ###################################### 
data_test$respiratory<-ifelse(data_test$pao2/data_test$fio2>=400,0,
                                    ifelse(data_test$pao2/data_test$fio2>=300,1,
                                       ifelse(data_test$pao2/data_test$fio2>=200,2,
                                          ifelse(data_test$pao2/data_test$fio2>=100,3,4))))
 head(data_test[,c('pao2','fio2','respiratory')])
 #data_test$pao2表示取数据框中特定的列；
 #注意ifelse的用法，每一个ifelse的括号内迭套一个ifelse，最后一个else可不表明，直接写条件结果即可
 
 #################          方法二            #######################################
 ###################      神经系统评分        ###################################### 
 data_test$neuro<-cut(data_test$gcs,breaks = c(3,5,9,12,14,15),labels=c(4,3,2,1,0),include.lowest = TRUE)
 #cut是分段函数，第一个参数表示作用对象，第二个数据表示分段的断点，第三个参数表示每一段的标签
 #cut函数所实现的分段是左开右闭，最后一个参数默认为false，选择TRUE则使最左边的分段为左右皆闭
 #cut函数返回的数据为factor；
 #factor是R语言的一种数据类型，用来存储类别变量(categorical variables)和有序变量
 #这类变量不能用来计算而只能用来分类或者计数。
 data_test$neuro<-as.numeric(as.character(data_test$neuro))
 #将factor数据先转换为文本，再转换为数值
 #factor数据不能直接转换为数值，必须先转换为字符型数据再转换为数值
 #factor转换成数值型数据 的规则是这样的：
 #一共有n个数，那么转换后的数字就会在1——n中取值，数字最小的取一，次小的取二，以此类推。
 data_test$neuro
 
 ###################          肾脏系统评分           ###################################### 
 cr_score<-ifelse(data_test$cr<110,0,
                  ifelse(data_test$cr<=170,1,
                         ifelse(data_test$cr<=229,2,
                                ifelse(data_test$cr<=440,3,
                                       4))))
 uo_score<-ifelse(data_test$uo>=500,0,
                  ifelse(data_test$uo>=200,3,
                         4))
 data_test$renal<-max(cr_score,uo_score)
 
 ###################          循环系统评分           ###################################### 
 map_score<-ifelse(data_test$map>=70,0,
                   1)
 dop_score<-ifelse(data_test$dop<=5,2,
                   ifelse(data_test$dop>5,3,
                          ifelse(data_test$dop>15,4,0)))
 epi_score<-ifelse(data_test$epi<=0.1,3,
                   4)
 nor_score<-ifelse(data_test$nor<=0.1,3,
                   4)
 dob_score<-ifelse(data_test$dob==1,2,
                   0)
 data_test$cardio<-max(map_score,dop_score,epi_score,
                       nor_score,dob_score)
 
 ###################          肝脏系统评分           ######################################
 bilirubin_score<-ifelse(data_test$bilirubin<20.5,0,
                         ifelse(data_test$bilirubin<=34.1,1,
                                ifelse(data_test$bilirubin<=102.5,2,
                                       ifelse(data_test$bilirubin<=205.1,3,4))))
 data_test$liver<-bilirubin_score
 
 ###################          凝血系统评分           ######################################
 plt_score<-ifelse(data_test$platelet<=20,4,
                   ifelse(data_test$platelet<=50,3,
                          ifelse(data_test$platelet<=100,2,
                                 ifelse(data_test$platelet<=150,1,0))))
 data_test$coagulation<-plt_score
 
 ###################          计算SOFA得分           ######################################
 data_test$SOFA<-rowSums(data_test[,c('respiratory','neuro','renal','cardio','liver','coagulation')])
 #mode可以判断数据类型
 #rowsums可根据行进行分组，然后求和

#连续变量重新编码
 data_test$oxyindex<-data_test$pao2/data_test$fio2
 data_test$berlin<-cut(data_test$oxyindex,breaks = c(min(data_test$oxyindex),100,200,300,max(data_test$oxyindex)),
                       labels = c('servere','moderate','mild','None'),include.lowest = TRUE)
 table(data_test$berlin)
 #table 函数对应的就是统计学中的列联表，是一种记录频数的方法。将数据中的factor分别计数后展现出来

#变量重新命名
 names(data_test)[11]<-'creatinine'
 names(data_test)
 #names()返回一个list，根据index即可对特定的变量进行重新赋值命名
 
 library(reshape)
 data_test<-rename(data_test,c(gcs='GCS',epi='epinephrine',nor='norepinephrine',dob='dobutamine'))
 names(data_test)
  
