####Reshape程序包的数据处理#####

 ##########################   模拟练习数据   ###########################
 set.seed(123)
 id<-rep(1:3,each=3)
 time<-rep(1:3,3)
 PaO2<-round(rnorm(9,mean=70,sd=10))
 PcvO2<-round(rnorm(9,mean=40,sd=8))
 data<-data.frame(id,time,PaO2,PcvO2)
 
 head(data,9)
 
 ##########################  melt函数： “宽”数据格式转化为“长”数据格式   ###########################
 library(reshape)
 data_melt<-melt(data,id.vars=(c('id','time')),measure.vars=(c('PaO2','PcvO2')), variable_name = 'PO2')
 #id参数定义原始数据中某些变量在新数据框内保持不变，
 #measure.vars将原有变量定义为新变量；
 #variable_names定义新变量的变量名
 #由此就由原来的“宽”数据变为了“长”数据
 
 ##########################  cast函数：对数据进行各类型转换   ###########################
 cast(data_melt,id~PO2,mean)
 #第二个参数，左边表示不变的变量，右边是展开时的依据变量，
 #第三个参数表示运算时使用的函数
 #以id为分段依据计算每一段的平均值
 cast(data_melt,time~PO2,mean)
 cast(data_melt,id+time~PO2)
 #等式左边可定义两个变量，由加号表示
 cast(data_melt,id~time+PO2,subset = time< 3 & id <3)
 #subset是设定亚组
 #此时只选择时间小于3且id小于3的数据
 #结果展示时，也是每个病人，分次进行展示
 cast(data_melt,id~time~PO2)
 #此时id和time不变，PO2拆分后分别进行展开 
 cast(data_melt,id~time|PO2)$PaO2
 #‘|’表示条件运算符，指明针对PO2展开，且$指明展开的是PaO2
 
 ##########################  cast函数：计算边际值   ###########################
 cast(data_melt,time~PO2,mean,margins = c('grand_row','grand_col'))
 #根据PO2进行展开，边界值即为总数统计值
 #注意第二个参数是边界值需要进行的运算；
 #第三个参数是需要展示的边界值，即横行还是纵列需要展示边界值
 
 ##########################  拆分字符向量为多个列   ###########################
 #假设我们连续3天测量了三个实验室项目，其表示如下并存入DataFrame
 data_split<-data.frame(lac_1=2.3,lac_2=3.4,lac_3=4.5,wbc_1=12,wbc_2=11,wbc_3=6,hb_1=60,hb_2=77,hb_3=89)
 data_split
 
 variable_names<-colsplit(names(data_split),'_',c('lab','days'))
 #第一个参数表示操作对象，即data_split的变量名
 #第二个参数表示拆分规则，即以下划线为拆分规则，将其前后的字符拆分为两部分
 #第三个参数为拆分后的对象分别进行命名
 data_reshape<-cbind(variable_names,t(data_split))
 #t实现行列互换
 #cbind将转置后的data_split赋值给variable_names
 row.names(data_reshape)<-NULL
 #row.names为操作对象的行变量进行命名，这里不需要
 names(data_reshape)[3]<-'value'
 #为data_reshape的第三列命名
 data_reshape
 
 ##########################  生成研究队列基线特征   ###########################
 round(funstofun(mean,median,min,max,sd)(data$PaO2),1)
 round(funstofun(mean,median,min,max,sd)(data$PcvO2),1)
 #funstofun可将多个函数一起运算后以向量的形式返回结果，第二个括号内指明操作变量
 