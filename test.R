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
