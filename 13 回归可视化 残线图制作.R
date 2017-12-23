###回归可视化：列线图制作###
 ##########################   虚拟场景   ###########################
 n<-1000
 set.seed(88)
 age<-rnorm(n,65,11)
 lac<-round(abs(rnorm(n,3,1)),1)
 sex<-factor(sample(1:2,n,prob = c(0.6,0.4),TRUE),labels = c('male','female'))
 #sample函数
 #第一个参数指明factor的展示数据，即用1和2来表示各个变量；
 #第二个参数指明数据量，第三个参数指明各个factor的概率；
 shock<-factor(sample(1:4,n,prob = c(0.3,0.3,0.25,0.15),TRUE),
                  labels = c('no','mild','moderate','severe'))
 z<-0.2*age+3*lac*as.numeric(sex)+5*as.numeric(shock)-rnorm(n,36,15)
 y<-ifelse(runif(n)<=plogis(z),1,0) #plogis为logit反函数，根据线性函数计算出概率
 Y<-ifelse(y==0,0,sample(1:3,length(y),TRUE))
 data<-data.frame(age=age,lac=lac,sex=sex,shock=shock,y=y,Y=Y)
 
 var_labels=c(age='Age in Years',
              lac='lactate',
              sex='Sex of the participant',
              shock='shock',
              y='outcome',
              Y='ordinal')
 library(rms)
 label(data)=lapply(names(var_labels),
                    function(x) label(data[,x])=var_labels[x])
 #names()是为了获得对象的名
 #label为变量添加标签，相当于写备注
 head(data)
 
 ##########################   列线图绘制   ###########################
 library(rms)
 ddist<-datadist(data) #获取对象的变量分布特征，用于绘图、校正其它变量
 options(datadist='ddist') 
 #将获取的特征用于模型的建立
 #options是对全局变量都进行调整,可获得目标变量的分布特征，以便于后续作图
 mod_bi<-lrm(y~shock+lac*sex+age,data) #建立回归模型
 nom_bi<-nomogram(mod_bi,
                  lp.at = seq(-3,4,by=0.5),
                  fun = function(x) 1/(1+exp(-x)),
                  fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                  funlabel = 'Risk of Death',
                  conf.int = c(0.1,0.7),
                  abbrev = TRUE,
                  minlength = 1,
                  lp = F)
 # 第一行mod_bi就是刚才logistic回归的模型名称。Lp选择True或False，是否显示线性预测坐标（linear predictor）
 # 
 #lp.at则是设置其坐标轴的范围，此处就是从-3到4，每个刻度为0.5
 # 
 # fun是要自己设一个函数，对lp进行转换，并建立一个新坐标轴。此处就用logit变换的反函数，将lp转换为我们熟悉的风险概率
 # 
 # function(x) 1/(1+exp(-x))这一串，即使用function()构建一个自定义函数，括号中的x从lp的范围中取值，代入1/(1+exp(-x))中运算。
 # 
 # fun.at则是给新的坐标轴设置范围。本例中，lp的最小值是-3，对应的死亡风险是0.0474，所以起始值要大于这个值才能显示，不落在其范围内的都是不会显示的。
 # 
 # funlabel则是给上面转换好的新坐标轴起个名字，Risk of Death。其实有了这条坐标轴，上面lp那里也可以设为F，不显示了。但即使不显示，lp.at还是要设置，否则光设后面的fun.at似乎是无效的。
 # 
 # conf.int展示每个自变量坐标轴上各刻度的置信区间，这里显示两层置信区间，即0.1和0.7; 默认是F，即不显示。其实好多文章都不显示，你可以多学一招。
 plot(nom_bi,lplabel = 'Linear Predictor',
      fun.side = c(3,3,1,1,3,1,3,1,1,1,1,1,3),
      col.conf=c('blue','green'),
      conf.space=c(0.1,0.5),
      label.every = 3,
      col.grid = gray(c(0.8,0.95)),
      which = 'shock')
 # 用plot()作图。nom是刚才定义的所有参数对象的名字，lplabel是lp坐标轴的名称，如果刚才lp选了F，这行也可以不要了。
 # 
 # fun.side是设置新坐标轴“Risk of Death”的坐标刻度显示在哪一边，1表示下方，3为上方。不写则默认下方，但当空间比较窄，刻度密集时，可以把几个刻度放到上面。刚才fun.at设了几个刻度，这里的fun.side就要设几个值。数不清的话，可以把fun.at那一行单独选中运行一次，然后运行length(fun.at)查看。
 # 
 # label.every也是这个意思，刻度每3个显示1个，拒绝密恐。
 # 
 # col.conf设置置信区间的颜色。传说“红配绿赛狗屁”不是没有道理，忘了是Nat Commum.还是哪个杂志来着，建议作者们不要使用红绿配色，照顾色盲同学。。。
 # 
 # conf.space是设置刚才那些置信区间条在两条坐标轴之间的位置，两轴之间的距离为1，现在设成0.1~0.5之间，虽然设了看起来不精确，但不设会压成一团。
 # 
 # col.grid设置垂直参考线的颜色，从自变量轴指向point轴。一般设2个，表示主要和次要刻度。此处表示灰色的0.8透明度和0.95透明度，你也可以设成其他颜色，格式同上面的红配绿
 legend.nomabbrev(nom_bi,which = 'shock',x=.5,y=.5)
 #x与y代表legend位置的坐标值
 
 mod_ord<-lrm(Y~age+rcs(lac,4)*sex) #rcs为改变变量的阶数，从而产生不同的模型结果，第二个参数表示分类数量
 fun2<-function(x) plogis(x-mod_ord$coef[1]+mod_ord$coef[2])
 #表示两种可能结果（0&1 与 2&3）
 fun3<-function(x) plogis(x-mod_ord$coef[1]+mod_ord$coef[3])
 #表示（0&1&2 与 3）
 f<-Newlabels(mod_ord,c(age='Age(years)'))
 nom_ord<-nomogram(f,fun = list('Prob Y>=1'=plogis,
                                'Prob Y>=2'=fun2,
                                 'Prob Y=3'=fun3),
                                  lp=F,
                   fun.at = c(.01,.05,seq(.1,.9,by=.1),.95,.99))
 plot(nom_ord,lmgp=.2,cex.axis=.6)
 
 
 ##########################   生存资料的列线图 全参数模型  ###########################
 library(survival)
 library(rms)
 lung$sex<-factor(lung$sex,labels = c('male','female'))
 mod_sur<-psm(Surv(time,status)~ph.ecog+sex+age,lung,dist='weibull')
 #生存函数存在两个因变量，一个是时间time，一个是状态status；
 #设定其服从weibull分布
 #psm是参数生存模型
 med<-Quantile(mod_sur) #将线性预测转化为中位生存时间
 surv<-Survival(mod_sur) #将线性预测转化为某个时间点的生存概率
 
 ddist<-datadist(lung)
 options(datadist = 'ddist')
 
 #预测生存时间
 nom_sur1<-nomogram(mod_sur,
            fun = list(function(x)med(lp=x,q=0.5),
                       function(x)med(lp=x,q=0.25)),
            funlabel = c('Median Survival Time',
                         '1Q Survival Time'),
                       lp=F)
 #此时传递了两个函数，故用list进行传递；q=0.5表示中位生存时间，q=0.25表示四分之一即第一个quater生存时间
 
 plot(nom_sur1,
      fun.side = list(c(rep(1,7),3,1,3,1,3),rep(1,7)),
      col.grid = c('red','green'))
 
 #预测某个时间点生存概率
 nom_sur2<-nomogram(mod_sur,fun = list(function(x)surv(200,x),
                                       function(x)surv(400,x)),
                    funlabel = c('200-Day Survival Probability',
                                 '400-Day Survival Probability'),
                               lp=F)
 plot(nom_sur2,
      fun.side = list(c(rep(c(1,3),5),1,1,1,1),
                      c(1,1,1,rep(c(3,1),6))),
      xfrac=.7,
      col.grid = c('red','green'))
 
 ##########################   生存资料的列线图 半参数模型  ###########################
 library(survival)
 library(rms)
 ddist<-datadist(lung)
 options(datadist='ddist')
 mod_cox<-cph(Surv(time,status)~ph.ecog+sex+age,lung,surv = TRUE)
 #cph是Cox比例生存模型
 surv_cox<-Survival(mod_cox) #某个时间点的cox概率
 nom_cox<-nomogram(mod_cox,
                   fun = list(function(x)surv_cox(200,x),
                              function(x)surv_cox(400,x)),
                   funlabel = c('200-Day Sur.Prob.',
                                '400-Day Sur.Prob.'),
                   lp=F)
 plot(nom_cox,fun.side = list(c(rep(c(1,3),5),1,1,1,1),
                              c(1,1,1,rep(c(3,1),6))))
 