###热图及层级聚类分析###
 ##########################  层级聚类分析（HCA）简介   ###########################
 # 通过求取各元素之间的距离，选取元素距离最短的归为同一类
 # 若遇到{x1,x2}与x3，x4判断距离，则两丛集之间的距离为丛集内元素之间距离最长者
 #eg
 
 df<-matrix(c(1,2,4,3,2,1,7,9),nrow = 4)
 rownames(df)<-c('x1','x2','x3','x4')
 colnames(df)<-c('a','b')
 df
 
 dist(df)
 #最终得到的丛集分类如下：{x1,x2}，{x3,x4}
 plot(hclust(dist(df)))
 #如图所示，x1和x2为一类，x3和x4为一类
 
 ##########################  数据构建   ###########################
 nvar=5
 data<-data.frame(diag=factor(rep(c("Sepsis","AECOPD","Surgery","MODS","Poisoning"),50)))
 for (i in 1:nvar) {
   data[[paste("x",i,sep="")]]<-rnorm(250)
 }
 attach(data)
 data$y<-3*x1+2*x2-2*x3+x3^2-x4+x5^3-2*x5
 detach()
 #attach可以使数据框中的所有变量直接调用，如调用x1直接键入变量名即可，无需使用data&x1
 #detach()则发挥相反的作用
 #diag是诊断，x代表不同的检验结果，y表示outcome
 
 #########################  以diag作为分组依据，求y与各x之间的回归系数   ##########################
 library(lme4)
 coeff<-lapply(data[,2:6],function(x) {
   coef(lmList(y~x|diag,data=data.frame(x=x,y=data$y,diag=data$diag)))[2]
 })
 coeff
 #lapply()返回一个长度与X一致的列表，每个元素为FUN计算出的结果，且分别对应到X中的每个元素
 #lmList是对数据中的各亚组进行线性拟合
 #coef函数返回截距和回归系数，回归系数在list的第二个位置，故需要标注[2]
 
 coefficient<-t(as.data.frame(coeff))
 varlist<-names(data[,2:6])
 row.names(coefficient)<-varlist 
 coefficient
 #t函数求矩阵或data frame的转置，as.data.frame(coeff)将list转为data frame
 #names取变量名构成list
 
 ##########################  绘制热图   ###########################
 library(gplots)
 heatmap.2(coefficient,
           key = TRUE,
           keysize = 1.5,
           ColSideColors=rainbow(ncol(coefficient)),
           RowSideColors=rainbow(nrow(coefficient)),
           srtCol=30)
 #每一个方块表示相关系数，颜色不同表示了其数值的不同,legend有相应的展示
 
 ##########################  热图内添加散点图即拟合曲线   ###########################
 #思路
 #1.用lattice建立散点图；2.每个散点图的背景色与回顾系数相关；3.绘制树图（dendrogram）
 library(lattice)
 library(reshape2)
 m.data<-melt(data,id.vars = c('diag','y'))
 head(m.data)
 #id.vars是被当做维度的列变量，每个变量在结果中占一列；
 #measure.vars是被当成观测值的列变量，它们的列变量名称和值分别组成variable和value两列
 #列变量名称用variable.name和value.name来指定
 #melt函数将原来的数据框进行转化，保留原有的其中两个变量，将其它变量和观测值均列入variable和value
 #详情可看浏览器中的收藏
 
 dd.row <- as.dendrogram(hclust(dist(coefficient)))
 row.ord <- order.dendrogram(dd.row)
 dd.col <- as.dendrogram(hclust(dist(t(coefficient))))
 col.ord <- order.dendrogram(dd.col)
 par(mfrow=c(2,1))
 plot(dd.row)
 plot(dd.col)
 #dist默认对表格中每一行的变量进行计算，如需对列变量计算dist，则需要先进行行-列转置
 #order.dendrogram返回树图中每个变量的位置序号
 
 coeff.order<-coefficient[row.ord,col.ord]
 #按照树图中的顺序排列数据
 scale.coef<-as.vector(round((coeff.order-min(coefficient))*10+1))
 #以coefficient中最小值为参照，每一个数相对于这个最小值进行变换，从而使热图中的颜色渐变
 #取整数便于从调色板调取颜色
 palette(rainbow(round((max(coefficient)-min(coefficient))*10)+1,start=0,end=0.7))
 #定义调色板
 #rainbow的第一个参数为调色板中的颜色数量
 library(latticeExtra)
 plot<-xyplot(y~value|variable+diag,     #variable和diag是分组变量
              data=m.data,              
              par.strip.text = list(cex = 0.6), #方框边上的条是strip，cex是其中字符的大小，小于1缩小，大于1增大，默认为1
              key=list(space="left",    
                       lines=list(col=seq(1,round((max(coefficient)-min(coefficient))*10)+1,4),lwd=4,size=1),
                         text=list(as.character(round((seq(1,round((max(coefficient)-min(coefficient))*10)+1,4)-1)/10+min(coefficient),1)))
                ),
                legend =
                  list(right =
                         list(fun = dendrogramGrob,
                              args =
                                list(x = dd.col, ord = col.ord,
                                     side = "right",
                                     size = 10)),
                       top =
                         list(fun = dendrogramGrob,
                              args =
                                list(x = dd.row,
                                     side = "top",
                                     type = "triangle"))),
                mycolors =scale.coef,
                panel = function(x, y,col,mycolors) {
                  panel.fill(col=mycolors[panel.number()])
                  panel.xyplot(x, y,cex=0.2,col="black")
                  panel.loess(x, y, col="black",lwd=2)
                },
                index.cond=list(row.ord,col.ord),
                xlab="x value"
 )
 useOuterStrips(plot)
 #key表示左边的图例，lines是确定表示线段的参数，其为1至最大最小值之差的一串数，相邻数之间相差4
 #legend根据参数定义了其所用函数为dendrogram，从而能画出树图
 #mycolors确定填充颜色时所用的数值
 #panel实现对格子颜色进行填充
 #panel.fill的目的是填充颜色；
 #panel.xyplot的目的是绘制散点图
 #panel.loess是绘制散点的趋势线
 #index.cond是变量的排序
 #xlab是横轴的标题
 #useOuterStrips的目的是将所有的strip移动到box外围
 