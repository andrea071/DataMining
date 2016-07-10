#install.packages(pkgs='arules')
library(arules)

data<-list()
#1、读数据
x<-read.transactions("~/Desktop/Analysis.csv",format="basket",sep = "")
#转换数据
# for (n in 1:length(x)) {data[n]<-strsplit(x[n],",")}
inspect(x)
summary(x)
trans<-as(x,"transactions")

#关联规则挖掘
frequentsets<- eclat(trans,parameter=list(support=0.01,maxlen=10,minlen=2))

#频繁项集
inspect(frequentsets)
#所有频繁项集

#规则
rules <- apriori(trans,parameter=list(support=0.01,confidence=0.4,minlen=2))
#按支持度查看前6条规则
inspect(sort(rules,by="support")[1:6])
#按置信度查看前6条规则
inspect(sort(rules,by="confidence")[1:6])
summary(rules)
#所有规则
inspect(rules)
sub.rules2=subset(rules, subset = rhs %pin% "2" &lift > 10)

#画出频繁项集
itemFrequencyPlot(trans,support=0.05,cex.names=0.8)
summary(rules)

#根据lift排序
sorted_lift<-sort(rules,by='lift')
inspect(sorted_lift)

#删除冗余规则
subset.matrix<-is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)
rules.pruned<-rules[!redundant]
inspect(rules.pruned)

#可视化
#install.packages(pkgs="arulesViz")
library(arulesViz)
plot(rules)
plot(rules,method="graph",control=list(type="items"))
plot(rules,method="paracoord",control=list(reorder=TRUE))

#保存结果
df.rules=as(rules,"data.frame")
write.csv(df.rules,file = "~/Desktop/result/result.csv",row.names = F, quote = F)