# Classification tree
load("E:\\U course\\STA 141\\Assignment\\141 HW5\\trainVariables.rda")
load("D:\\testData.rda")
load("D:\\blindTestData.rda")
test1=testVariables
trv1=trainVariables
trv=rbind(test1,trv1)
bli=blindTestVariables
test=bli

trv.new=data.frame(trv[,-30],factor(trv[,30]))
names(trv.new)[30]="isSpam"
library(rpart)
library(rpart.plot)
ctree=rpart(isSpam~.,trv.new, method="class")
rpart.plot(ctree)
predictree=predict(ctree, test, type="class")
save(predictree, file="Ctreepredict.rda")
