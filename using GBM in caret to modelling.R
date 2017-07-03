library(caret)
library(mlbench)
library(e1071)
data(Sonar)

set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

fitControl <- trainControl(## 10-fold CVmethod = "repeatedcv",
                        number = 10,
                        ## repeated ten times
                        repeats = 10)

set.seed(825)
gbmFit1 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

##修改超参数
##========================================

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

## 性能评估可视化
## 通过?plot.train, ?ggplot.train等 查看可选择的图表类型，可调参数等等
trellis.par.set(caretTheme())
plot(gbmFit2)  

plot(gbmFit2, metric = "Kappa")

plot(gbmFit2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))

ggplot(gbmFit2, metric = 'Kappa')  + theme_bw() + geom_point(alpha = 0.2)

## 输出预测概率：
## For predict.train, the type options are standardized to be "class" and "prob" 
predict(gbmFit3, newdata = head(testing))
predict(gbmFit3, newdata = head(testing), type = "prob")

