library(readxl)
library(randomForest)
library(dplyr)
library(yardstick)
x <- read.csv("C:/Users/mohammad/Desktop/new/X.csv")
y <- read.csv("C:/Users/mohammad/Desktop/new/Y.csv")
##removing first and secound col of x
x=x[,-c(1,2)]
### removing first col of y
y=y[,-1]
set.seed(123)
for (i in ncol(y)){
  main=cbind(x,y[,i])
  ind <- sample(2, nrow(main), replace = TRUE, prob = c(0.7, 0.3))
  train <- main[ind==1,]
  test <- main[ind==2,]
  #training  model
  rf <- randomForest(train, train[,226],importance=TRUE,na.action=TRUE,type="regression")
  #Confusion Matrix - traindata
  p1 <- predict(rf, train)
  train.rf_scored <- as_tibble(cbind(train, p1))
  RMSE_rf_Train <- yardstick::rmse(train.rf_scored, truth=train[,i], estimate=p1)
  
  #Confusion Matrix testdata
  p2 <- predict(rf, test)
  test.rf_scored <- as_tibble(cbind(test,p2))
  RMSE_rf_Test <- yardstick::rmse(test.rf_scored, truth=test[,i], estimate=p2)
  
}

#Plot the Random Forest Results
pdf("C:/Users/Mohammad/desktop/plot.pdf")
plot(rf)
dev.off()

# creating dataFrame to sort top VImps

imp <- as.data.frame(varImp(rf),scale = T)
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
var_imp <- imp[order(imp$overall,decreasing = T),]
var_imp <- filter(var_imp, overall < 5)


#ploting top 15 varible importance
pdf("C:/Users/Mohammad/desktop/Vipms.pdf")

top_n(var_imp, n=15, overall) %>%
  ggplot(., aes(x=names, y=overall))+
  geom_bar(stat="identity", fill="steelblue", width = 0.5) + ggtitle("Top 15 Variable Importance")+
  theme(text = element_text(size = 9))
  
dev.off()

  