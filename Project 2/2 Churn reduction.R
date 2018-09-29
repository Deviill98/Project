rm(list=ls())

setwd("D:/DATA SCIENCE/R")



##Loading libraries

D = c("DMwR" , "randomForest" , "ggbiplot" , "dplyr" , "caret" , "inTrees" , "corrgram" , 
      "corrplot" , "C50" , "class" , "e1071" , "usdm" , "xgboost")


lapply(D , require , character.only = TRUE)



# Reading the data

A_train  = read.csv("Train_data.csv" , header = T)

B_test = read.csv("Test_data.csv" , header = T)



# Merging the data for easier pre processing 

X = rbind(A_train , B_test)


View(X)

str(X)

X$phone.number = NULL
X$area.code = as.factor(X$area.code)



# Label encoding for categorical variables

for(i in 1:ncol(X)){

  if(class(X[,i]) == 'factor'){

    X[,i] = factor(X[,i] ,labels = (1:length(levels(factor(X[,i])))))
  }
}



sum(is.na(X))

Z = sapply(X , is.numeric) 
Z_1 = X[,Z]

cnames = colnames(Z_1)



# Boxplot


for (i in 1:length(cnames)){
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(X))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" , outlier.shape=18,
                        outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot",cnames[i])))
} 

gridExtra::grid.arrange(gn1 , gn2, gn3 ,  ncol = 3)
gridExtra::grid.arrange(gn4 , gn5, gn6 ,  ncol = 3)
gridExtra::grid.arrange(gn7 , gn8, gn9 , ncol = 3)
gridExtra::grid.arrange(gn10 , gn11, gn12 ,  ncol = 3)
gridExtra::grid.arrange(gn13 , gn14, gn15 ,  ncol = 3)



# Outlier removal

for(i in cnames){
  val = X[,i][X[,i] %in% boxplot.stats(X[,i])$out]
  X[,i][X[,i] %in% val] = NA
}

sum(is.na(X))

X = knnImputation(X , k=5) 
sum(is.na(X))


# Corrgram

corrgram(Z_1 , order = F , upper.panel = panel.pie , text.panel = panel.txt , 
         main = "Correlation plot")


Z_2 = Z_1

colnames(Z_2) = c("acclen" , "novmail" , "daym" , "dayca" , "daych" , "evem" , "eveca" , "evech" , 
                  "nigm" , "nigca" , "nigch" , "intm" , "intca" , "intch" , "custserca" )


Y = cor(Z_2)


# Corrplot

corrplot(X2 ,   method="number" )

symnum(cor(Z_2))


factor_index = sapply(X , is.factor)

factor_data = X[,factor_index]


# Chi - square test

for (i in 1:4){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn , factor_data[,i])))
}



# VIF test

vif(Z_1[,1:15])

vifcor(Z_1[,1:15], th = 0.9)

X2 = subset(X , select = -c(total.day.charge , total.eve.charge ,total.night.minutes,
                            total.intl.charge))

View(X2)

str(X2)


# Selecting the same rows of the original data for train and test and feeding the model

train = X2[1:nrow(A_train),]
test  = X2[-(1:nrow(A_train)),]




## Decision Tree Classification

DT = C5.0(Churn ~ . , train , trials = 100 , rules = TRUE)

summary(DT)


DT_pred = predict(DT , test[,-16] , type = "class")

# For output of predicted data

DT_pred  


DTCOnf = table(test$Churn , DT_pred)
confusionMatrix(DTCOnf)


ACC = 93.7

FNR = FN / FN+TP = 45.5





## Random Forest Classification

RF = randomForest(Churn ~ . , train , importance = TRUE , ntree = 500)

RF_Pred = predict(RF , test[,-16])


# For output of predicted data
RF_Pred


RFCOnf = table(test$Churn , RF_Pred)
confusionMatrix(RFCOnf)


Acc = 92

FNR = FN / FN+TP = 51.3 




## Naive Bayes 

NB = naiveBayes(Churn ~ . , data=train)

NBPred = predict(NB , test[,1:15] , type = 'class' )


# For output of predicted data
NBPred


NBConf = table(observed = test[,16] , predicted = NBPred)
confusionMatrix(NBConf)

mean(NBPred == test$Churn)


ACC = 88

FNR = FN / FN+TP = 77.6




## Logistic Regression 

LR = glm(Churn ~ . , data = train , family = "binomial")

summary(LR)

LR_Pred = predict(LR , newdata = test , type = "response")

LR_Pred = if_else(LR_Pred > 0.5 , 1 , 0)

# For output of predicted data

LR_Pred


LRConf = table(test$Churn , LR_Pred)

Acc = 87.3

FNR = 80.8





## KNN Classification

KNN_Pred = knn(train[,1:16] , test[,1:16] , train$Churn , k = 9)


# For output of predicted data

KNN_Pred

KNNConf = table(KNN_Pred , test$Churn)

sum(diag(KNNConf))/nrow(test)
 
ACC = 89

FNR = 19.4



## Output of predicted data can  be found under the respective models
### End of code
