rm(list=ls())

setwd("D:/DATA SCIENCE/R")



# Loading libraries

l = c("xlsx" , "DMwR" , "ggplot2" , "scales" , "gplots" , "psych" , "corrgram" , "corrplot" ,
      "dplyr" , "rpart" , "randomForest" , "inTrees" , "caret")

lapply(l , require , character.only = TRUE)



#Reading the data

X = read.xlsx("Absenteeism_at_work_Project.xls" , sheetIndex = 1 , header = T)

#view(X)

d = colnames(X)

#View(d)


# Converting the variables to proper datatypes :


X$Reason.for.absence = as.factor(X$Reason.for.absence)
X$Day.of.the.week = as.factor(X$Day.of.the.week)
X$Seasons = as.factor(X$Seasons)
X$Disciplinary.failure = as.factor(X$Disciplinary.failure)
X$Education = as.factor(X$Education)
X$Social.smoker = as.factor(X$Social.smoker)
X$Social.drinker = as.factor(X$Social.drinker)
X$Month.of.absence = as.factor(X$Month.of.absence)


#View(X)

sum(is.na(X))



# Missing Value Analysis

Y = data.frame(apply(X,2,function(x){sum(is.na(x))}))

Y$columns = row.names(Y)
row.names(Y) = NULL

names(Y)[1] = "Miss_Percent"

Y$Miss_Percent = (Y$Miss_Percent/nrow(X)) * 100

Y = Y[order(-Y$Miss_Percent),] 

Y = Y[,c(2,1)]

#View(Y)

# Impute the missing values

# X$Body.mass.index[is.na(X$Body.mass.index)] = mean(X$Body.mass.index , na.rm = T )

# X$Body.mass.index[is.na(X$Body.mass.index)] = median(X$Body.mass.index ,na.rm = T)

X = knnImputation(X , k = 5)

sum(is.na(X))

# X[121,20] = 24
# X[121,20] = 26.68 - Mean
# X[121,20] = 25 - Median
# X[121,20] = 24 -knn



qplot(x = X$Reason.for.absence, y = X$Absenteeism.time.in.hours, data= X, 
      color= "pink", xlab= "Reason for absence",
      ylab= "Absenteeism time in hours", main= "Reason for absence")



ggplot(X, aes(Reason.for.absence, fill = Reason.for.absence ) ) +
  geom_bar() 


ggplot(X, aes(Age, fill = Age ) ) +
  geom_bar()+ xlab("Age") + scale_x_continuous(breaks = pretty_breaks())



# Outlier Analysis

Z = sapply(X , is.numeric)

Z_1 = X[,Z]


#View(Z_1)

cnames = colnames(Z_1)

#View(cnames)



for (i in 1:length(cnames)){
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(X))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" , outlier.shape=18,
                          outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot",cnames[i])))
}  
  

gridExtra::grid.arrange(gn1 , gn2,  ncol = 2)
gridExtra::grid.arrange(gn3  , gn4 , gn5 , ncol = 3)
gridExtra::grid.arrange(gn6 , gn7 , gn8 , ncol = 3)
gridExtra::grid.arrange(gn9 , gn10 , gn11 , ncol = 3)
gridExtra::grid.arrange(gn12)


for(i in cnames){
  val = X[,i][X[,i] %in% boxplot.stats(X[,i])$out]
  X[,i][X[,i] %in% val] = NA
}


sum(is.na(X))

X = knnImputation(X , k=3)  

sum(is.na(X))


# Feature Selection

corrgram(Z_1 , order = F , upper.panel = panel.pie , text.panel = panel.txt , 
         main = "Correlation plot")

Z_2 = Z_1

colnames(Z_2) = c("ID" , "Trans" , "Distance" , "Service" , "Age" , "Work" , "Hit" , 
                  "Son" , "Pet" , "Weight" , "Height" , "BMI" , "Absent")

X2 = cor(Z_2)


corrplot(X2 ,   method="number" )

symnum(cor(Z_1))


# Model preparation and evaluation

df = X 


train_index = sample(1:nrow(df) , 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]




# Linear Regression

library(usdm)

lm_model = lm(Absenteeism.time.in.hours ~ .  , data = train)


summary(lm_model)


predictions_LR = predict(lm_model, test[,1:20])

regr.eval(test[,21] , predictions_LR , stats = c("mse" , "rmse" , "mae" , "mape"))





# Decision Tree Regression


fit = rpart(Absenteeism.time.in.hours ~ . , data = train , method = "anova")

predictions_DT = predict(fit , test[,-21])

regr.eval(test[,21] , predictions_DT , stats = c("mse" , "rmse" , "mae" , "mape"))





# RF Regression


RF_mod = randomForest(Absenteeism.time.in.hours ~ . , train)


rf = randomForest(Absenteeism.time.in.hours ~ . , train , mtry=5, ntree=500 , importance = TRUE) 
  
importance(rf)
  
pred = predict(rf,test[,-21]) 
  

regr.eval(test[,21] , pred , stats = c("mse" , "rmse" , "mae" , "mape"))




# End of code ..................................





