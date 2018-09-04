#Prepare data

data <-Cardiotocographic
str(data)
data$NSPF <-factor(data$NSP)

#Partition data into training and validaiton error 
set.seed(1234)
pd <- sample(2,nrow(data) , replace = TRUE , prob =  c(0.8,0.2))
train <-data[pd==1,]
validate <-data[pd==2,]

# Decision tree with party 
install.packages("party")
library("party")
View(data)
tree <-ctree(NSPF~LB+AC+FM , data = train ,controls =ctree_control(mincriterion = 0.99 ,minsplit = 99))
                                                                
tree
plot(tree)

#Predict

predict(tree , validate , type="prob")
predict(tree ,validate)


#decision Tree with Rpart

install.packages("rpart.plot")
library("rpart.plot")

library(rattle)
library(rpart)
library(RColorBrewer)

tree1 <- rpart(NSPF~LB+AC+FM , train)
library(rpart.plot)
rpart.plot(tree1 ,extra =4)


predict(tree1,validate)

tab<-table(predict(tree), data$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

tab<-table(predict(tree, validate)
print(tab)
1-sum(diag(tab))/sum(tab)
