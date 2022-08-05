library(jtools) #https://www.rdocumentation.org/packages/jtools/versions/2.1.4

library(olsrr) #https://www.rdocumentation.org/packages/olsrr/versions/0.5.3

library(ggplot2) #https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5

library(caret) #https://www.rdocumentation.org/packages/caret/versions/6.0-91

library(MLeval) #https://www.rdocumentation.org/packages/MLeval/versions/0.3

library(class) #https://cran.r-project.org/web/packages/class/class.pdf

set.seed(42)

df <- read.csv('./40288049_features.csv')

data <- data.frame(df[which(df$label %in% c('a','j','sad','smiley','xclaim')),])

data$class[data$label %in% c('a','j')] <- 'Letter'

data$class[data$label %in% c('smiley')] <- 'Smiley_Face'

data$class[data$label %in% c('sad')] <- 'Sad_Face'

data$class[data$label %in% c('xclaim')] <- 'Xclaim'

#Section 2.1 - Chosen features: 1.) Rows with 1 2.)No neighbor vertical 3.) Connected Areas 4.) Eyes

k <- seq(1,13,2)

inv_k <- 1/k

x <- cbind(data$rows_with_1,data$no_neigh_vert,data$connected_areas,data$eyes)[1:nrow(data),]
colnames(x) <- c('rows_with_1','no_neigh_vert','connected_areas','eyes')

y <- cbind(data$class)[1:nrow(data),]
accuracies <- c()



for(i in k){

  knn.pred=knn(x,x,y,k=i)
  
  tab <- table(knn.pred,y)
  print(tab) 
  
  accuracy <- mean(knn.pred == y)
  
  accuracies <- c(accuracies, accuracy)
}

accuracies

#Section 2.2

train_control <- trainControl(method="cv", number=5, 
                              savePredictions=TRUE, 
                              classProbs = TRUE)

og_model <- train(class~ rows_with_1 + no_neigh_vert + connected_areas + eyes, data=data, 
               trControl=train_control, method="knn", tuneGrid = data.frame(k=k))

og_model

opK <- 0
accuracies_cf <- c()
max_acc <- 0

for(i in k){
  model <- train(class ~ rows_with_1 + no_neigh_vert + connected_areas + eyes, data=data, 
        trControl=train_control, method="knn", tuneGrid = data.frame(k=i))
  
  pred <- predict(model,data)
  tab <- table(pred,data$class)
  accuracy <- mean(pred == y)
  
  accuracies_cf <- c(accuracies_cf,accuracy)
  
  if(accuracy >= max_acc){
    max_acc = accuracy
    opK = i
  }
  
}


plot(inv_k, accuracies_cf, type="o", col="red",pch = "o",lty = 1, ylab = "Accuracy Rate")
points(inv_k, og_model$results$Accuracy, col = "green", pch = "*")
lines(inv_k, og_model$results$Accuracy, col = "green", lty = 2)
legend(x = "bottom",inset = 0,
       legend = c("Prediction",'Model Accuracies'), 
       col=c("red","green"), lwd=5, cex=.5, horiz = TRUE)

#Section 2.3

op_model <- model <- train(class~ rows_with_1 + no_neigh_vert + connected_areas + eyes, data=data, 
                           trControl=train_control, method="knn", tuneGrid = data.frame(k=opK))

pred <- predict(model,data)
tab <- table(pred,data$class)

tab

accuracy <- mean(pred == y)


#Section 2.4

error_rate <- 1- accuracies

error_rate_cf <- 1- accuracies_cf
accuracies_cf
plot(inv_k, accuracies, type="o", col="red",pch = "o",lty = 1, ylab = "Accuracy Rate")
points(inv_k, accuracies_cf, col = "green", pch = "*")
lines(inv_k, accuracies_cf, col = "green", lty = 2)
legend(x = "bottom",inset = 0,
       legend = c("Non-Cross-Validated",'Cross-Validated'), 
       col=c("red","green"), lwd=5, cex=.5, horiz = TRUE)