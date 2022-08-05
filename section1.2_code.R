library(jtools) #https://www.rdocumentation.org/packages/jtools/versions/2.1.4

library(olsrr) #https://www.rdocumentation.org/packages/olsrr/versions/0.5.3

library(ggplot2) #https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5

library(caret) #https://www.rdocumentation.org/packages/caret/versions/6.0-91

library(MLeval) #https://www.rdocumentation.org/packages/MLeval/versions/0.3

set.seed(42)
df <- read.csv('./40288049_features.csv')
df$isLetter <- as.integer(df$label %in% c('a','b','c','d','e','f','g','h','i','j'))
#df$isLetter[df$isLetter==1] <- 'yes'
#df$isLetter[df$isLetter==0] <- 'no'
head(df)

#section 1.1

shuffled <- df[sample(nrow(df)),]
training <- shuffled[1:(floor(0.8*nrow(shuffled))),]
test <- shuffled[(floor(0.8*nrow(shuffled))+1):nrow(shuffled),]

glmfit <- glm(isLetter ~ aspect_ratio + nr_pix,
              data = training, family = 'binomial')

summary(glmfit)

newdata <- data.frame(aspect_ratio = test$aspect_ratio,
                         nr_pix = test$nr_pix)

predictions <- predict(glmfit,newdata,type = "response")

print(predictions)

predictions[predictions>=0.5] <- 'Letter'
predictions[predictions<0.5] <- 'Non_Letter'

print(predictions)

obs <- data.frame(acc = test$isLetter,
                  pred = predictions)

obs$acc[obs$acc == 1] <- 'Letter'
obs$acc[obs$acc == 0] <- 'Non_Letter'

nn <- length(which(obs$acc == 'Non_Letter' & obs$pred == 'Non_Letter'))

ny <- length(which(obs$acc == 'Non_Letter' & obs$pred == 'Letter'))

yn <- length(which(obs$acc == 'Letter' & obs$pred == 'Non_Letter'))

yy <- length(which(obs$acc == 'Letter' & obs$pred == 'Letter'))

val <- c(nn,ny,yn,yy)

cm <- matrix(val,nrow = 2, ncol = 2)

colnames(cm) <- c('acc_Non_Letter' , 'acc_Letter')
rownames(cm) <- c('pred_Non_Letter' , 'pred_Letter')

cm

accuracy <- (cm[2,2] + cm[1,1])/(cm[1,2]+cm[2,2]+cm[1,1]+cm[2,1])

accuracy

precision <- cm[2,2]/(cm[2,2]+cm[2,1])

precision

recall <- cm[2,2]/(cm[2,2] + cm[1,2])

recall

f1 <- 2*(precision*recall)/(precision + recall)

f1

view <- data.frame(accuracy = accuracy,
                   precision = precision,
                   recall = recall,
                   f1 = f1)
#Section 1.2

df$isLetter[df$isLetter==1] <- 'Letter'
df$isLetter[df$isLetter==0] <- 'Non_Letter'
kfoldsk = 5
train_control <- trainControl(method="repeatedcv", number=kfoldsk, 
                              savePredictions=TRUE, 
                              classProbs = TRUE)

model <- train(isLetter~ nr_pix + aspect_ratio, data=df, 
               trControl=train_control, method="glm", family="binomial")

cm = confusionMatrix(table(model$pred$Letter >= 0.5,
                           model$pred$obs == "Letter")) 

tab <- cm$table

tab

accuracy <- (tab[2,2] + tab[1,1])/(tab[1,2]+tab[2,2]+tab[1,1]+tab[2,1])

accuracy

precision <- tab[2,2]/(tab[2,2]+tab[2,1])

precision

recall <- tab[2,2]/(tab[2,2] + tab[1,2])

recall

f1 <- 2*(precision*recall)/(precision + recall)

f1

row <- c(accuracy,precision,recall,f1)

view[2,] <- row

rownames(view)<- c('Non_CV','CV')
view
#Section 1.3

res <- evalm(model)

res$roc
