library(jtools) #https://www.rdocumentation.org/packages/jtools/versions/2.1.4

library(olsrr) #https://www.rdocumentation.org/packages/olsrr/versions/0.5.3

library(ggplot2) #https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5

library(caret) #https://www.rdocumentation.org/packages/caret/versions/6.0-91

library(MLeval) #https://www.rdocumentation.org/packages/MLeval/versions/0.3

library(class) #https://cran.r-project.org/web/packages/class/class.pdf

library(dplyr) #https://www.rdocumentation.org/packages/dplyr/versions/0.7.8

library(stringr) #https://cran.r-project.org/web/packages/stringr/index.html

library(randomForest) #https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

data <- read.csv('.all_features.csv', header = FALSE)
head(data)

label <- c()
index <- c()
nr_pix <- c()
nrow1 <- c()
ncol1 <- c()
nrow3 <- c()
ncol3 <- c()
aspect_ratio <- c()
neigh_1 <- c()
no_neigh_above <- c()
no_neigh_left <- c()
no_neigh_below <- c()
no_neigh_right <- c()
no_neigh_horiz <- c()
no_neigh_vert <- c()
connected_areas <- c()
eyes <- c()
diagonaless <- c()

for(i in 1:nrow(data)){
  str <- strsplit(toString(data[i,1]),"\t")
  label <- c(label,str[[1]][1])
  index <- c(index, str[[1]][2])
  nr_pix <- c(nr_pix, str[[1]][3])
  nrow1 <- c(nrow1,str[[1]][4])
  ncol1 <- c(ncol1,str[[1]][5])
  nrow3 <- c(nrow3,str[[1]][6])
  ncol3 <- c(ncol3,str[[1]][7])
  aspect_ratio <- c(aspect_ratio,str[[1]][8])
  neigh_1 <- c(neigh_1,str[[1]][9])
  no_neigh_above <- c(no_neigh_above,str[[1]][10])
  no_neigh_left <- c(no_neigh_left,str[[1]][11])
  no_neigh_below <- c(no_neigh_below,str[[1]][12])
  no_neigh_right <- c(no_neigh_right,str[[1]][13])
  no_neigh_horiz <- c(no_neigh_horiz,str[[1]][14])
  no_neigh_vert <- c(no_neigh_vert,str[[1]][15])
  connected_areas <- c(connected_areas,str[[1]][16])
  eyes <- c(eyes,str[[1]][17])
  diagonaless <- c(diagonaless,str[[1]][18])
}

df <- data.frame(label = label,
                 index = index,
                 nr_pix = nr_pix,
                 rows_with_1 = nrow1,
                 cols_with_1 = ncol1,
                 rows_with_3p = nrow3,
                 cols_with_3p = ncol3,
                 aspect_ratio = aspect_ratio,
                 neigh_1 = neigh_1,
                 no_neigh_above = no_neigh_above,
                 no_neigh_below = no_neigh_below,
                 no_neigh_left = no_neigh_left,
                 no_neigh_right = no_neigh_right,
                 no_neigh_horiz = no_neigh_horiz,
                 no_neigh_vert = no_neigh_vert,
                 connected_areas = connected_areas,
                 eyes = eyes,
                 diagonaless = diagonaless)

df = df[,!(colnames(df) %in% "index")]

#Section 3.1 

Nt <- seq(25,375,50)
Np <- c(2,4,6,8)

opNt <- 0
opNp <- 0
maxAcc <- 0
accuracies <- c()

control <- trainControl(method = 'repeatedcv', number = 5, search = 'grid')
tune_grid <- expand.grid(.mtry = Np)
results <- data.frame(matrix(ncol = 5, nrow = 7))

for(i in 1:length(Nt)){
  rf_model <- train(label~.,data = df,method = 'rf',metric = 'Accuracy', tuneGrid = tune_grid,ntree = Nt[i])
  
  accuracy <- max(rf_model$results$Accuracy)
  
  if(accuracy >= maxAcc){
    maxAcc <- accuracy
    opNt <- Nt[i]
    opNp <- rf_model$bestTune$mtry
  }
  results[i,] <- c(Nt[i],rf_model$results$Accuracy)
}

maxAcc
opNt
opNp

colnames(results) <- c("Nt","Np-2","Np-4","Np-6","Np-8")
results

#Section 3.2

control <- trainControl(method = 'repeatedcv', number = 5, search = 'grid')
tune_grid <- expand.grid(.mtry = opNp)
model_acc <- c()

for(i in 1:15){
  rf_model <- train(label~.,data = df,method = 'rf',metric = 'Accuracy', tuneGrid = tune_grid,ntree = opNt)
  
  accuracy <- max(rf_model$results$Accuracy)
  model_acc <- c(model_acc,accuracy)
}

model_acc

hist(model_acc,main = "Histogram of Accuracies for a given RF",col = "red",xlab = "Accuracy")

t <- (0.5 - mean(model_acc))/(sd(model_acc)/sqrt(length(model_acc)))
pt(t,14)

t.test(model_acc, mu = (1/13))
#Graph comparing accuracies 
plot(results$Nt,results$`Np-8`,type="o", col="red",pch = "o",lty = 1, ylab = "Accuracy Rate",xlab = "Number of trees", ylim = c(0.3,0.7),main = "Plot of Accuracies Against Number of Trees in a RF")
points(results$Nt, results$`Np-6`, col = "green", pch = "*")
lines(results$Nt, results$`Np-6`, col = "green", lty = 2)
points(results$Nt, results$`Np-4`, col = "blue", pch = 4)
lines(results$Nt, results$`Np-4`, col = "blue", lty = 2)
points(results$Nt, results$`Np-2`, col = "purple", pch = 0)
lines(results$Nt, results$`Np-2`, col = "purple", lty = 2)
legend(x = "bottom",inset = 0,
       legend = c("Np-2","Np-4","Np-6","Np-8"), 
       col=c("purple","blue","green","red"), lwd=5, cex=.5, horiz = TRUE)