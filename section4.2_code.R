library(jtools) #https://www.rdocumentation.org/packages/jtools/versions/2.1.4

library(olsrr) #https://www.rdocumentation.org/packages/olsrr/versions/0.5.3

library(ggplot2) #https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5



df <- read.csv('./40288049_features.csv')
df$isLetter <- as.integer(df$label %in% c('a','b','c','d','e','f','g','h','i','j'))
head(df)


#Section 4.1

#https://www.rdocumentation.org/packages/olsrr/versions/0.5.3

model <- lm(aspect_ratio ~ nr_pix+rows_with_1+cols_with_1+rows_with_3p+cols_with_3p+neigh_1+no_neigh_above+no_neigh_below+no_neigh_left+no_neigh_right+no_neigh_vert+connected_areas+eyes+pixel_ratio,df)
r1 <- summ(model)
k <- ols_step_both_p(model)
Formula <- as.formula(paste("aspect_ratio ~", paste(as.vector(k$predictors),collapse = " + ")))
model <- lm(Formula,df)
sum <- summ(model)
summ(model)


#Section 4.2

#most discriminatory value - no_neigh_vert p = 0.00000
plot(df$no_neigh_vert,df$isLetter)

shuffled <- df[sample(nrow(df)),]
training <- shuffled[1:(floor(0.8*nrow(shuffled))),]
test <- shuffled[(floor(0.8*nrow(shuffled))+1):nrow(shuffled),]


glmfit <- glm(isLetter ~ no_neigh_vert,
              data = training, family = 'binomial')
summary(glmfit)
newdata <- as.data.frame(test$no_neigh_vert)
colnames(newdata)<- "no_neigh_vert"
predictions <- predict(glmfit,newdata,type = "response")

x.range <- range(training$no_neigh_vert)
x.vals <- seq(x.range[1],x.range[2],length.out=1000)

fitted.curve <- data.frame(no_neigh_vert = x.vals)
fitted.curve$isLetter <- predict(glmfit,fitted.curve,type = "response")

plt <-ggplot(training, aes(x=no_neigh_vert, y=isLetter)) +
  geom_point(aes(colour = factor(isLetter)),
             show.legend = T)+
  geom_line(data=fitted.curve, colour="orange", size=1)

plt


#Section 4.3

splits <- matrix(c(1:9),nrow = 3, ncol = 3)
features <- c(3,8,9)

letters <- df[df$label %in% c('a','b','c','d','e','f','g','h','i','j'),]
faces <- df[df$label %in% c('smiley','sad'),]
xclaim <- df[df$label %in% c('xclaim'),]
normal_cons <- c()
medians <- lapply(df[,features],median)

for(i in 1:3){
  letters[,features[i]][letters[,features[i]]<medians[i]] <- 0
  letters[,features[i]][letters[,features[i]] >= medians[i]] <- 1

  faces[,features[i]][faces[,features[i]]<medians[i]] <- 0
  faces[,features[i]][faces[,features[i]] >= medians[i]] <- 1

  xclaim[,features[i]][xclaim[,features[i]]<medians[i]] <- 0
  xclaim[,features[i]][xclaim[,features[i]] >= medians[i]] <- 1

  letter_count <- table(letters[,features[i]])
  face_count <- table(faces[,features[i]])
  xclaim_count <- table(xclaim[,features[i]])

  if(is.na(letter_count[2])){
    letter_count <- c(0,0)
  }

  if(is.na(face_count[2])){
    face_count <- c(0,0)
  }

  if(is.na(xclaim_count[2])){
    xclaim_count <- c(0,0)
  }
  normal_cons <- c(normal_cons,(letter_count[2]+face_count[2]+xclaim_count[2])/nrow(df))
  splits[1,i] <- (letter_count[2]) / (nrow(letters))
  splits[2,i] <- (face_count[2]) / (nrow(faces))
  splits[3,i] <- (xclaim_count[2]) / (nrow(xclaim) )

}
colnames(splits) <- c("Split1","Split2","Split3")
rownames(splits) <- c("Letters","Faces","Exclamation Marks")
splits <- as.data.frame(splits)
df$split1 <- c(letters[,3],faces[,3],xclaim[,3])
df$split2 <- c(letters[,8],faces[,8],xclaim[,8])
df$split3 <- c(letters[,9],faces[,9],xclaim[,9])

print(splits)

letter_splits <- splits[1,]
face_splits <- splits[2,]
xclaim_splits <- splits[3,]


#section 4.4

df$class[df$label %in% c('a','b','c','d','e','f','g','h','i','j')] <- 'Letter'
df$class[df$label %in% c('smiley','sad')] <- 'Face'
df$class [df$label %in% c('xclaim')]<- 'Xclaim'

shuffled <- df[sample(nrow(df)),]
training <- shuffled[1:(floor(0.8*nrow(shuffled))),]
test <- shuffled[(floor(0.8*nrow(shuffled))+1):nrow(shuffled),]



q <- table(training$class)
total <- nrow(training)

prior_face <- q[1]/total
prior_letter <- q[2]/total
prior_xclaim <- q[3]/total


letters_p1 <- (prior_letter * prod(letter_splits))/ prod(normal_cons)

faces_p1 <- (prior_face * prod(face_splits)) / prod(normal_cons)

xclaim_p1 <- (prior_xclaim * prod(xclaim_splits)) / prod(normal_cons)

letters_p0 <- (prior_letter * prod((1-letter_splits)))/prod(normal_cons)

faces_p0 <- (prior_face * prod((1-face_splits))) / prod(normal_cons)

xclaim_p0 <- (prior_xclaim * prod((1-xclaim_splits))) / prod(normal_cons)

p1 <- c(letters_p1,faces_p1,xclaim_p1)
p0 <- c(letters_p0,faces_p0,xclaim_p0)

nb <- data.frame(splits1 = p1,
                 splits0 = p0)

print(nb)
