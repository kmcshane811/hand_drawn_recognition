library(ggplot2) #https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5

library(psych) #https://cran.r-project.org/web/packages/psych/psych.pdf

library(dplyr) #https://www.rdocumentation.org/packages/dplyr/versions/0.7.8




hist <- function(data,title,inc){
  out <- ggplot(as.data.frame(data), aes(x = data)) +
    geom_histogram(breaks=seq(min(data),
                              max(data), by=inc)) +
    ggtitle(title)
  return(out)
}

scatter <- function(x,y,title,x_title,y_title){
  plot(x, y, main = title,
       xlab = x_title, ylab = y_title,
       pch = 19, frame = FALSE)
}
df <- read.csv('./40288049_features.csv')

features <- c("nr_pix","rows_with_1","cols_with_1","rows_with_3p","cols_with_3p","aspect_ratio","neigh_1","no_neigh_above","no_neigh_below","no_neigh_left","no_neigh_right","no_neigh_horiz","no_neigh_vert","connected_areas","eyes","pixel_ratio")
labels <- unique(df$label)

letters <- df[df$label %in% c('a','b','c','d','e','f','g','h','i','j'),]
non_letters <- df[df$label %in% c('smiley','sad','xclaim'),]

skew_ <- c()

for(i in 3:18){
  skew_<-c(skew_,skew(df[,i]))
}

skews <- data.frame(Feature = features,
                    Skews = skew_)

summary(df)
summary(letters)
summary(non_letters)

#Section 3.1

nr_pix_hist <- ggplot(as.data.frame(df$nr_pix), aes(x = df$nr_pix)) +
  geom_histogram(breaks=seq(min(df$nr_pix),
                            max(df$nr_pix), by=3)) +
  ggtitle("Number of Pixels")

nr_pix_hist
skew(df$nr_pix)

rows_1_hist <- ggplot(as.data.frame(df$rows_with_1), aes(x = df$rows_with_1)) +
  geom_histogram(breaks=seq(min(df$rows_with_1),
                            max(df$rows_with_1), by=0.9)) +
  ggtitle("Rows with 1 pixel")

rows_1_hist
skew(df$rows_with_1)

cols_1_hist <- ggplot(as.data.frame(df$cols_with_1), aes(x = df$cols_with_1)) +
  geom_histogram(breaks=seq(min(df$cols_with_1),
                            max(df$cols_with_1), by=0.9)) +
  ggtitle("Columns with 1 pixel")

cols_1_hist
skew(df$cols_with_1)

rows_3_hist <- ggplot(as.data.frame(df$rows_with_3p), aes(x = df$rows_with_3p)) +
  geom_histogram(breaks=seq(min(df$rows_with_3p),
                            max(df$rows_with_3p), by=1)) +
  ggtitle("Rows with 3 or more pixels")

rows_3_hist
skew(df$rows_with_3p)

cols_3_hist <- ggplot(as.data.frame(df$cols_with_3p), aes(x = df$cols_with_3p)) +
  geom_histogram(breaks=seq(min(df$cols_with_3p),
                            max(df$cols_with_3p), by=0.9)) +
  ggtitle("Columns with 3 or more Pixels")

cols_3_hist
skew(df$cols_with_3p)

aspect_ratio_hist <- ggplot(as.data.frame(df$aspect_ratio), aes(x = df$aspect_ratio)) +
  geom_histogram(breaks=seq(min(df$aspect_ratio),
                            max(df$aspect_ratio), by=0.09)) +
  ggtitle("Aspect Ratio")

aspect_ratio_hist
skew(df$aspect_ratio)


#Section 3.2

letter_means <- as.data.frame(colMeans(letters[,3:18]))
non_letter_means <- as.data.frame(colMeans(non_letters[,3:18]))

letter_medians <- as.data.frame(lapply(letters[,3:18],median))
non_letter_medians <- as.data.frame(lapply(non_letters[,3:18],median))

letter_sd <- as.data.frame(lapply(letters[,3:18],sd))
non_letter_sd <- as.data.frame(lapply(non_letters[,3:18],sd))

#feature boxplots
par(mfrow=c(1,2))
boxplot(letters$rows_with_1,main = "Plot of rows_with_1 in letters",
        xlab = "Rows with 1 pixel")
boxplot(non_letters$rows_with_1,main = "Plot of rows_with_1 in non_letters",
        xlab = "Rows with 1 pixel")

par(mfrow=c(1,2))
boxplot(letters$no_neigh_horiz,main = "Plot of no_neigh_horiz in letters",
        xlab = "Pixels with no horizontal neighbor")
boxplot(non_letters$no_neigh_horiz,main = "Plot of no_neigh_horiz in non_letters",
        xlab = "Pixels with no horizontal neighbor")

par(mfrow=c(1,2))
boxplot(letters$connected_areas,main = "Plot of connected_area in letters",
        xlab = "Connected Areas")
boxplot(non_letters$connected_areas,main = "Plot of connected_area in non_letters",
        xlab = "Connected Areas")

#Section 3.3

#randomization tests - code adjusted from sample Lecture code '111_randomization_test_example.R'
nsims = 100000
pvalues <- c()
for(i in 3:18){
  letter_pop <- letters[,i]
  non_letter_pop <- non_letters[,i]

  obs_diff <- mean(letter_pop) - mean(non_letter_pop)

  all <- c(letter_pop,non_letter_pop)

  nulldiffs <- rep(0,nsims)

  for (ni in 1:nsims) {
    randomA = sample(all)
    randomX = randomA[1:length(letter_pop)]
    randomY = randomA[(length(letter_pop)+1):length(all)]
    nulldiffs[ni] <- mean(randomX) - mean(randomY)
  }

  pvalue = sum(abs(nulldiffs)>abs(obs_diff))/nsims
  pvalues <- c(pvalues,pvalue)

  quantile(nulldiffs, 0.025)
  quantile(nulldiffs, 0.975)


  rand_df <- as.data.frame(nulldiffs)
  rand_df$group <- "randomization"
  colnames(rand_df) <- c("test.statistic", "group")
  rand_df$group <- as.factor(rand_df$group)

}
pvals <- data.frame(Feature = features,
                    P = pvalues) %>% filter((P>=0 & P<=0.025) | (P<=1 & P>=0.975))
pvals

ggplot(df,aes(x = rows_with_3p))+
  geom_histogram(data = letters,aes(fill ="letters"), alpha = 2) +
  geom_histogram(data = non_letters,aes(fill ="non_letters"), alpha = 2)

ggplot(df,aes(x = cols_with_3p))+
  geom_histogram(data = letters,aes(fill ="letters"), alpha = 2) +
  geom_histogram(data = non_letters,aes(fill ="non_letters"), alpha = 2)

ggplot(df,aes(x = no_neigh_left))+
  geom_histogram(data = letters,aes(fill ="letters"), alpha = 2) +
  geom_histogram(data = non_letters,aes(fill ="non_letters"), alpha = 2)

ggplot(df,aes(x = no_neigh_right))+
  geom_histogram(data = letters,aes(fill ="letters"), alpha = 2) +
  geom_histogram(data = non_letters,aes(fill ="non_letters"), alpha = 2)


#Section 3.4

nsims = 10000
pval_matrix <- matrix(0, nrow = 16, ncol = 16)
corr_matrix <- matrix(0, nrow = 16, ncol = 16)

for(i in 3:18){
  x_data <- df[,i]
  for(j in 3:18){
    nulldiffs <- rep(0,nsims)
    if(i<j){
      y_data <- df[,j]
      for (ni in 1:nsims) {
        test <- corr.test(data.frame(x_data),data.frame(sample(y_data)))
        nulldiffs[ni] <- test$r
      }

      test_r <- corr.test(x_data,y_data)
      r <- test_r$r
      r_mean <- mean(nulldiffs)
      r_sd <- sd(nulldiffs)
      p <- pnorm(r,mean = r_mean, sd = r_sd)
      corr_matrix[i-2,j-2] <- r
      pval_matrix[i-2,j-2] <- p
    }
  }
}

mat_pos <- which((pval_matrix <= 0.025 & pval_matrix!=0) | (pval_matrix>=0.975 & round(pval_matrix, digits = 5)!=1),
                 arr.ind = TRUE)

x <- c()
y <- c()
cor <- c()
p <- c()

for(i in 1:nrow(mat_pos)){
  x_ind <- mat_pos[i,1]
  y_ind <- mat_pos[i,2]

  if(x_ind!=y_ind | pval_matrix[x_ind,y_ind]!=1){
    x_val <- features[x_ind]
    y_val <- features[y_ind]
    x <- c(x,x_val)
    y <- c(y,y_val)
    cor <- c(cor,corr_matrix[x_ind,y_ind])
    p <- c(p,pval_matrix[x_ind,y_ind])
  }
}

corr_df <- data.frame(X = x,
                      Y = y,
                      R = cor,
                      P = p)
corr_df <- corr_df[order(corr_df$X,)]

print(corr_df)
par(mfrow=c(3,2))
for(i in 1:6){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}

par(mfrow=c(3,2))
for(i in 7:12){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}

par(mfrow=c(3,2))
for(i in 13:18){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}

par(mfrow=c(3,2))
for(i in 19:24){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}

par(mfrow=c(3,2))
for(i in 25:30){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}

par(mfrow=c(3,2))
for(i in 31:35){
  x <- mat_pos[i,1] + 2
  y <- mat_pos[i,2] + 2
  plot(df[,x],df[,y],main = paste(features[x-2],"vs",features[y-2]),xlab = features[x-2], ylab = features[y-2] )
}
