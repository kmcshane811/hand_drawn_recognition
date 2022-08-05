library(MASS) #https://www.rdocumentation.org/packages/MASS/versions/7.3-47

#Section 1


#https://stackoverflow.com/questions/66482754/convert-pgm-to-matrix

files <- list.files(path= './pgm', pattern=".pgm", all.files=T, full.names=T)
for(i in files){
    pgm <- readLines(i)
    pgm <- pgm[-(1:4)] # remove first 4 elements
    pgm <- as.numeric(pgm) # convert from character to numeric vector
    pgm <- matrix(pgm, 18, 18, byrow=TRUE)

    is.matrix(pgm) # TRUE
    dim(pgm) # [1] 18 18
    pgm[pgm<128] <- 1
    pgm[pgm>=128] <- 0

    name <- substr(i,6,nchar(i)-4)
    name <- paste("./images/",name,sep="")
    name <- paste(name,"csv",sep=".")
    write.table(pgm,sep = ",",file = name,col.names = F,row.names = F)
}

