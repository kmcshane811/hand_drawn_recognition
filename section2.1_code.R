library(dplyr) #https://www.rdocumentation.org/packages/dplyr/versions/0.7.8

library(stringr) #https://www.rdocumentation.org/packages/stringr/versions/1.4.0

library(GDINA) #https://www.rdocumentation.org/packages/GDINA/versions/2.8.8

library(matlab) #https://www.rdocumentation.org/packages/R.matlab/versions/3.6.2/topics/R.matlab-package

library(raster) ##https://www.rdocumentation.org/packages/raster/versions/3.4-5/topics/clump

library(igraph) #https://www.rdocumentation.org/packages/igraph/versions/0.3.1

library(functional) #https://www.rdocumentation.org/packages/functional/versions/0.6

#Section 2

#files
files <- list.files(path= './images', pattern=".csv", all.files=T, full.names=T)

#feature functions
find_label <- function(file){
  label <- strsplit(file, "_",)[[1]][2]
  return(label)
}

find_index <- function(file){
  index <- strsplit(file, "_")[[1]][3]
  index <- substr(index,1,2)
  return(index)
}

find_npix <- function(matrix){
  nr_pix <- matrix == 1
  nr_pix <- length(which(nr_pix == TRUE))
  return(nr_pix)
}

pix_col <- function(matrix,ncols){
  for(i in 1:ncols){
    row <- matrix[,i] == 1
    num <- length(which(row == TRUE))
    nr_pix_cols = c(nr_pix_cols,num)
  }
  return(nr_pix_cols)
}

pix_row <- function(matrix,nrows){
  for(i in 1:nrows){
    row <- matrix[i,] == 1
    num <- length(which(row == TRUE))
    nr_pix_rows = c(nr_pix_rows,num)
  }
  return(nr_pix_rows)
}

row_pix1 <- function(nr_pix_rows){
  nrow1 <- nr_pix_rows == 1
  nrow1 <- length(which(nrow1 == TRUE))
  return(nrow1)
}

col_pix1 <- function(nr_pix_cols){
  ncol1 <- nr_pix_cols == 1
  ncol1 <- length(which(ncol1 == TRUE))
  return(ncol1)
}

row_pix3 <- function(nr_pix_rows){
  nrow3 <- nr_pix_rows >= 3
  nrow3 <- length(which(nrow3 == TRUE))
  return(nrow3)
}

col_pix3 <- function(nr_pix_cols){
  ncol3 <- nr_pix_cols >= 3
  ncol3 <- length(which(ncol3 == TRUE))
  return(ncol3)
}

get_aspect_ratio <- function(matrix,ncols,nrows){
  for(y in 1:ncols){
    for(x in 1:nrows){
      if(matrix[x,y]==1){
        left <- y
        break
      }
    }
  }

  for(y in ncols:1){
    for(x in 1:nrows){
      if(matrix[x,y]==1){
        right <- y
        break
      }
    }
  }

  for(x in 1:nrows){
    for(y in 1:ncols){
      if(matrix[x,y]==1){
        tallest <- x
        break
      }
    }
  }

  for(x in nrows:1){
    for(y in 1:ncols){
      if(matrix[x,y]==1){
        lowest <- x
        break
      }
    }
  }

  height <- -(lowest - tallest)
  width <- -(right - left)

  if(height == 0){
    height = 1
  }

  if(width == 0){
    width = 1
  }
  aspect_ratio <- width/height
  return(aspect_ratio)
}

#https://stackoverflow.com/questions/29105175/find-neighbouring-elements-of-a-matrix-in-r

get_neighbours <- function(matrix){
  matrix.pad = rbind(NA, cbind(NA, matrix, NA), NA)
  ind = 2:nrow(matrix)+1 # row/column indices of the "middle"
  neigh = rbind(o  = as.vector(matrix.pad[ind , ind ]),
                N  = as.vector(matrix.pad[ind - 1, ind    ]),
                NE = as.vector(matrix.pad[ind - 1, ind + 1]),
                E  = as.vector(matrix.pad[ind    , ind + 1]),
                SE = as.vector(matrix.pad[ind + 1, ind + 1]),
                S  = as.vector(matrix.pad[ind + 1, ind    ]),
                SW = as.vector(matrix.pad[ind + 1, ind - 1]),
                W  = as.vector(matrix.pad[ind    , ind - 1]),
                NW = as.vector(matrix.pad[ind - 1, ind - 1]))

  neigh[is.na(neigh)] <- 0
  return(neigh)
}

#https://www.rdocumentation.org/packages/raster/versions/3.4-5/topics/clump
find_areas <- function(matrix){
  rast <- raster(matrix)
  clumps <- clump(rast,directions = 8,gaps = FALSE)
  values <- freq(clumps)[,1]
  connected_areas <- values[length(values)-1]
  return(connected_areas)
}

find_eyes <- function(matrix){
  inver <- matrix
  inver[inver == 1] <- -1
  inver[inver == 0] <- 1
  inver[inver == -1] <- 0
  rast <- raster(inver)
  clumps <- clump(rast,directions = 4,gaps = FALSE)
  values <- freq(clumps)[,1]
  eyes <- values[length(values)-1]-1
  return(eyes)
}

find_neigh1 <- function(neigh,neigh_col){
  nr_neigh <- c()
  for(i in 1:ncol(neigh)){
    row <- neigh[,i] == 1
    num <- length(which(row == TRUE))
    nr_neigh = c(nr_neigh,num)
  }
  neigh_1 <- length(which(nr_neigh == 1))
  return(neigh_1)
}

#initialise feature arrays

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
pixel_ratio <- c()
#Loop for all features

for(i in files){
    nr_pix_rows <- c()
    nr_pix_cols <- c()
    csv <- read.csv(file = i, header = TRUE)
    matrix <- as.matrix(csv)
    matrix <- rbind(NA, cbind(NA, matrix, NA), NA)
    matrix[is.na(matrix)] <- 0
    nrows <- nrow(matrix)
    ncols <- ncol(matrix)

    #Create Label & Index

    label <- append(label,find_label(i))


    index <- c(index,find_index(i))


    #Calculate total number of pixels
    num_pix <- find_npix(matrix)
    nr_pix <- c(nr_pix,num_pix)


    #Calculate number of pixels in a row & column

    nr_pix_rows <- pix_row(matrix,nrows)

    nr_pix_cols <- pix_col(matrix,ncols)


    #rows & cols with 1 pixel

    nrow1 <- c(nrow1,row_pix1(nr_pix_rows))

    ncol1 <- c(ncol1,col_pix1(nr_pix_cols))

    #rows & cols with >= 3 pixels

    nrow3 <- c(nrow3,row_pix3(nr_pix_rows))

    ncol3 <- c(ncol3,col_pix3(nr_pix_cols))

    #aspect ratio

    aspect_ratio <- c(aspect_ratio,get_aspect_ratio(matrix,ncols,nrows))

    #Neighbours

    neigh <- get_neighbours(matrix)

    neigh_col <- ncol(neigh)

    neigh_1 <- c(neigh_1,find_neigh1(neigh,neigh_col))

    no_neigh_above <- c(no_neigh_above,length(which(neigh[2,] == 0 & neigh[1,] == 1 & neigh[3,] == 0 & neigh[9,] == 0 ) == TRUE))
    no_neigh_below <- c(no_neigh_below,length(which(neigh[6,] == 0 & neigh[1,] == 1 & neigh[5,] == 0 & neigh[7,] == 0) == TRUE))
    no_neigh_left <- c(no_neigh_left,length(which(neigh[8,] == 0 & neigh[1,] == 1 & neigh[7,] == 0 & neigh[9,] == 0) == TRUE))
    no_neigh_right <- c(no_neigh_right,length(which(neigh[4,] == 0 & neigh[1,] == 1 & neigh[3,] == 0 & neigh[5,] == 0) == TRUE))
    no_neigh_horiz <- c(no_neigh_horiz,length(which(neigh[6,] == 0 & neigh[4,] == 0 & neigh[1,] == 1)==TRUE))
    no_neigh_vert <-  c(no_neigh_vert,length(which(neigh[2,]== 0 & neigh[6,] == 0 & neigh[1,] == 1)==TRUE))

    #Connected Areas

    connected_areas <- c(connected_areas,find_areas(matrix))

    #Eyes

    eyes <- c(eyes,find_eyes(matrix))

    #pixel ratio
    ratio <- num_pix/(ncols * nrows)
    pixel_ratio <- c(pixel_ratio,ratio)
}


 #initialise df
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
                  no_neigh_horiz = no_neigh_horis,
                  no_neigh_vert = no_neigh_vert,
                  connected_areas = connected_areas,
                  eyes = eyes,
                  pixel_ratio = pixel_ratio)
head(df)
write.csv(df,'./40288049_features.csv',row.names = FALSE)
