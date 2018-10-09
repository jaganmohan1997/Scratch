setwd("i:/CAP_Profile/Desktop/Scratch/KNN")

#Reading the Input Dataset
df <- read.csv('Iris.csv')

#setting the seed to generate same randomeness in the future
set.seed(10)

#splitting into test and train dataset
nrows <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), nrows)
trndf <- df[train_ind,]
tstdf <- df[-train_ind,]

#Euclidean distance function
top.euclidean.distance.for.each.row <- function(testdatapoint, trainingdataset, k){
  
  distance <- data.frame(index = 1:(nrow(trainingdataset)), actual.row = 3, d= 0)
  
  for (i in 1:nrow(trainingdataset)) {
    distance[i,1] <- i
    distance[i,2] <- as.integer(rownames(trainingdataset[i,]))
    distance[i,3] <- 0
    for (j in 1:(ncol(testdatapoint)-1)) {
      distance[i,3] = distance[i,3] + (testdatapoint[j] - trainingdataset[i,j])^2
    }
    distance[i,3] <- sqrt(distance[i,3])
  }
  
  sorted_distance <- distance[order(distance$d),]
  
  #distance is a list of row numbers of top k observations
  return(sorted_distance[1:k,])
}
#x <- top.euclidean.distance.for.each.row(tstdf[1,],trndf,5)

#Nearest neighbours function
finding.nearest.neighbour <- function(euclidian.output, trainingdataset.classification.coloumn, training.dataset){
  classes <- as.data.frame(unique(trainingdataset.classification.coloumn))
  count <- nrow(classes)
  count.class <- data.frame(matrix(0, nrow = 1, ncol = count))
  for (i in 1:nrow(euclidian.output)) {
    for (j in 1:count) {
      if (training.dataset[euclidian.output[i,1],5] == classes[j,]) {
        count.class[1,j] = count.class[1,j] + 1
        break
      }
    }
  }
  flag = 1
  for (i in 2:count) {
    if (count.class[1,flag] < count.class[1,i]) {
      flag = i
    }
  }
  
  return(classes[flag,])
}

#z <- finding.nearest.neighbour(x,trndf$Class,trndf)

knn.for.datapoint <- function(testingdatapoint, trainingdataframe,k,trainingdataset.classification.coloumn){
  euclidian.output <- top.euclidean.distance.for.each.row(testingdatapoint, trainingdataframe, k)
  return(finding.nearest.neighbour(euclidian.output, trainingdataset.classification.coloumn, trainingdataframe))
}

#zz <- knn.for.datapoint(tstdf[36,], trndf, 5 , trndf$Class)


knn <- function(testingdataframe, trainingdataframe, k, trainingdataset.classification.coloumn){
  
  output <- data.frame(matrix(NA, nrow = nrow(testingdataframe), ncol = 2))
  for (i in 1:nrow(testingdataframe)) {
    output[i,1] <- i
    output[i,2] <- knn.for.datapoint(testingdataframe[i,], trainingdataframe,k,trainingdataset.classification.coloumn)
  }
  return(output)
}
final <- knn(tstdf, trndf, 3, trndf$Class)

#now coming to the performance
final$class <- NA
final$class[final$X2 == 1] <- 'Iris-setosa'
final$class[final$X2 == 2] <- 'Iris-versicolor'
final$class[final$X2 == 3] <- 'Iris-virginica'


table(tstdf$Class,final$class)













