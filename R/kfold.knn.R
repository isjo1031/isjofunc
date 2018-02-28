kfold.knn <- function (ds,cl, fold=10, k.value=5) {
  library(caret)   # for createFolds
  library(class)   # for knn
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)

  acc = c()              # classification result
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    result = knn(train.ds, test.ds, train.cl, k=k.value)
    acc[i] = mean(result==test.cl)

  }

  return(mean(acc))  # average accuracy of k=fold test

}
