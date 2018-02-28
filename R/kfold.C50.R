kfold.C50 <- function (ds,cl, fold=10) {
  library(caret)   # for createFolds
  library(C50)          # for SVM
  set.seed(100)
  k.fold <-createFolds(as.vector(cl),k=fold)
  cl = factor(as.integer(cl))
  acc = c()              # classification result
  for (i in 1:fold) {
    this.fold = k.fold[[i]]
    train.ds = ds[-c(this.fold),]
    train.cl = cl[-c(this.fold)]
    test.ds  = ds[c(this.fold),]
    test.cl  = cl[c(this.fold)]
    model = C5.0(train.ds,train.cl)
    result = predict(model, test.ds)
    acc[i] = mean(result==test.cl)
  }

  return(mean(acc))  # average accuracy of k=fold test

}
