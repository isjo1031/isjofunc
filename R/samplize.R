samplize <- function(x, is_valid = FALSE, p = c(4, 1)){
  set.seed(72170235)
  ds        <- x
  nds <- nrow(ds)
  partition <- c("train", "test")
  idx       <- c()
  rtn       <- c()
  test <- data.frame(); train <- data.frame(); valid <- data.frame();
  if(is_valid == FALSE){
    idx     <- sample(x = partition, size = nds, replace = TRUE, prob = p)
    test    <- ds[idx == "test", ]
    train   <- ds[idx == "train", ]
    rtn     <- list(train=train, test=test)
    return(rtn)
  }else{
    partition = c(partition, "valid")
    idx     <- sample(x = partition, size = nds, replace = TRUE, prob = p)
    test    <- ds[idx == "test", ]
    train   <- ds[idx == "train", ]
    valid   <- ds[idx == "valid", ]
    rtn     <- list(train=train, test=test, valid = valid)
    return(rtn)
  }
}
