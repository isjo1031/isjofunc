normalize <- function(x, class = FALSE, cl = ncol(x)){
  ds=x
  if(class == TRUE){
    ds = ds[, -cl]
  }
  nor = as.data.frame(lapply(ds, function(x){return((x- min(x))/(max(x) - min(x)))}))
  return(nor)
}
