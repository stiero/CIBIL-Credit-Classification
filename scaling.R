num_preds <- c(NULL)
predictors = colnames(train)

for(p in predictors){
  if(class(train[,p]) == "numeric" | class(train[,p]) == "integer")
  {
    num_preds = c(num_preds, p)
  }
}


for(p in num_preds){
  train[,p] <- scale(train[,p])
}

#########################

num_preds <- c(NULL)
predictors = colnames(test)

for(p in predictors){
  if(class(test[,p]) == "numeric" | class(test[,p]) == "integer")
  {
    num_preds = c(num_preds, p)
  }
}


for(p in num_preds){
  test[,p] <- scale(test[,p])
}


print("Scaling complete")