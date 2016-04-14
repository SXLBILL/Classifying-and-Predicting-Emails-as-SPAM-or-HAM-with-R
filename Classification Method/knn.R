knn_self = function(X_tr, X_test, Y_tr, methodname, k_nearst,...){
  # KNN model which contains spatial information
  
  # calculate the distance between the training data and the validation data
  dismatrix2=rbind(X_test,X_tr)
  method = methodname
  data.s=scale(dismatrix2, center = TRUE, scale = TRUE)   # scale the data
  dismatrix=dist(data.s, method = methodname,...) # dismatrix is N_whole*N_whole matrix
  dismatrix= as.matrix(dismatrix) 
  for(i in 1:nrow(dismatrix2)) dismatrix[i,i]=1000
  # dismatrixfinal is N_train*N_test matrix
  dismatrixfinal=dismatrix[c((nrow(test)+1):nrow(dismatrix2)), c(1:nrow(test))]  
  
  # order the nearest observarions of the training data
  nearest=apply(dismatrixfinal, 2, order)
  # get the predict results based on the training data
  predict = sapply(1:nrow(test), function(i) 
                  decision(dismatrixfinal, nearest, cl, k, i))
  list(predict = predict, nearest = nearest)
}

decision = function(dismatrixfinal, nearest, Y_train, k_nearst, testobs){
  # score the k nearest distance of the training data to the ith observation of validation data 
  nearobs = nearest[1:k_nearst, testobs]
  label = Y_train[nearobs]
  weight = dismatrixfinal[nearobs, testobs] / sum(dismatrixfinal[nearobs, testobs])
  score = (1/weight)/ sum(1/weight)
  chooselabel = sapply(0:9, function(i) sum(score[which(label == i)]))
  obslabel = which(chooselabel== max(chooselabel)) - 1
  obslabel
}

cv_knn = function(X_whole, Y_whole, methodname, k_nearst, K, ...){
  # given cost and kernel, get Cross Validation error then chose best cost and kernel
  Y_true_matrix = matrix(0, nrow(X_whole), 1)
  Y_pred_matrix = matrix(0, nrow(X_whole), 1)
  start = 0
  folds = cvFolds(nrow(X_whole), K)
  whole = array(folds$subsets)
  
  for(cvgroup in 1:K){
    test_idx = whole[which(folds$which==cvgroup)]
    train_idx = whole[which(folds$which!=cvgroup)]
    X_train = X_whole[train_idx, ]
    X_test = X_whole[test_idx, ]
    Y_test = as.matrix(Y_whole[test_idx])
    Y_train = as.matrix(Y_whole[train_idx])
    res_pred = knn_self(X_train, X_test, Y_train, methodname, k_nearst,...)
    start = 1 + start
    end = start + length(test_idx) - 1
    Y_pred_matrix[start:end] = res_pred$predict
    Y_true_matrix[start:end] = Y_test
    start = end
  }
  
  Y_pred_matrix = as.numeric(Y_pred_matrix)
  Y_true_matrix = as.numeric(Y_true_matrix)
  
  # Confusion Matrix
  Cf_Matrix = matrix(0, 10, 10)
  
  for(i in 1:nrow(X_whole)){
    Cf_Matrix[Y_true_matrix[i]+1, Y_pred_matrix[i]+1] = Cf_Matrix[Y_true_matrix[i]+1, Y_pred_matrix[i]+1] + 1
  }
  rownames(Cf_Matrix) = c(0:9)
  colnames(Cf_Matrix) = c(0:9)
  cvmse = 1 - sum(diag(Cf_Matrix))/nrow(X_whole)
  
  list(cvmse = cvmse, Cf_Matrix = Cf_Matrix, Y_pred_matrix = Y_pred_matrix, Y_true_matrix = Y_true_matrix)
}



res1 = cv_knn(X_train, Y_train, "euclidean", 1, 5)
res2 = cv_knn(X_train, Y_train, "euclidean", 5, 5)
res3 = cv_knn(X_train, Y_train, "euclidean", 50, 5)
cvmses = sapply(c(1:50), function(i) cv_knn(X_train, Y_train, "euclidean", i, 10)[[1]])

