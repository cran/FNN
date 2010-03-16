################################################################################
# KNN Classification and Regression                                            #
# File:   KNNCR.R                                                              #
# Author: Shengqiao Li                                                         #
# Date:   December 12, 2008                                                    #
#                                                                              #
# for small dataset, nrow(train)<50 and nrow(test)<50,                         #
# ANN is slower than VR's method.                                              #
# for large dataset, ANN is faster                                             #
################################################################################

knn<- function(train, test, cl, k=1, l=0, 
              prob=FALSE, use.all=TRUE, 
              algorithm=c("cover_tree", "kd_tree", "VR")
              )
{
  algorithm<- match.arg(algorithm);
  
  train <- as.matrix(train)
  if (is.null(dim(test))) 
      dim(test) <- c(1, length(test))
  test <- as.matrix(test)
  if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
      stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if (length(cl) != ntr) 
      stop("'train' and 'class' have different lengths")
  if (ntr < k) {
      warning(gettextf("k = %d exceeds number %d of patterns", k, ntr), domain = NA)
      k <- ntr
  }
  if (k < 1)
      stop(gettextf("k = %d must be at least 1", k), domain = NA)
  nte <- nrow(test)
  if (ncol(test) != p) 
      stop("dims of 'test' and 'train' differ")
  clf <- as.factor(cl)
  nc <- max(unclass(clf))
  switch(algorithm,
         cover_tree = ,
         kd_tree ={Z<- get.knnx(data=train, query=test, k=k, algorithm);	         
                      nn.class<- matrix(cl[Z$nn.index], ncol=k); #factor levels are taken.      
                      pred_prob<- function(x) 
                      {
                        freq<- table(x)
                        prob<- freq/k;
                        max.ind<- which.max(freq)         
                        list(names(max.ind), prob[max.ind])
                      }      
                      res<- apply(nn.class, 1, pred_prob);      
                      res<- matrix(unlist(res), ncol=2, byrow=TRUE);
                      if(prob) pr<- as.numeric(res[,2]);
                      res<- as.factor(res[,1]);
         },
         VR = {Z <- .C("VR_knnc", as.integer(k), as.integer(l), as.integer(ntr),
                 as.integer(nte), as.integer(p), as.double(train), 
                  as.integer(unclass(clf)), as.double(test), res = integer(nte),
                  pr = double(nte), integer(nc+1), as.integer(nc), as.integer(FALSE),
                  as.integer(use.all), nn.index = integer(nte*k), nn.dist = double(nte*k)
                );
                res <- factor(Z$res, levels=seq_along(levels(clf)),labels=levels(clf));
                if(prob) pr<- Z$pr;
        }
      )
      
  if(prob) attr(res, "prob") <- pr;
  attr(res, "nn.index")<- matrix(Z$nn.index, ncol=k);;
  attr(res, "nn.dist")<- matrix(sqrt(Z$nn.dist), ncol=k);
 
  return(res);
}

knn.cv <- function(train, cl, k=1, l=0,
          prob=FALSE, use.all=TRUE,
          algorithm=c("cover_tree", "kd_tree", "VR")
          )
{
  algorithm<- match.arg(algorithm);
  
  train <- as.matrix(train)
  if(any(is.na(train)) || any(is.na(cl)))
     stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
  if(ntr-1 < k) {
    warning(gettextf("k = %d exceeds number %d of patterns", k, ntr-1), domain = NA)
      k <- ntr - 1
    }
  if (k < 1)
            stop(gettextf("k = %d must be at least 1", k), domain = NA)
  clf <- as.factor(cl)
  nc <- max(unclass(clf))

  switch(algorithm,
     cover_tree =,
     kd_tree  = {Z<- get.knnx(data=train, query=train, k=k);	         
                  nn.class<- matrix(cl[Z$nn.index], ncol=k); #factor levels are taken.      
                  pred_prob<- function(x) 
                  {
                    freq<- table(x)
                    prob<- freq/k;
                    max.ind<- which.max(freq)         
                    list(names(max.ind), prob[max.ind])
                  }
  
                  res<- apply(nn.class, 1, pred_prob);      
                  res<- matrix(unlist(res), ncol=2, byrow=TRUE);
                  if(prob) pr<- as.numeric(res[,2]);
                  res<- as.factor(res[,1]);
     },
     VR = {Z <- .C("VR_knnc", as.integer(k), as.integer(l),
                  as.integer(ntr), as.integer(ntr), as.integer(p),
                  as.double(train), as.integer(unclass(clf)), as.double(train),
                  res = integer(ntr), pr = double(ntr), integer(nc+1),
                  as.integer(nc), as.integer(TRUE), as.integer(use.all),
                  nn.index = integer(ntr*k), nn.dist = double(ntr*k)
                  );
              res <- factor(Z$res, levels=seq_along(levels(clf)),labels=levels(clf));
                  if(prob) pr<- Z$pr;
     }
  );
      
  if(prob) attr(res, "prob") <- pr;
  attr(res, "nn.index")<- matrix(Z$nn.index, ncol=k);;
  attr(res, "nn.dist")<- matrix(sqrt(Z$nn.dist), ncol=k);

  return(res);
}

knn.reg<- function(train, test=NULL, y, k=3, use.all=FALSE,
          algorithm=c("cover_tree", "kd_tree", "VR")
          )
{
  #KNN regression. If test is not supplied, LOOCV is performed and R2 is the predicted R2
  algorithm<- match.arg(algorithm); 
  train<- as.matrix(train);
   
  if(is.null(test)) {
    notest<- TRUE;
    test<- train;
  } else notest<-FALSE;

  if (is.null(dim(test))) 
      dim(test) <- c(1, length(test))
  if(is.vector(train)) {train<- cbind(train); test<- cbind(test)} #univariate
  else if(is.vector(test)) test<- rbind(test);

  ntr<- nrow(train); p<- ncol(train);
  n<- ifelse(is.null(dim(test)), length(test), nrow(test)); #number of samples to be predict
 
  pred<- switch(algorithm,
            cover_tree =, 
            kd_tree = {Z<- get.knnx(train, test, k, algorithm);
                      rowMeans(matrix(y[Z$nn.index], ncol=k));
            },
            VR = .C("VR_knnr",
                    as.integer(k), as.integer(ntr),	as.integer(n),
                    as.integer(p), as.double(train), as.double(y),
                    as.double(test), res = double(n),	as.integer(is.null(test)),
                    as.integer(use.all), nn.index = integer(n*k), nn.dist = double(n*k)
            )$res            
  )

  residuals<- if(notest) y-pred else NULL;
  PRESS<- if(notest) sum(residuals^2) else NULL;
  R2<- if(notest){ 1-PRESS/sum((y-mean(y))^2) } else NULL;

  res<- list(call=match.call(), k=k, n=n, y=y, pred=pred, residuals=residuals, PRESS= round(PRESS), R_square=R2);
  class(res)<- "KNNReg";
  
  return(res);
}

knn.reg.VR2<- function(train, test=NULL, y, k=3, use.all=FALSE)
{
  #using VR get.knnx

  if(is.null(test)) {
    notest<- TRUE;
    test<- train;
  } else notest<-FALSE;

  if(is.vector(train)) {train<- cbind(train); test<- cbind(test)} #univariate
  else if(is.vector(test)) test<- rbind(test);

  ntr<- nrow(train); p<- ncol(train);
  n<- ifelse(is.null(dim(test)), length(test), nrow(test)); #number of samples to be predict

  Z <- get.knnx(train, test, k, algorithm="VR");

  pred<- rowMeans(matrix(y[Z$nn.index], ncol=k));

  residuals<- if(notest) pred-y else NULL;

  R2<- if(notest){ 1-sum(residuals^2)/sum((y-mean(y))^2) } else NULL;

  list(call=match.call(), k=k, n=n, y=y, pred=pred, residuals=residuals, R2=R2);
}

