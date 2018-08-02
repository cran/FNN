################################################################################
# Search k nearest neighbors                                                   #
# File:   KNN.R                                                                #
# Author: Shengqiao Li                                                         #
# Date:   December 12, 2008                                                    #
#         2017-12-27 remove DUP=FALSE from .C                                  #
#                                                                              #
################################################################################
get.knn<- function (data, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  algorithm<- match.arg(algorithm);  
  
  #check data
  if(!is.matrix(data)) data<- as.matrix(data);
  if(!is.numeric(data)) stop("Data non-numeric")  
  if(any(is.na(data))) stop("Data include NAs") 
  if(storage.mode(data)=="integer") storage.mode(data)<- "double";


  n <- nrow(data);
  d <- ncol(data);

  if(k>=n) warning("k should be less than sample size!");
  
  knnres<- switch(algorithm, 
              cover_tree = .C("get_KNN_cover", t(data), as.integer(k), d, n, nn.index = integer(n*k), nn.dist = double(n*k)),
              kd_tree= .C("get_KNN_kd", t(data), as.integer(k), d, n, nn.index = integer(n*k), nn.dist = double(n*k)),
              CR = .C("get_KNN_CR", t(data), as.integer(k), d, n, nn.index = integer(n*k), nn.dist = double(n*k)),
              brute = .C("get_KNN_brute", t(data), as.integer(k), d, n, nn.index = integer(n*k), nn.dist = double(n*k))
  ); 

                                           
  nn.index<-  matrix(knnres$nn.index, byrow=T, nrow=n, ncol=k);
  nn.dist<- matrix(knnres$nn.dist, byrow=T, nrow=n, ncol=k);
  
  if(k>=n){
      nn.index[, n:k]<- NA;
      nn.dist[, n:k]<- NA;
  }    
    
  return(list(nn.index=nn.index, nn.dist=nn.dist));    
}
get.knnx<- function (data, query, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  #k neearest neighbor Euclidean distances
  algorithm<- match.arg(algorithm);	  
  
  #check data
  if(!is.matrix(data)) data<- as.matrix(data);
  if(!is.numeric(data)) stop("Data non-numeric")
  if(any(is.na(data))) stop("Data include NAs") 
  if(storage.mode(data)=="integer") storage.mode(data)<- "double";


  #check query
  if(!is.matrix(query)) query<- as.matrix(query);
  if(!is.numeric(query)) stop("Data non-numeric")
  if(any(is.na(query))) stop("Data include NAs") 
  if(storage.mode(query)=="integer") storage.mode(query)<- "double";

  n <- nrow(data); m<- nrow(query);
  d <- ncol(data); p<- ncol(query);

  if(d!=p) stop("Number of columns must be same!.");
  if(k>n) warning("k should be less than sample size!");
  
  knnres<- switch(algorithm, 
                cover_tree = .C("get_KNNX_cover", t(data), t(query), as.integer(k), d, n, m, nn.index = integer(m*k), nn.dist = double(m*k)),
                kd_tree= .C("get_KNNX_kd", t(data), t(query), as.integer(k), d, n, m, nn.index = integer(m*k), nn.dist = double(m*k)),                 
                CR = .C("get_KNNX_CR", t(data), t(query), as.integer(k), d, n, m, nn.index = integer(m*k), nn.dist = double(m*k)),
                brute = .C("get_KNNX_brute", t(data), t(query), as.integer(k), d, n, m, nn.index = integer(m*k), nn.dist = double(m*k))
  ); 
                                            
  nn.index<- matrix(knnres$nn.index, byrow=T, nrow=m, ncol=k);
  nn.dist<-  matrix(knnres$nn.dist,  byrow=T, nrow=m, ncol=k);
#2012_10_15
#  if(k>=n) {
#    nn.index[, n:k]<- NA;
#    nn.dist[, n:k]<- NA;
#  }
   if (k > n) {
     nn.index[, (n+1):k] <- NA
     nn.dist[, (n+1):k] <- NA
  }
  return(list(nn.index=nn.index, nn.dist=nn.dist));
}
knn.index<- function (data, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  get.knn(data, k, algorithm )$nn.index;
}
knn.dist<- function (data, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  get.knn(data, k, algorithm )$nn.dist;
}
knnx.dist<- function (data, query, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  get.knnx(data, query, k, algorithm )$nn.dist  
}
knnx.index<- function (data, query, k = 10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
{
  get.knnx(data, query, k, algorithm )$nn.index;
}
