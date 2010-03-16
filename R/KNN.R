################################################################################
# Search k nearest neighbors                                                   #
# File:   FNN.R                                                                #
# Author: Shengqiao Li                                                         #
# Date:   December 12, 2008                                                    #
#                                                                              #
################################################################################
get.knn<- function (data, k = 10, algorithm=c("cover_tree", "kd_tree", "VR")) 
{
  algorithm<- match.arg(algorithm);   
  if(storage.mode(data)=="integer") storage.mode(data)<- "double";
  if(!is.matrix(data)) data<- as.matrix(data);

  n <- nrow(data);
  d <- ncol(data);

  if(k>=n) warning("k should be less than sample size!");
  Cname<- switch(algorithm, 
              cover_tree = "get_KNN_cover",
              kd_tree= "get_KNN_kd",
              VR = "get_KNN_VR" 
  ); 
  knnres<- .C(Cname, data, as.integer(k), d, n, nn.index = integer(n*k), nn.dist = double(n*k));
                                           
  nn.index<-  matrix(knnres$nn.index, byrow=T, nrow=n, ncol=k);
  nn.dist<- matrix(knnres$nn.dist, byrow=T, nrow=n, ncol=k);
  
  if(k>=n){
      nn.index[, n:k]<- NA;
      nn.dist[, n:k]<- NA;
  }    
    
  return(list(nn.index=nn.index, nn.dist=nn.dist));    
}
get.knnx<- function (data, query, k = 10, algorithm=c("cover_tree", "kd_tree", "VR"))
{
  #k neearest neighbor Euclidean distances
  algorithm<- match.arg(algorithm);	  
  if(storage.mode(data)=="integer") storage.mode(data)<- "double";
  if(storage.mode(query)=="integer") storage.mode(query)<- "double";

  if(!is.matrix(data)) data<- as.matrix(data);
  if(!is.matrix(query)) query<- as.matrix(query);

  n <- nrow(data); m<- nrow(query);
  d <- ncol(data); p<- ncol(query);

  if(d!=p) stop("Number of columns must be same!.");
  if(k>=n) warning("k should be less than sample size!");
  
  Cname<- switch(algorithm, 
                cover_tree = "get_KNNX_cover",
                kd_tree= "get_KNNX_kd",                 
                VR = "get_KNNX_VR"
  ); 
  knnres<- .C(Cname, data, query, as.integer(k), d, n, m, nn.index = integer(m*k), nn.dist = double(m*k));
                                             
  nn.index<-  matrix(knnres$nn.index, byrow=T, nrow=n, ncol=k);
  nn.dist<- matrix(knnres$nn.dist, byrow=T, nrow=n, ncol=k);

  if(k>=n) {
    nn.index[, n:k]<- NA;
    nn.dist[, n:k]<- NA;
  }

  return(list(nn.index=nn.index, nn.dist=nn.dist));
}
knnx.dist<- function (data, query, k = 10, algorithm=c("cover_tree", "kd_tree", "VR")) 
{
  nn.dist<- get.knnx(data, query, k, algorithm )$nn.dist  
}
knnx.index<- function (data, query, k = 10, algorithm=c("cover_tree", "kd_tree", "VR"))
{
  nn.index<- get.knnx(data, query, k, algorithm )$nn.index;
}
knn.index<- function (data, k = 10, algorithm=c("cover_tree", "kd_tree", "VR")) 
{
  get.knn(data, k, algorithm )$nn.index;  
}
knn.dist<- function (data, k = 10, algorithm=c("cover_tree", "kd_tree", "VR")) 
{
  get.knn(data, k, algorithm )$nn.dist; 
}

KL.dist<- function (X, Y, k = 10, algorithm="kd_tree") 
{
  #Kullback-Leibler distance
  algorithm<- match.arg(algorithm);
  if(storage.mode(X)=="integer") storage.mode(X)<- "double";
  if(storage.mode(Y)=="integer") storage.mode(Y)<- "double";

  if(!is.matrix(X)) X<- as.matrix(X);
  if(!is.matrix(Y)) Y<- as.matrix(Y);

  n<- nrow(X); m<- nrow(Y);
  d <- ncol(X); p<- ncol(Y);
  
  if(d!=p) stop("Number of columns must be same!.");
  if(k>=n) warning("k should be less than sample size!");

  kl.dist<- .C("KL_dist", X, Y, as.integer(k), d, n, m, kl.dist = double(k), DUP=FALSE)$kl.dist;    

  return(kl.dist);  
}

KLD.dist<- function (X, Y, k = 10, algorithm="kd_tree") 
{
  #Symmetric Kullback-Leibler distance, i.e. Kullback-Leibler divergence.
  algorithm<- match.arg(algorithm);
  if(storage.mode(X)=="integer") storage.mode(X)<- "double";
  if(storage.mode(Y)=="integer") storage.mode(Y)<- "double";

  if(!is.matrix(X)) X<- as.matrix(X);
  if(!is.matrix(Y)) Y<- as.matrix(Y);

  n<- nrow(X); m<- nrow(Y);
  d<- ncol(X); p<- ncol(Y);
  
  if(d!=p) stop("Number of columns must be same!.");
  if(k>=n) warning("k should be less than sample size!");

  klc.dist<- .C("KLD_dist", X, Y, as.integer(k), d, n, m, kl.dist = double(k), DUP=FALSE)$kl.dist;    

  return(klc.dist);   
}
KLx.dist<- function(x, y, k=1)
{
  #Kullback-Leibler Distance
  if (!is.matrix(x))  x<- matrix(x);
  if (!is.matrix(y))  y<- matrix(y);

  n<- nrow(x); p<- ncol(x);
  m<- nrow(y);

  log(m/n) + p*(colMeans(log(knnx.dist(y, x, k=k)))- colMeans(log(knn.dist(x, k=k))));

}

KLs.dist<- function(x, y, k=1)
{
  #Symmetric Kullback-Leibler distance, i.e. Kullback-Leibler divergence.
 KLx.dist(x, y, k)+ KLx.dist(y, x, k)
 
}
