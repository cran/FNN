/******************************************************************************\
* File: KNN_correlation_distance.cpp                                           *
\******************************************************************************/ 
#include <R.h>
#define MAX_TIES 1000

extern "C"{
///////////
// Distance: 1- inner product
//////////

void get_KNN_CR(
                const double *train, //data
          	    const int *kin,    //number of neighbors
              	const int *dim,      //dimension
              	const int *n_pts,   //number of training points
              	int   *nn_idx,     //indice of neighbors
               	double *nn_dist    //distances of neighbors
             		)
{
  int		d = *dim;		// Actual Dimension
  int		n = *n_pts;	// Number of Data points
  int   	K = *kin;
  int   	k1, kn;
  int   	*pos = new int[MAX_TIES+K];
  double 	dist, tmp;
  double 	*nndist= new double[MAX_TIES+K];

  for (int i = 0; i < n; i++) {
  		kn = K;
  		for (int k = 0; k < kn; k++)	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++){ 
          if(j == i) continue; //no self-match allowed 
  	    	dist = 0.0;
  	    	for (int k = 0; k < d; k++) {
    			//inner product
    				tmp = train[k + i*d] * train[k + j*d];
    				dist += tmp ;
  	    	}
  	    	dist = 1.0 - dist; //map angle [0, pi] to [0,2]

  	    	if (dist <= nndist[K - 1])
  			  for (int k = 0; k <= kn; k++)
  		    	if (dist < nndist[k]) {
      					for (k1 = kn; k1 > k; k1--) {
      			    		nndist[k1] = nndist[k1 - 1];
      			    		pos[k1] = pos[k1 - 1];
      					}
      					nndist[k] = dist;
      			
      					pos[k] = j;
      					if (nndist[kn] <= nndist[K - 1])
      						if (++kn == MAX_TIES - 1)  Rf_error("too many ties in knn");
      						// if k = MAX_TIES/2-1, error occurs too!    						
  				  	  break;
  		    	}
    
  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}
  												
  		for (int k = 0; k < K; k++){	
  			nn_dist[i*K+k] = nndist[k];			
  			nn_idx[i*K+k] = pos[k]+1;	
  		}

	}
	delete[] pos;
	delete[] nndist;
	return;
}

void get_KNNX_CR(
    	const double *train, //data
   	const double *test, //data
	const int *kin,    //number of neighbors
	const int *dim,      //dimension
	const int *n_pts,   //number of training points
	const int *m_pts,   //number of testing points
 	int   *nn_idx,     //indice of neighbors
   	double *nn_dist    //distances of neighbors
 		)
{
	int		d=*dim;		// Actual Dimension
	int		n=*n_pts;	// Number of Data points
	int		m=*m_pts;	// Number of Data points
	int   K = *kin;
    int   k1, kn;
    int   *pos = new int[MAX_TIES+K];
    double dist, tmp;
    double *nndist = new double[MAX_TIES+K];

    for (int i = 0; i < m; i++) {
  		kn = K;
  		for (int k = 0; k < kn; k++)
  	    	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++) { 
  	    	dist = 0.0;
  	    	for (int k = 0; k < d; k++) {
  			    tmp = test[k + i*d] * train[k + j*d];
    			dist += tmp ;
  	    	}
  	    	dist = 1.0 - dist; //map angle [0, pi] to [0,2]
  	    	if (dist <= nndist[K - 1])
		      for (int k = 0; k <= kn; k++)
    		    	if (dist < nndist[k]) {
    					for (k1 = kn; k1 > k; k1--) {
    			    		nndist[k1] = nndist[k1 - 1];
    			    		pos[k1] = pos[k1 - 1];
    					}
    					nndist[k] = dist;
  			
    					pos[k] = j;
    					if (nndist[kn] <= nndist[K - 1])
    			    		if (++kn == MAX_TIES - 1) Rf_error("too many ties in knn");
    					break;
  		    	}
  
  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}
  												
  		for (int k = 0; k < K; k++){	
  			nn_dist[i*K+k]= nndist[k];			
  			nn_idx[i*K+k]=pos[k]+1;	
  		}		 
  
  	}

  	delete[] pos;
	delete[] nndist;	
	return;
}

}  //end of extern
