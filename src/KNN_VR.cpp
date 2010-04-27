/*
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 or 3 of the License
 *  (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 *
 */
 
 
/*12/16/2008*/
 
#include <R.h>

#define EPS 1e-4		/* relative test of equality of distances */
#define MAX_TIES 1000
/* Not worth doing this dynamically -- limits k + # ties + fence, in fact */

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()

extern "C"{
void get_KNN_VR(
    const double *train, //data
		const int *kin,    //number of neighbors
		const int *dim,      //dimension
		const int *n_pts,   //number of training points
 	  int   *nn_idx,     //indice of neighbors
   	double *nn_dist    //distances of neighbors
 		)
{
	  int		d=*dim;		// Actual Dimension
	  int		n=*n_pts;	// Number of Data points
	  int   kinit = *kin;
    int   k1, kn;
    int   pos[MAX_TIES];
    double dist, tmp, nndist[MAX_TIES];
  
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (int i = 0; i < n; i++) {
  		kn = kinit;
  		for (int k = 0; k < kn; k++)	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++){ 
          if(j == i) continue; //no self-match allowed 
  	    	dist = 0.0;
  	    	for (int k = 0; k < d; k++) {
    			//	tmp = train[i + k * n] - train[j + k * n];
    				tmp = train[k + i*d] - train[k + j*d];
    				dist += tmp * tmp;
  	    	}
  			/* Use 'fuzz' since distance computed could depend on order of coordinates */
  	    	if (dist <= nndist[kinit - 1] * (1 + EPS))
  			  for (int k = 0; k <= kn; k++)
  		    	if (dist < nndist[k]) {
      					for (k1 = kn; k1 > k; k1--) {
      			    		nndist[k1] = nndist[k1 - 1];
      			    		pos[k1] = pos[k1 - 1];
      					}
      					nndist[k] = dist;
      			
      					pos[k] = j;
      					/* Keep an extra distance if the largest current one ties with current kth */
      					if (nndist[kn] <= nndist[kinit - 1])	if (++kn == MAX_TIES - 1)  error("too many ties in knn");
  				  	  break;
  		    	}
    
  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}
  												
  		for (int k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
  			nn_dist[i*kinit+k] = sqrt(nndist[k]);			
  			nn_idx[i*kinit+k] = pos[k]+1;	
  		}									/*Done for return distances and indice		*/		 

	}
	
	return;

}
void get_KNNX_VR(
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
	  int   kinit = *kin;
    int   k1, kn;
    int   pos[MAX_TIES];
    double dist, tmp, nndist[MAX_TIES];
  
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (int i = 0; i < m; i++) {
  		kn = kinit;
  		for (int k = 0; k < kn; k++)
  	    	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++) { 
  	    	dist = 0.0;
  	    	for (int k = 0; k < d; k++) {
  			//	tmp = test[i + k * m] - train[j + k * n]; //column major
  				tmp = test[k + i*d] - train[k+j*d];   //row major
  			
  				dist += tmp * tmp;
  	    	}
  		    /* Use 'fuzz' since distance computed could depend on order of coordinates */
  	    	if (dist <= nndist[kinit - 1] * (1 + EPS))
		      for (int k = 0; k <= kn; k++)
    		    	if (dist < nndist[k]) {
    					for (k1 = kn; k1 > k; k1--) {
    			    		nndist[k1] = nndist[k1 - 1];
    			    		pos[k1] = pos[k1 - 1];
    					}
    					nndist[k] = dist;
  			
    					pos[k] = j;
    					/* Keep an extra distance if the largest current one ties with current kth */
    					if (nndist[kn] <= nndist[kinit - 1])
    			    		if (++kn == MAX_TIES - 1)
    						error("too many ties in knn");
    					break;
  		    	}
  
  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}
  												
  		for (int k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
  			nn_dist[i*kinit+k]= sqrt(nndist[k]);			
  			nn_idx[i*kinit+k]=pos[k]+1;	
  		}									/*Done for return distances and indice		*/		 
  
  	}
	
	return;
}

//classification
void VR_knnc(const int *kin, 		//k
			 const int *lin, 		//l
			 const int *pntr, 		//number of training points
			 const int *pnte, 		//number of testing points			 
			 const int *p,    		//dimension
			 const double *train, 	//training set
			 const int *cl,  		//class label
			 const double *test,    //testing set
			 int *res, 				//prediction
			 double *pr, 			//posterior probability
			 int *votes,			//votes for a sample
			 const int *nc, 		//number of classes
			 const int *cv, 		//boolean
			 const int *use_all, 	//boolean
       		 int   *nn_idx,         //indice of neighbors
       		 double *nn_dist        //distances of neighbors
			 )
{
    int   i, index, j, k, k1, kinit = *kin, kn, l = *lin, mm, npat, ntie,
    	  ntr = *pntr, nte = *pnte, extras;
    int   pos[MAX_TIES], ncl[MAX_TIES];
    int   j1, j2, needed, t;
    double dist, tmp, nndist[MAX_TIES];
  
    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (npat = 0; npat < nte; npat++) {
		kn = kinit;
		for (k = 0; k < kn; k++) nndist[k] = 0.99 * DOUBLE_XMAX;
		for (j = 0; j < ntr; j++) {
	    	if ((*cv > 0) && (j == npat))	continue;
	    	dist = 0.0;
	    	for (k = 0; k < *p; k++) {
				tmp = test[npat + k * nte] - train[j + k * ntr];
				dist += tmp * tmp;
	    	}
			/* Use 'fuzz' since distance computed could depend on order of coordinates */
	    	if (dist <= nndist[kinit - 1] * (1 + EPS)){
				for (k = 0; k <= kn; k++){
		    		if (dist < nndist[k]) {
						for (k1 = kn; k1 > k; k1--) {
				    		nndist[k1] = nndist[k1 - 1];
				    		pos[k1] = pos[k1 - 1];
						}
						nndist[k] = dist;
			
						pos[k] = j;
					/* Keep an extra distance if the largest current one ties with current kth */
						if (nndist[kn] <= nndist[kinit - 1])
				    		if (++kn == MAX_TIES - 1)	error("too many ties in knn");
						break;
		    		}
	    	 	}
	    	}
	    	nndist[kn] = 0.99 * DOUBLE_XMAX;
		}

		for (k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
			nn_dist[k*nte+npat]= sqrt(nndist[k]);			
			nn_idx[k*nte+npat]=pos[k]+1;	
		}									/*Done for return distances and indice		*/		 
	
	
		for (j = 0; j <= *nc; j++) 	votes[j] = 0;
		if (*use_all) {
	    	for (j = 0; j < kinit; j++)	votes[cl[pos[j]]]++;
	    	extras = 0;
		    for (j = kinit; j < kn; j++) {
				if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
				extras++;
				votes[cl[pos[j]]]++;
	    	}
		} //end of "use all"
		else { /* break ties at random */
	    	extras = 0;
	    	for (j = 0; j < kinit; j++) {
				if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
		    		break;
				votes[cl[pos[j]]]++;
	    	}
	   		j1 = j;
	    	if (j1 == kinit - 1) votes[cl[pos[j1]]]++; /* no ties for largest */				
	    	else {
				/* Use reservoir sampling to choose amongst the tied distances */
				j1 = j;
				needed = kinit - j1;
				for (j = 0; j < needed; j++) ncl[j] = cl[pos[j1 + j]];
				t = needed;
				for (j = j1 + needed; j < kn; j++) {
		    		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
		    		if (++t * UNIF < needed) {
						j2 = j1 + (int) (UNIF * needed);
						ncl[j2] = cl[pos[j]];
		    		}
				}
				for (j = 0; j < needed; j++) votes[ncl[j]]++;
	    	}
		}//end not "not use all"

		/* Use reservoir sampling to choose amongst the tied votes */
		ntie = 1;
		if (l > 0) 	mm = l - 1 + extras;
		else  	mm = 0;
		index = 0;
		for (i = 1; i <= *nc; i++){
	    	if (votes[i] > mm) {
				ntie = 1;
				index = i;
				mm = votes[i];
	    	} else if (votes[i] == mm && votes[i] >= l) {
				if (++ntie * UNIF < 1.0) index = i;
	    	}
    	}
		res[npat] = index;
		pr[npat] = (double) mm / (kinit + extras);
    }
    RANDOUT;
}

//regression
void VR_knnr(const int *kin, 		//k
			 const int *pntr, 		//number of training points
			 const int *pnte, 		//number of testing points			 
			 const int *p,    		//dimension
			 const double *train, 	//training set
			 const double *Y,  		//response
			 const double *test,    //testing set
			 double *res, 			//prediction
			 const int *cv, 		//boolean
			 const int *use_all, 	//boolean
 		   int   *nn_idx,         //indice of neighbors
       double *nn_dist        //distances of neighbors
			 )
{
    int   j, k, k1, kinit = *kin, kn, npat,
    	  ntr = *pntr, nte = *pnte, extras;
    int   pos[MAX_TIES];
    double nY[MAX_TIES];
    int   j1, j2, needed, t;
    double dist, tmp, nndist[MAX_TIES];
  
    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
*/

    for (npat = 0; npat < nte; npat++) {
		kn = kinit;
		for (k = 0; k < kn; k++) nndist[k] = 0.99 * DOUBLE_XMAX;
		for (j = 0; j < ntr; j++) {
	    	if ((*cv > 0) && (j == npat))	continue;
	    	dist = 0.0;
	    	for (k = 0; k < *p; k++) {
				tmp = test[npat + k * nte] - train[j + k * ntr];
				dist += tmp * tmp;
	    	}
			/* Use 'fuzz' since distance computed could depend on order of coordinates */
	    	if (dist <= nndist[kinit - 1] * (1 + EPS)){
				for (k = 0; k <= kn; k++){
		    		if (dist < nndist[k]) {
						for (k1 = kn; k1 > k; k1--) {
				    		nndist[k1] = nndist[k1 - 1];
				    		pos[k1] = pos[k1 - 1];
						}
						nndist[k] = dist;
			
						pos[k] = j;
					/* Keep an extra distance if the largest current one ties with current kth */
						if (nndist[kn] <= nndist[kinit - 1])
				    		if (++kn == MAX_TIES - 1)	error("too many ties in knn");
						break;
		    		}
	    	 	}
	    	}
	    	nndist[kn] = 0.99 * DOUBLE_XMAX;
		}

		for (k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
			nn_dist[k*nte+npat]= sqrt(nndist[k]);			
			nn_idx[k*nte+npat]=pos[k]+1;	
		}									/*Done for return distances and indice		*/		 

		res[npat] = 0;
		if (*use_all) {
	    	for (j = 0; j < kinit; j++)	res[npat] += Y[pos[j]];
	    	extras = 0;
		    for (j = kinit; j < kn; j++) {
				if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
				extras++;
				res[npat] += Y[pos[j]];
	    	}
	    	res[npat] /= kn;
		} //end of "use all"
		else { /* break ties at random */
	    	extras = 0;
	    	for (j = 0; j < kinit; j++) {
				if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
		    		break;
				res[npat] += Y[pos[j]];
	    	}
	   		j1 = j; 
	    	if (j1 == kinit - 1) res[npat] += Y[pos[j1]]; /* no ties for largest */				
	    	else {
				/* Use reservoir sampling to choose amongst the tied distances */
				j1 = j;
				needed = kinit - j1;
				for (j = 0; j < needed; j++) nY[j] = Y[pos[j1 + j]];
				t = needed;
				for (j = j1 + needed; j < kn; j++) {
		    		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
		    		if (++t * UNIF < needed) {
						j2 = j1 + (int) (UNIF * needed);
						nY[j2] = Y[pos[j]];
		    		}
				}
				for (j = 0; j < needed; j++) res[npat] += nY[j];
	    	}
   			res[npat] /= kinit;
		}//end not "not use all"
    }
    RANDOUT;
}
}  //end of extern

