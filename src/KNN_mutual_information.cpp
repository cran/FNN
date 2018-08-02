/******************************************************************************\
* File: KNN_mutual_information.cpp                                              *
\******************************************************************************/
#include <R.h>
#define MAX_TIES 1000

extern "C"{
void mutinfo(double *XY, const int *kin, const int *n_pts, int* nx, int*ny)
 //Mutual Information
{
  int		d = 2;		// Actual Dimension
  int		n = *n_pts;	// Number of Data points
  int		K = *kin;
  int   	k1, kn;
  int   	*pos = new int[MAX_TIES+K];
  double 	dx, dy, dist;
  double  *nndist = new double[MAX_TIES+K];

    for (int i = 0; i < n; i++) {
  		kn = K;
  		for (int k = 0; k < kn; k++)	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++){
          	if(j == i) continue; //no self-match allowed

   	      	dx = fabs(XY[i*d]-XY[j*d]);
  	    	dy = fabs(XY[i*d+1]-XY[j*d+1]);

  	    	if(dx > dy ) dist = dx;
          	else dist = dy;

  	    	if (dist <= nndist[K - 1] )
  			  for (int k = 0; k <= kn; k++)
  		    		if (dist < nndist[k]) {
      					for (k1 = kn; k1 > k; k1--) {
      			    			nndist[k1] = nndist[k1 - 1];
      			    			pos[k1] = pos[k1 - 1];
      					}
      					nndist[k] = dist;

      					pos[k] = j;

      					if (nndist[kn] <= nndist[K - 1]) if (++kn == MAX_TIES - 1)  error("too many ties in knn");
  				  	break;
  		    		}

  	    		nndist[kn] = 0.99 * DBL_MAX;
  			}

	      	nx[i] = 0;
	      	ny[i] = 0;

		for (int j = 0; j < n; j++){
			//if(j == i) continue; //no self-match allowed
			dx = fabs(XY[i*d]-XY[j*d]);
			dy = fabs(XY[i*d+1]-XY[j*d+1]);
			if(dx < nndist[K-1]) nx[i]++;
			if(dy < nndist[K-1]) ny[i]++;
	      }

	}

	return;

} //end of mutinfo


void mdmutinfo(double *X, double *Y, const int *xdim, const int *ydim, const int *kin, const int *n_pts, int* nx, int*ny)
 //Multidimensional Mutual Information
{
  int		d1 = *xdim;		// Actual Dimension
  int		d2 = *ydim;		// Actual Dimension
  int		n = *n_pts;	// Number of Data points
  int		K = *kin;
  int   	k1, kn;
  int   	*pos = new int[MAX_TIES+K];
  double 	dx, dy, dist, tmp;
  double  *nndist = new double[MAX_TIES+K];

    for (int i = 0; i < n; i++) {
  		kn = K;
  		for (int k = 0; k < kn; k++)	nndist[k] = 0.99 * DBL_MAX;
  		for (int j = 0; j < n; j++){
          if(j == i) continue; //no self-match allowed

         dist = 0.0;

	       for (int k = 0; k < d1; k++) {
     	      tmp = fabs(X[i*d1+k]-X[j*d1+k]);
  			    if (tmp > dist) dist=tmp;
  	    	}

	       for (int k = 0; k < d2; k++) {
     	      tmp = fabs(Y[i*d2+k]-Y[j*d2+k]);
  			    if (tmp > dist) dist=tmp;
  	    	}

  	    	if (dist <= nndist[K - 1])
  			  for (int k = 0; k <= kn; k++)
  		    	if (dist < nndist[k]) {
      					for (k1 = kn; k1 > k; k1--) {
      			    		nndist[k1] = nndist[k1 - 1];
      			    		pos[k1] = pos[k1 - 1];
      					}
      					nndist[k] = dist;

      					pos[k] = j;

      					if (nndist[kn] <= nndist[K - 1])	if (++kn == MAX_TIES - 1)  error("too many ties in knn");
  				  	  break;
  		    	}

  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}

      nx[i] = 0;
      ny[i] = 0;

	    for (int j = 0; j < n; j++){
        //if(j == i) continue; //no self-match allowed

         dx = 0.0;
	       for (int k = 0; k < d1; k++) {
     	      tmp = fabs(X[i*d1+k]-X[j*d1+k]);
  			    if (tmp > dx) dx=tmp;
  	    	}

	       dy = 0.0;
	       for (int k = 0; k < d2; k++) {
     	      tmp = fabs(Y[i*d2+k]-Y[j*d2+k]);
  			    if (tmp > dy) dy=tmp;
  	    	}

        if(dx < nndist[K-1]) nx[i]++;
        if(dy < nndist[K-1]) ny[i]++;
      }

	}

	return;

} //end of Multidimensional Mutual Information


}  //end of extern
