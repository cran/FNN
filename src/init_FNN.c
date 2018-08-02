#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

void get_KNN_cover(double *data, const int *k, 
                  const int *p, const int *n_pts,
                  int *nn_idx, double *nn_dist);

void get_KNNX_cover(double *data,  double *query,
                    const int *k, const int *p, 
                    const int *n_pts, int* m_pts, 
                    int *nn_idx, double *nn_dist);

void get_KNN_kd(double *data, const int *k, const int *dim, const int *n_pts, int *nn_idx, double *nn_dist);

void get_KNNX_kd(double *data, double* query,
                 int *k, int *dim,
                 int *n_pts, int *m_pts,
                 int *nn_idx, double *nn_dist);

				 
void get_KNN_brute(double *data, const int *k, const int *dim, const int *n_pts, int *nn_idx, double *nn_dist);
void get_KNNX_brute(double *data, double* query,
                 int *k, int *dim,
                 int *n_pts, int *m_pts,
                 int *nn_idx, double *nn_dist);
				 
void KNN_MLD_brute(double *data, const int *k, const int *dim,
                const int *n_pts, double *MLD);

void KNN_MLD_kd(double *data, const int *k, const int *dim,
                const int *n_pts, double *MLD);

void KL_divergence(double *X, double* Y, const int *k, const int *dim,
     const int *n_pts, const int *m_pts, double* kl_dive);

void KL_dist(double *X, double* Y, const int *k, const int *dim,
     const int *n_pts, const int *m_pts, double* kl_dist);

void get_KNN_CR(
                const double *train, //data
          	    const int *kin,    //number of neighbors
              	const int *dim,      //dimension
              	const int *n_pts,   //number of training points
              	int   *nn_idx,     //indice of neighbors
               	double *nn_dist    //distances of neighbors
             	);
void get_KNNX_CR(
    	const double *train, //data
   	const double *test, //data
	const int *kin,    //number of neighbors
	const int *dim,      //dimension
	const int *n_pts,   //number of training points
	const int *m_pts,   //number of testing points
 	int   *nn_idx,     //indice of neighbors
   	double *nn_dist    //distances of neighbors
 		);
		
void mutinfo(double *XY, const int *kin, const int *n_pts, int* nx, int*ny);

void mdmutinfo(double *X, double *Y, const int *xdim, const int *ydim, 
	const int *kin, const int *n_pts, int* nx, int*ny);


#define C_DEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CMethodDef cMethods[] = {
 	C_DEF(get_KNN_cover, 6),
	C_DEF(get_KNNX_cover, 8),
	
	C_DEF(get_KNN_kd, 6),
	C_DEF(get_KNNX_kd, 8),

	C_DEF(get_KNN_CR, 6),
	C_DEF(get_KNNX_CR, 8),
	
	C_DEF(get_KNN_brute, 6),
	C_DEF(get_KNNX_brute, 8),
	
	C_DEF(KNN_MLD_brute, 5),
	C_DEF(KNN_MLD_kd, 5),
	
	C_DEF(KL_divergence, 7),
	C_DEF(KL_dist, 7),
	
/*not used
	C_DEF(mutinfo, 5),
*/	
	C_DEF(mdmutinfo, 8),
	{NULL, NULL, 0}
};


void R_init_FNN(DllInfo *info)
{
	/* Register the .C 
	No .Call routines.
	No .Fortran() or .External() routines,
	so pass those arrays as NULL.
	*/

	R_registerRoutines(info, cMethods, NULL, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
   	R_forceSymbols(info, TRUE);

}

