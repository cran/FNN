/******************************************************************************\
* File: KNN_ANN.cpp                                                            *
* K Nearest Neighbour Program                       			       *
* Assume ANN lib allows Self-match                                             *
* Tree construction is fast, search is slow.                                   *
* Date:                                                                        *
*      2017-12-27 removed keyword register                                     *
\******************************************************************************/
#include <ANN/ANN.h> // ANN header
#include <R.h>       // R header

using namespace std;

void Rvector2ANNarray(ANNpointArray data_pts,
      double *data,
      const int n, const int d)
{
	//copy data point address into n x d matrix from d x n R vector column by column 
 	for (int i = 0; i < n; i++) {
		data_pts[i] = data+i*d;
	}	 	   	    
} 

extern "C" {

void get_KNN_brute(double *data, const int *k, const int *dim, const int *n_pts, int *nn_idx, double *nn_dist)
{
	const int	d=*dim;		// Actual Dimension
	const int	n=*n_pts;	// Number of Data points
	const int	K = *k+1;	// Max. num of NN. first one  (itself) will discarded.
	const double		error_bound = 0.0;
	int ptr = 0;

	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor squared distance
        ANNpointArray	data_pts = new ANNpoint[n];
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");

	Rvector2ANNarray(data_pts, data, n, d);

	ANNbruteForce	*kd_tree = new ANNbruteForce(data_pts, n, d);

	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		  // query point
			K,				      // number of near neighbors
			index,				  // nearest neighbors index
			dist,				  // squared distance
			error_bound);		  // error bound

	    for (int j = 1; j < K; j++)      //return result row by row
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
            nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			ptr++;
	    } // end inner for
	} // end for

	delete[] index;
	delete[] dist;
	delete kd_tree;
	delete[] data_pts;
	
	annClose();

}//end get_KNN_brute
void get_KNNX_brute(double *data, double* query,
                 int *k, int *dim,
                 int *n_pts, int *m_pts,
                 int *nn_idx, double *nn_dist)
{
	const int				d=*dim;		// Actual Dimension
	const int				n=*n_pts;	// Number of Base Data points
	const int				m=*m_pts;	// Number of Query Data points
	const int				K = *k;		// Max. num of NN
	const double		error_bound = 0.0;

	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
//	ANNpointArray	data_pts  = annAllocPts(n, d);	// Base Data points
//	ANNpointArray	query_pts  = annAllocPts(m, d);	// Query Data points
        ANNpointArray	data_pts  = new ANNpoint[n];
        ANNpointArray	query_pts  = new ANNpoint[m];

	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	if(query_pts==NULL) error("Cannot allocate memroy for query data matrix!\n");

	//copy data into  matrix from a vector column by column
	Rvector2ANNarray(data_pts, data, n, d);
 	Rvector2ANNarray(query_pts, query, m, d);

	ANNbruteForce	*kd_tree	= new ANNbruteForce(data_pts, n, d);

 	int ptr = 0;
	for(int i = 0; i < m; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			query_pts[i],		// query point
			K,				// number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound

	    for (int j = 0; j < K; j++)      //return result row by row
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
            nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			ptr++;
	    } // end inner for
	} // end for

	delete[] index;
	delete[] dist;
//	delete kd_tree;

//	annDeallocPts(data_pts);
//	annDeallocPts(query_pts);
	delete[] data_pts;
	delete[] query_pts;
	delete kd_tree;


	annClose();

}//end get_KNNX_brute

void get_KNN_kd(double *data, const int *k, const int *dim, const int *n_pts, int *nn_idx, double *nn_dist)
{
	const int	d=*dim;		// Actual Dimension
	const int	n=*n_pts;	// Number of Data points
	const int	K = *k+1;	// Max. num of NN. first one  (itself) will discarded.
	const double	error_bound = 0.0;
	int ptr = 0;
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor squared distance
        ANNpointArray	data_pts = new ANNpoint[n];
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	
	Rvector2ANNarray(data_pts, data, n, d);
	
	ANNkd_tree	*kd_tree = new ANNkd_tree(data_pts, n, d);	
  
	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		  // query point
			K,				      // number of near neighbors
			index,				  // nearest neighbors index
			dist,				  // squared distance
			error_bound);		  // error bound

	    for (int j = 1; j < K; j++)      //return result row by row
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
			  nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			  ptr++;
	    } // end inner for
	} // end for

	delete[] index;
	delete[] dist;
	delete kd_tree;
	delete[] data_pts;
	annClose();
	
}//end get_KNN_kd

void get_KNNX_kd(double *data, double* query,
                 int *k, int *dim,
                 int *n_pts, int *m_pts,
                 int *nn_idx, double *nn_dist)
{
	const int				d=*dim;		// Actual Dimension
	const int				n=*n_pts;	// Number of Base Data points
	const int				m=*m_pts;	// Number of Query Data points
	const int				K = *k;		// Max. num of NN	
	const double				error_bound = 0.0;
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor squared distance		
//	ANNpointArray	data_pts  = annAllocPts(n, d);	// Base Data points
//	ANNpointArray	query_pts  = annAllocPts(m, d);	// Query Data points
        ANNpointArray	data_pts  = new ANNpoint[n];
        ANNpointArray	query_pts  = new ANNpoint[m];

	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	if(query_pts==NULL) error("Cannot allocate memroy for query data matrix!\n");
		
	//copy data into  matrix from a vector column by column
	Rvector2ANNarray(data_pts, data, n, d);
 	Rvector2ANNarray(query_pts, query, m, d);

	ANNkd_tree	*kd_tree	= new ANNkd_tree(data_pts, n, d);	
  
 	int ptr = 0;	
	for(int i = 0; i < m; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			query_pts[i],		// query point
			K,				// number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound
			
	    for (int j = 0; j < K; j++)      //return result row by row
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
			  nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			  ptr++;
	    } // end inner for
	} // end for

	delete[] index;
	delete[] dist;	
//	delete kd_tree;

//	annDeallocPts(data_pts);
//	annDeallocPts(query_pts);
	delete[] data_pts;
	delete[] query_pts;
	delete kd_tree;

	
	annClose();
	
}//end get_KNNX_kd

void KNN_MLD_brute(double *data, const int *k, const int *dim,
                const int *n_pts, double *MLD)
//mean of log distances
{
 	//For large dataset, use less memory than returning distances.
	const int				d=*dim;		// Actual Dimension
	const int				n=*n_pts;	// Number of Data points
	const int				K = *k+1;		// Max. num of NN. first one  (itself) will discarded.
	const double		error_bound = 0.0;

	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor squared distance
	ANNpointArray	data_pts  = new ANNpoint[n];
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");

	//copy data into n x d matrix from n*d vector column by column
	Rvector2ANNarray(data_pts, data, n, d);

	ANNbruteForce	*kd_tree = new ANNbruteForce(data_pts, n, d);

	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		// query point
			K,				     // number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound

	    for (int j = 0; j < K-1; j++)
	    {
	    	MLD[j] += log(dist[j+1]);  //discard first one (0 distance).
	    } // end inner for
	} // end for

	for (int j = 0; j < K-1; j++)
	{
	  	MLD[j] /= 2*n; //colMeans(log(dnn))
	}

	delete[] index;
	delete[] dist;
	delete kd_tree;
    delete[] data_pts;
	annClose();

}//end KNN_MLD_brute

void KNN_MLD_kd(double *data, const int *k, const int *dim,
                const int *n_pts, double *MLD)
//mean of log distances                
{
 	//For large dataset, use less memory than returning distances. 
	const int				d=*dim;		// Actual Dimension
	const int				n=*n_pts;	// Number of Data points
	const int				K = *k+1;		// Max. num of NN. first one  (itself) will discarded.
	const double		error_bound = 0.0;
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist	
    ANNpointArray	data_pts  = new ANNpoint[n];
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
			
	//copy data into n x d matrix from n*d vector column by column
	Rvector2ANNarray(data_pts, data, n, d);  	    
	
	ANNkd_tree	*kd_tree = new ANNkd_tree(data_pts, n, d);	

	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		// query point
			K,				     // number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound
			
	    for (int j = 0; j < K-1; j++)
	    {
	    	MLD[j] += log(dist[j+1]);  //discard first one (0 distance).
	    } // end inner for
	} // end for
	
	for (int j = 0; j < K-1; j++)
	{
	  	MLD[j] /= 2*n; //colMeans(log(dnn))
	}

	delete[] index;
	delete[] dist;	
	delete kd_tree;
        delete[] data_pts;
  
	annClose();
	
}//end KNN_MLD_kd


void KL_divergence(double *X, double* Y, const int *k, const int *dim,
     const int *n_pts, const int *m_pts, double* kl_dive)
//Kullback-Leibler divergence
{
	const int			d=*dim;		// Actual Dimension
	const int			n=*n_pts;	// Number of X points
	const int			m=*m_pts;	// Number of Y points
	const int			K = *k;		// Max. num of NN	
	const double	error_bound = 0.0;
	
        double	*XX_dist= new double[K];
  
	double	*XY_dist= new double[K];
        ANNpointArray	X_pts = new ANNpoint[n];		// X points
        ANNpointArray	Y_pts = new ANNpoint[m];		// Y points
	if(X_pts==NULL) error("Cannot allocate memroy for X vector!\n");
	if(Y_pts==NULL) error("Cannot allocate memroy for Y vector!\n");
		
 	ANNidxArray		index = new ANNidx[K+1];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K+1];		// Near neighbor squared distance
	
	//copy data into a matrix from a R vector column by column
	Rvector2ANNarray(X_pts, X, n, d);
	Rvector2ANNarray(Y_pts, Y, m, d);

	ANNkd_tree *Y_tree = new ANNkd_tree(Y_pts, m, d);	

 	for(int j = 0; j < K; j++)	 XY_dist[j] =  0.0;	
	for(int i = 0; i < n; i++)
	{
	    Y_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);
	    for (int j = 0; j < K; j++)	 XY_dist[j] += log(dist[j]);	
	}
	
        delete Y_tree;
//	annDeallocPts(Y_pts); // release memory ASAP
        delete[] Y_pts;
	ANNkd_tree *X_tree = new ANNkd_tree(X_pts, n, d);	

	for (int j = 0; j < K; j++)	 XX_dist[j] =  0.0;	   
	for(int i = 0; i < n; i++){
	   //discard self-match
	    X_tree->annkSearch(X_pts[i], K+1,	index, dist, error_bound);		
	    for (int j = 0; j < K; j++)	 XX_dist[j] += log(dist[j+1]);		    
	} 

        delete[] index;
	delete[] dist;
 	delete X_tree;
	delete[] X_pts;
	annClose();

	for (int j = 0; j < K; j++)	 
        kl_dive[j] = log(double(m)/n)+ d*(XY_dist[j]-XX_dist[j])/2/n;	//divided by 2 because of squared distance

	delete[] XX_dist;
	delete[] XY_dist;
	
}//end KL_diver


void KL_dist(double *X, double* Y, const int *k, const int *dim,
     const int *n_pts, const int *m_pts, double* kl_dist)
 //Symmetric Kullback-Leibler Divergence (Distance) 
{

	const int			d=*dim;		// Actual Dimension
	const int			n=*n_pts;	// Number of X points
	const int			m=*m_pts;	// Number of Y points
	const int			K = *k;		// Max. num of NN	
	const double	error_bound = 0.0;
	
	double *XX_dist = new double[K];
	double *XY_dist = new double[K];
	double *YY_dist = new double[K];
	double *YX_dist = new double[K];
	
	for (int j = 0; j < K; j++){
		XX_dist[j] =  0.0;	
		XY_dist[j] =  0.0;	
		YY_dist[j] =  0.0;	
		YX_dist[j] =  0.0;
	}
 	ANNidxArray		index = new ANNidx[K+1];		// Near neighbor indices
	ANNdistArray	dist  = new ANNdist[K+1];		// Near neighbor squared distance
        ANNpointArray	X_pts = new ANNpoint[n];		// X points
        ANNpointArray	Y_pts = new ANNpoint[m];	  //Y points
	
	if(X_pts==NULL) error("Cannot allocate memroy for X vector!\n");
	if(Y_pts==NULL) error("Cannot allocate memroy for Y vector!\n");
	
	//copy data into a matrix from a R vector column by column
	Rvector2ANNarray(X_pts, X, n, d);
	Rvector2ANNarray(Y_pts, Y, m, d);
	
	ANNkd_tree *X_tree = new ANNkd_tree(X_pts, n, d);	

	for(int i = 0; i < m; i++)
	{
	    X_tree->annkSearch(Y_pts[i], K,	index, dist, error_bound);
	    for (int j = 0; j < K; j++)	 YX_dist[j] += log(dist[j]);	      
	}

	for(int i = 0; i < n; i++)
	{	    
	    X_tree->annkSearch(X_pts[i], K+1,	index, dist, error_bound);		
	    for (int j = 0; j < K; j++)	 XX_dist[j] += log(dist[j+1]);		    
	} 

	delete X_tree; //release ASAP

	ANNkd_tree *Y_tree = new ANNkd_tree(Y_pts, m, d);	

	for(int i = 0; i < n; i++)
	{
	    Y_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);
	    
	    for (int j = 0; j < K; j++)	 XY_dist[j] += log(dist[j]);	
	    
	} 

	for(int i = 0; i < m; i++)
	{   
	    Y_tree->annkSearch(Y_pts[i], K+1,	index, dist, error_bound);		
	    for (int j = 0; j < K; j++)	 YY_dist[j] += log(dist[j+1]);		    
	} 

	delete[] index;
	delete[] dist;
        delete Y_tree;

        delete[] X_pts;
        delete[] Y_pts;
 	annClose();	

	for (int j = 0; j < K; j++)	
    	kl_dist[j]=  d*(YX_dist[j]/m + XY_dist[j]/n - XX_dist[j]/n -YY_dist[j]/m)/2;	

	delete[] XX_dist;
	delete[] YY_dist;
	delete[] XY_dist;
	delete[] YX_dist;
	
}//end KL_dist

}//end extern

