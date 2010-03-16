//////////////////////////////////////////////////////////////////
// Near Neighbours Program                       				//
// Tree construction is fast, search is slow.                   //
//////////////////////////////////////////////////////////////////
#include <ANN/ANN.h> // ANN declarations
//#include <ANN/ANNx.h>					// more ANN declarations
//#include <ANN/ANNperf.h>				// performance evaluation

#include <math.h>    // math routines
#include <R.h>       // R header

extern "C" {
void get_KNN_kd(const double *data, const int *k, const int *dim, const int *n_pts, int *nn_idx, double *nn_dist)
{
	int				d=*dim;		// Actual Dimension
	int				n=*n_pts;	// Number of Data points
	double		error_bound = 0.0;
	int				K = *k;		// Max. num of NN
	
	ANNpointArray	data_pts  = annAllocPts(n, d);	// Data points
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
	ANNkd_tree		*kd_tree;	
	
	int d_ptr[d];
	
	for(int j = 0; j < d; j++)   d_ptr[j] = j*n; //row one pointers
		
	//copy data into n x d matrix
	for(int i = 0; i < n; i++) 
		for(int j = 0; j < d; j++) 		
		data_pts[i][j]= data[d_ptr[j]+i];	   	    
	
	kd_tree = new ANNkd_tree(data_pts, n, d);	

	int ptr=0;
	
	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		// query point
			K,				// number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound

	    for (int j = 0; j < K; j++)
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
			  nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			  ptr++;
	    } // end inner for
	} // end for


	annDeallocPts(data_pts);
	delete[] index;
	delete[] dist;
	
	delete kd_tree;
	
}//end KNN

void get_KNNX_kd(double *data, double* query, int *k, int *dim, int *n_pts, int *m_pts, int *nn_idx, double *nn_dist)
{
	int				d=*dim;		// Actual Dimension
	int				n=*n_pts;	// Number of Base Data points
	int				m=*m_pts;	// Number of Query Data points
	
	double			error_bound = 0.0;
	int				K = *k;		// Max. num of NN
	
	ANNpointArray	data_pts  = annAllocPts(n, d);	// Base Data points
	ANNpointArray	query_pts  = annAllocPts(m, d);	// Query Data points
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	if(query_pts==NULL) error("Cannot allocate memroy for query data matrix!\n");
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
	ANNkd_tree		*kd_tree;	
	
	int d_ptr[d];
	
	for(int j = 0; j < d; j++)   d_ptr[j] = j*n; //row one pointers
		
	//copy base data into n x d matrix
	for(int i = 0; i < n; i++) 
		for(int j = 0; j < d; j++) 		
		data_pts[i][j]= data[d_ptr[j]+i];	   	    
	
	kd_tree = new ANNkd_tree(data_pts, n, d);	
		
	for(int j = 0; j < d; j++)   d_ptr[j] = j*m; //row one pointers
		
	//copy data into m x d matrix
	for(int i = 0; i < m; i++) 
		for(int j = 0; j < d; j++) 		
		query_pts[i][j]= query[d_ptr[j]+i];	   	    
		
	
	int ptr=0;
	
	for(int i = 0; i < m; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			query_pts[i],		// query point
			K,				// number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound

	    for (int j = 0; j < K; j++)
	    {
	    	nn_dist[ptr] = sqrt(dist[j]);	// unsquare distance
			nn_idx[ptr]  = index[j] + 1;	// put indexes in returned array
			ptr++;
	    } // end inner for
	} // end for


	annDeallocPts(data_pts);
	annDeallocPts(query_pts);
	
	delete[] index;
	delete[] dist;
	
	delete kd_tree;
	
}//end KNNX


void KNNentropy(const double *data, const int *k, const int *dim, const int *n_pts, double *entropy)
{
	//For large dataset, use less memory than returning distances. 
	
	int				d=*dim;		// Actual Dimension
	int				n=*n_pts;	// Number of Data points
	double			error_bound = 0.0;
	int				K = *k;		// Max. num of NN
	
	ANNpointArray	data_pts  = annAllocPts(n, d);	// Data points
	if(data_pts==NULL) error("Cannot allocate memroy for data matrix!\n");
	
	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
	ANNkd_tree		*kd_tree;	
	
	int d_ptr[d];
	
	for(int j = 0; j < d; j++)   d_ptr[j] = j*n; //row one pointers
		
	//copy data into n x d matrix
	for(int i = 0; i < n; i++) 
		for(int j = 0; j < d; j++) 		
		data_pts[i][j]= data[d_ptr[j]+i];	   	    
	
	kd_tree = new ANNkd_tree(data_pts, n, d);	
	
	for(int i = 0; i < n; i++)	// read query points
	{
	    kd_tree->annkSearch(	// search
			data_pts[i],		// query point
			K,				// number of near neighbors
			index,				// nearest neighbors index
			dist,				// squared distance
			error_bound);		// error bound
			

	    for (int j = 0; j < K; j++)
	    {

	    	entropy[j] += log(dist[j]);

	    } // end inner for
	} // end for
	
	for (int j = 0; j < K; j++)
	{
	  	entropy[j] /= 2*n; //colMeans(log(dnn))
	} // end inner for


	annDeallocPts(data_pts);
	delete[] index;
	delete[] dist;
	
	delete kd_tree;
	
}//end KNNentropy



//Kullback-Leibler Distance
void KL_dist(double *X, double* Y, int *k, int *dim, int *n_pts, int *m_pts, double* kl_dist)
{
	
	int				d=*dim;		// Actual Dimension
	int				n=*n_pts;	// Number of Base X points
	int				m=*m_pts;	// Number of Y X points
	
	double			error_bound = 0.0;
	int				K = *k;		// Max. num of NN
	double			XX_dist[K]; 

	ANNpointArray	X_pts  = annAllocPts(n, d);	// X points
	ANNpointArray	Y_pts  = annAllocPts(m, d);	// X points
	if(X==NULL) error("Cannot allocate memroy for X vector!\n");
	if(Y==NULL) error("Cannot allocate memroy for Y vector!\n");

	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
	ANNkd_tree*		X_tree;
	ANNkd_tree*		Y_tree;	
	
	int d_ptr[d];
	
	for(int j = 0; j < d; j++)   d_ptr[j] = j*n; //row one pointers
		
	//copy X into n x d matrix
	for(int i = 0; i < n; i++) 
		for(int j = 0; j < d; j++) 		
		X_pts[i][j]= X[d_ptr[j]+i];	   	    
		
	//copy X into m x d matrix
	for(int j = 0; j < d; j++)   d_ptr[j] = j*m; //row one pointers		
	for(int i = 0; i < m; i++) 
		for(int j = 0; j < d; j++) 		
		Y_pts[i][j]= Y[d_ptr[j]+i];	   	    
	
	Y_tree = new ANNkd_tree(Y_pts, m, d);	
	
	for (int j = 0; j < K; j++)	 kl_dist[j] =  0.0;	

	for(int i = 0; i < n; i++)
	{
	    Y_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);
	    
	    for (int j = 0; j < K; j++)	 kl_dist[j] += log(dist[j]);	
	    
	}

	delete Y_tree;
	annDeallocPts(Y_pts); // release memory ASAP
	
	X_tree = new ANNkd_tree(X_pts, n, d);	

	
	for (int j = 0; j < K; j++)	 XX_dist[j] =  0.0;	
    
	for(int i = 0; i < n; i++){
	    X_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);
		
	    for (int j = 0; j < K; j++)	 XX_dist[j] += log(dist[j]);	
	    
	} 
	
	for (int j = 0; j < K; j++)	 kl_dist[j] =  log(double(m)/n)+ d*(kl_dist[j]-XX_dist[j])/2/n;	//divided by 2 because of squared distance
	
	delete X_tree;
	
	annDeallocPts(X_pts);

	delete[] index;
	delete[] dist;
	
	
}//end KL.dist

//Symmetric Kullback-Leibler Distance (Divergence)
void KLD_dist(double *X, double* Y, int *k, int *dim, int *n_pts, int *m_pts, double* kl_dist)
{
	
	int				d=*dim;		// Actual Dimension
	int				n=*n_pts;	// Number of Base X points
	int				m=*m_pts;	// Number of Y X points
	
	double			error_bound = 0.0;
	int				K = *k;		// Max. num of NN
		
	double XX_dist[K];
	double XY_dist[K];
	double YY_dist[K];
	double YX_dist[K];
	for (int j = 0; j < K; j++){
		XX_dist[j] =  0.0;	
		XY_dist[j] =  0.0;	
		YY_dist[j] =  0.0;	
		YX_dist[j] =  0.0;
	}

	ANNpointArray	X_pts  = annAllocPts(n, d);	// X points
	ANNpointArray	Y_pts  = annAllocPts(m, d);	// X points
	if(X==NULL) error("Cannot allocate memroy for X vector!\n");
	if(Y==NULL) error("Cannot allocate memroy for Y vector!\n");

	ANNidxArray		index = new ANNidx[K];		// Near neighbor indices
	ANNdistArray	dist = new ANNdist[K];		// Near neighbor nn_dist
	ANNkd_tree*		X_tree;
	ANNkd_tree*		Y_tree;					
	
	int d_ptr[d];
	
	for(int j = 0; j < d; j++)   d_ptr[j] = j*n; //row one pointers
		
	//copy X into n x d matrix
	for(int i = 0; i < n; i++) 
		for(int j = 0; j < d; j++) 		
		X_pts[i][j]= X[d_ptr[j]+i];	   	    
	
	
	//copy X into m x d matrix
	for(int j = 0; j < d; j++)   d_ptr[j] = j*m; //row one pointers		
	for(int i = 0; i < m; i++) 
		for(int j = 0; j < d; j++) 		
		Y_pts[i][j]= Y[d_ptr[j]+i];	   	    

	//Rprintf("X kd_tree construction is in progress...\n");
	X_tree = new ANNkd_tree(X_pts, n, d);	
	//Rprintf("X kd_tree is constructed.\n");

	//Rprintf("Searching Y in X kd_tree...\n");
	for(int i = 0; i < m; i++)
	{
	    X_tree->annkSearch(Y_pts[i], K,	index, dist, error_bound);
	    for (int j = 0; j < K; j++)	 YX_dist[j] += log(dist[j]);	
	}
	//Rprintf("Y are searched in X.\n");

	//Rprintf("Searching X in X kd_tree...\n");
	for(int i = 0; i < n; i++)
	{	    
	    X_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);		
	    for (int j = 0; j < K; j++)	 XX_dist[j] += log(dist[j]);		    
	} 
	//Rprintf("X are searched in X.\n");

	delete X_tree; //release ASAP

	//Rprintf("Y kd_tree construction is in progress...\n");
	Y_tree = new ANNkd_tree(Y_pts, m, d);	
	//Rprintf("Y kd_tree is constructed\n");

	Rprintf("Searching X in Y kd_tree...\n");
	for(int i = 0; i < n; i++)
	{
	    Y_tree->annkSearch(X_pts[i], K,	index, dist, error_bound);
	    
	    for (int j = 0; j < K; j++)	 XY_dist[j] += log(dist[j]);	
	    
	} 
	//Rprintf("X are searched in Y.\n");

	//Rprintf("Searching Y in Y kd_tree...\n");
	for(int i = 0; i < m; i++)
	{   
	    Y_tree->annkSearch(Y_pts[i], K,	index, dist, error_bound);		
	    for (int j = 0; j < K; j++)	 YY_dist[j] += log(dist[j]);		    
	} 

	//Rprintf("Y are searched in Y.\n");
	
	for (int j = 0; j < K; j++)	 kl_dist[j]=  d*(YX_dist[j]/m + XY_dist[j]/n - XX_dist[j]/n -YY_dist[j]/m)/2;	
	
	delete Y_tree;

	annDeallocPts(X_pts);
	annDeallocPts(Y_pts);
	
	delete[] index;
	delete[] dist;
	
	
}//end KL_dist

}//end extern

