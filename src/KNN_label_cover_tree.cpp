/*******************************************************************************\
*	Cover tree near neighbours program                                           *
*  File: KNN_label_cover_tree.cpp                                              *
*  Author: Shegnqiao Li                                                        *
*  Label is added to the point to keep its ID for nearest neighbor indices     *
\*******************************************************************************/
#include "cover_tree.h"
#include "label_vector.h"

#include <vector>
#include <cmath>    
#include <algorithm>
#include <R.h>       // R header

extern int point_len; //declared in label_vector.h

//Assumption: points are a multiples of 8 long, otherwise zero-pad
v_array<label_vector> copy_points(const double* data, int n, int d)
{
  v_array<label_vector> parsed;
 
  if(d%8 > 0) point_len = (d/8 + 1)*8;
  else point_len = d;

  //float *coord = (float*)Calloc(n*point_len, float);
  //auto-initialize to 0, but  need Free by user 
  
   float *coord = (float*)R_alloc(n*point_len, sizeof(float));
  // not initialize to 0 

  int j;
  for( int i=0; i<n; i++)
   {  
      label_vector label_p;
      
      label_p.label = i;  //labels: 0 to n-1  Shengqiao Li      
      label_p.coord =  coord + i*point_len;
      j=0;
      for(;j<d; j++)  label_p.coord[j] = (float)data[j*n+i];             
      for(;j<point_len; j++) label_p.coord[j] = (float)(0.0); //zero-padding
      
      push(parsed, label_p);             
  }
  return parsed;
}

extern "C" {

void get_KNN_dist_cover(const double *data, const int *k,
                        const int *dim, const int *n_pts, 
                        double *nn_dist)
//only distance. may faster
{

	int	d=*dim;		// Actual Dimension
	int n=*n_pts;	// Number of Data points
	int	numNN = *k + 1;		// Max. num of NN including self
	int ptr;

  v_array<label_vector>  data_pts = copy_points(data, n, d);

  node<label_vector> top = batch_create(data_pts);
  node<label_vector> top_query = batch_create(data_pts);

  v_array<v_array<label_vector> > res;
  k_nearest_neighbor(top, top, res, numNN);

  for (int i = 0; i < n; i++)
  {	
  		double* dist= new double[res[i].index-1];
  		
  		for (int j = 1; j < res[i].index; j++){
  			dist[j-1] = distance(res[i][j], res[i][0], DBL_MAX);
  		}
  		sort(dist, dist+res[i].index-1);
  		
	    ptr = res[i][0].label*(*k); //searching is not in order. Reoder.  Shengqiao Li 3/12/2010
  
      for (int j = 1; j < numNN; j++)
 	    {
  	    	nn_dist[ptr] = dist[j];	
  			  ptr++;
 	    } 
  		delete[] dist;
   }
    	
}

void get_KNN_cover(const double *data, const int *k, 
                  const int *dim, const int *n_pts,
                  int *nn_idx, double *nn_dist)
{
	int	d=*dim;		// Actual Dimension
	int n=*n_pts;	// Number of Data points
	int	numNN = *k + 1;		// Max. num of NN including self
	int ptr;
 
  v_array<v_array<label_vector> > res;
      	
	v_array<label_vector>  data_pts = copy_points(data, n, d);
  node<label_vector> top = batch_create(data_pts);
  
  k_nearest_neighbor(top, top, res, numNN);
   
  for (int i = 0; i < n; i++)
  {	
	    ptr= (*k)*res[i][0].label; //position of results of ith point. 
	    
    	std::vector<Id_dist> dist;	
    	for (int j = 1; j < res[i].index; j++){
    		Id_dist adist(res[i][j].label+1, distance(res[i][j],res[i][0], DBL_MAX));
    		dist.push_back(adist);
    	} 

      sort(dist.begin(), dist.end()); 
          
    	for (int j = 1; j < numNN; j++)
    	{
          nn_idx[ptr] = dist[j].id;	
    	  	nn_dist[ptr] = dist[j].dist;

    		  ptr++;
    	}    
   }
  	
}

void get_KNNX_cover(const double *data, const double *query,
                    const int *k, const int *dim, 
                    const int *n_pts, int* m_pts, 
                    int *nn_idx, double *nn_dist)
{
	int	d=*dim;		// Actual Dimension
	int n=*n_pts;	// Number of Data points
	int m=*m_pts;	// Number of Query  points
	
	int	numNN = *k + 1;		// Max. num of NN including self
	int ptr;
 
  v_array<v_array<label_vector> > res;

	v_array<label_vector>  data_pts = copy_points(data, n, d);	
  node<label_vector> top = batch_create(data_pts);

  v_array<label_vector>  query_pts = copy_points(query, m, d);
  node<label_vector> top_query = batch_create(query_pts);

  k_nearest_neighbor(top, top_query, res, numNN);

  for (int i = 0; i < m; i++)
  {	
	    ptr= (*k)*res[i][0].label; //position of results of ith point. 
	    
    	std::vector<Id_dist> dist;	
    	for (int j = 1; j < res[i].index; j++){
    		Id_dist adist(res[i][j].label+1, distance(res[i][j],res[i][0], DBL_MAX));
    		dist.push_back(adist);
    	} 

      sort(dist.begin(), dist.end()); 
          
    	for (int j = 1; j < numNN; j++)
    	{
          nn_idx[ptr] = dist[j].id;	
    	  	nn_dist[ptr] = dist[j].dist;

    		  ptr++;
    	}    
  }
    	
}
} //end of extern  "C"

