/**************************************************************************\
*	Cover tree near neighbours program                                       *
*  File: KNN_cover_tree.cpp                                                *
*  Author: Shegnqiao Li                                                    *
*  Cover tree will return more than k neighbors if there are more thant one*
*   kth distances                                                          *
*  Label is added to the point to keep its ID for nearest neighbor indices *
\**************************************************************************/
#include <vector>
#include <algorithm>
#include <limits>

#include "label_point.h"
#include "cover_tree.h"

#ifdef USING_R 
  #include "R.h"
  #ifdef USING_RPRINTF
    #define printf Rprintf
  //Rprintf only used within R.
  //Not for standalone program  linked with FNN library. (It will crash on Windows)
  #endif  
#endif

v_array<label_point> copy_points(const double* data, int n, int d)
//copy point address only
{
  dim = d;
  #ifdef USING_R
  register label_point* elems = Calloc(n, label_point);
  #else
  register label_point* elems = (label_point*)calloc(n, sizeof(label_point));  
  #endif            
  
  //converting  double to float doesn't help fewer than k neighbor problem.
  //float *coord = (float*)R_alloc(n*dim, sizeof(float));
  //for(int j = 0; j<n*dim; j++) coord[j] = (float)data[j];
  
  for(int i=0; i<n; i++)
  {          
    elems[i].label = i;
    elems[i].coord = data+i*d; 
//    print(elems[i]);
  }

  v_array<label_point> point_array;
  point_array.index = n;
  point_array.length = n;
  point_array.elements=elems;
  
  return point_array;
}

void free_data_pts( v_array<label_point> data_pts)
{
  #ifdef USING_R
    Free(data_pts.elements);
  #else
    free(data_pts.elements);
  #endif
  
  return;
}

template<class P>
void free_nodes (node<P> p)
//destructor of the cover tree. Release memory. R gc cannot recycle the memory.
// It's very fast even using recursion.
// freeing 2e6 nodes only take 1 seconds
// k_nearest_neighbor and batch_create take almost all the cpu time. The time for
// ohter steps is ignorable.
// non-recursive tree destructor is not necessary.                                        
{
  for(int i=0; i<p.num_children;i++)
     free_nodes(p.children[i]);      
  free(p.children);
        
  return;
}

extern "C" {
void get_KNN_dist_cover(double *data, const int *k,
                        const int *p, const int *n_pts, 
                        double *nn_dist)
//only distance. may faster
{
	const int	d=*p;		// Actual Dimension
	const int	n=*n_pts;	// Number of Data points
	const int	K= *k + 1;		// Max. num of NN including self
	int ptr;

  v_array<label_point>  data_pts = copy_points(data, n, d);

  node<label_point> top = batch_create(data_pts);

  v_array<v_array<label_point> > res;
  k_nearest_neighbor(top, top, res, K);
 
  std::vector<double> dist;	       
  for (int i = 0; i < n; i++)
  {	
  	     		      
  		for (int j = 0; j < K; j++){
  			dist.push_back(distance(res[i][j+1], res[i][0], MAXFLOAT));
  		}
  		sort(dist.begin(), dist.end());

  	  if(K>=res[i].index){
        printf("Cover tree only found %d neighbors for point %d.\n", res[i].index-2, res[i][0].label+1);
        printf("%d points are in the vector.\n", dist.size());        
     }
     
	    ptr = res[i][0].label*(*k); //searching is not in order. Reoder.  Shengqiao Li 3/12/2010  
      for (int j = 1; j < K; j++) //discard distance to itself and extra neighborss
 	    {
  	      if(j<res[i].index-1)  nn_dist[ptr] = dist[j];
          else	nn_dist[ptr] = numeric_limits<double>::quiet_NaN();       	
  			  ptr++;
 	    }
      dist.clear(); 
    	free(res[i].elements);
   }
   
   free(res.elements);  	
   free_nodes(top);
   free_data_pts(data_pts);
      
   return;
}

void get_KNN_cover(double *data, const int *k, 
                  const int *p, const int *n_pts,
                  int *nn_idx, double *nn_dist)
{
	const int	d=*p;		// Actual Dimension
	const int n=*n_pts;	// Number of Data points
	const int	K=*k + 1;		// Max. num of NN including self
	register int ptr;
 
  v_array<v_array<label_point> > res;
	v_array<label_point>  data_pts = copy_points(data, n, d);

  node<label_point> top = batch_create(data_pts);
  k_nearest_neighbor(top, top, res, K);
  #ifdef DEBUG
    print_neighbor(res);
  #endif
  
  std::vector<Id_dist> dist;	     
  for (int i = 0; i < n; i++)
  {	       
    for (int j = 1; j < res[i].index; j++){
    	Id_dist adist(res[i][j].label+1, distance(res[i][j],res[i][0], MAXFLOAT));
    	dist.push_back(adist);    	
    } 
    sort(dist.begin(), dist.end()); 
    #ifdef DEBUG
      printf("%d points are in the vector:", dist.size());        
      for(unsigned int j=0; j<dist.size(); j++) printf("%d ", dist.at(j).id); printf("\n");
    #endif       
    if(K>=res[i].index){
        printf("Cover tree only found %d neighbors for point %d.\n", res[i].index-2, res[i][0].label+1);
        printf("%d points are in the vector:", dist.size());
    #ifdef DEBUG
        for(unsigned int j=0; j<dist.size(); j++) printf("%d ", dist.at(j).id);
        printf("\n");
    #endif
    }
    
 	  ptr= (*k)*res[i][0].label; //position of results of ith point. 
    for (int j = 1; j < K; j++)  //discard itself  and extra neighbors
    {  
       if(j<res[i].index-1){   
          nn_idx[ptr] = dist.at(j).id;	
    	  	nn_dist[ptr] = dist.at(j).dist;
       }
       else{
          nn_idx[ptr] =-1;	
    	  	nn_dist[ptr] = numeric_limits<double>::quiet_NaN();
       }

 		    ptr++;
    	}
      dist.clear();
          
    	free(res[i].elements);
   }
  
  free(res.elements);	
  free_nodes(top);       
  free_data_pts(data_pts);

  return;  
}

void get_KNNX_cover(double *data,  double *query,
                    const int *k, const int *p, 
                    const int *n_pts, int* m_pts, 
                    int *nn_idx, double *nn_dist)
{
  const	int	d=*p;		// Actual Dimension
	const int n=*n_pts;	// Number of Data points
	const int m=*m_pts;	// Number of Query  points	
	const int	K=*k;		// Max. num of NN including self
	register int ptr;
 
  v_array<v_array<label_point> > res;

	v_array<label_point>  data_pts = copy_points(data, n, d);	
  node<label_point> top = batch_create(data_pts);

  v_array<label_point>  query_pts = copy_points(query, m, d);
  node<label_point> top_query = batch_create(query_pts);

  k_nearest_neighbor(top, top_query, res, K);
 	std::vector<Id_dist> dist;	
      	
  for (int i = 0; i < m; i++)
  {	
    	for (int j = 1; j < res[i].index; j++){
    		Id_dist adist(res[i][j].label+1, distance(res[i][j],res[i][0], MAXFLOAT));
    		dist.push_back(adist);
    	} 
      sort(dist.begin(), dist.end()); 

       if(K>=res[i].index){
          printf("Cover tree only found %d neighbors for point %d.\n", res[i].index-1, res[i][0].label+1);
          printf("%d points are in the vector.\n", dist.size());        
      }
      
      ptr= (*k)*res[i][0].label; //position of results of ith point.       
      for (int j = 0; j < K; j++) //discard extra neighbors
    	{
        if(j<res[i].index-1){
          nn_idx[ptr] = dist.at(j).id;	
    	  	nn_dist[ptr] = dist.at(j).dist;
        }
        else{
          nn_idx[ptr] =-1;	
    	  	nn_dist[ptr] = numeric_limits<double>::quiet_NaN();
        }
    		  ptr++;
    	}
    	
      dist.clear();
    	
    	free(res[i].elements);    
  }
  
  free(res.elements);
  
  free_nodes(top);
  free_nodes(top_query);
  free_data_pts(data_pts);	
  free_data_pts(query_pts);	
        
  return;      	
}
} //end of extern  "C"


