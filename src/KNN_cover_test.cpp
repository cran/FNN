/********************************************************************************
* This file is for testing purpose only. FNN lib will no depend on it.          *
* It can be linked against FNN shared library or compiled with other source code*
* together.                                                                     *
********************************************************************************/

/*Solaris does not have a native cstdio */
#ifdef __sun
#include <stdio.h>
#else
#include <cstdio>
#endif

#include "label_point.h"

extern int N;
extern int dim;

void print_index(int *nn_idx, int k)
{
  printf("$index:\n");
  int ptr=0;
  for(int i=0;i<N; i++){
    printf("%d: ", i+1);
    for(int j=0; j<k; j++){
      printf("%d ", nn_idx[ptr]);
      ptr++;
    }
    printf("\n");

  }
}

void print_dist(double *nn_dist, int k)
{
  printf("$dist:\n");
  int ptr=0;
  for(int i=0;i<N; i++){
    printf("%d: ", i+1);
    for(int j=0; j<k; j++){
      printf("%g ", nn_dist[ptr]);
      ptr++;
    }
    printf("\n");

  }
}

//extern "C" __declspec(dllimport) void get_KNN_cover(double *data, const int *k,  const int *p, const int *n_pts, int *nn_idx, double *nn_dist);
extern "C" void get_KNN_cover(double *data, const int *k,  const int *p, const int *n_pts, int *nn_idx, double *nn_dist);

int main(int argc, char *argv[])
{
   int k = atoi(argv[1]);
   double *data = parse_points(argv[2]);

   int* nn_idx= new int[N*k];
   double* nn_dist= new double[N*k];

   get_KNN_cover(data, &k, &dim, &N, nn_idx, nn_dist);
   get_KNN_cover(data, &k, &dim, &N, nn_idx, nn_dist);

   print_index(nn_idx, k);
   print_dist(nn_dist, k);

   delete[] nn_idx;
   delete[] nn_dist;
   free(data);

  return 0;
}

