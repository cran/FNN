#include "label_point.h"
#include <cstdio>

int N;
int dim;

bool operator < ( const Id_dist &a, const Id_dist &b)
{
  return a.dist < b.dist;
}

float distance(label_point p1, label_point p2, float upper_bound)
{
  register float sum = 0.0;
  register float d12;
  register float max_sqd = upper_bound*upper_bound;
	
  //If using double and full distance, fewer than k neighbors may be found even k<<N
  for(int i=0; i<dim; i++){
       d12 = (float)p1.coord[i] - (float)p2.coord[i];
       d12 *= d12;
       sum += d12;
       if(sum>=max_sqd) return upper_bound;
  }

  return (float)sqrt(sum);
}

void print(label_point &p)
{
  printf("Point %2d: ", p.label+1);
  for (int i = 0; i<dim; i++)  printf("%g ", p.coord[i]);
  printf("\n");
}

void print_neighbor(v_array<v_array<label_point> > res)
{
  printf("=== Printing Results ===\n");
  for (int i = 0; i < res.index; i++)
  {
      printf("Step %d for: ", i);
      print(res[i][0]);
      
      for (int j = 1; j<res[i].index; j++)
      {
       printf("\t"); print(res[i][j]);   
      }
      printf("\n");
    }
    printf("=== Results Printed ===\n");  
    
    return;   
}

