#include "label_vector.h"
#include <cmath>

int dim;

bool operator < ( const Id_dist &a, const Id_dist &b)
{
  return a.dist < b.dist;
}

float distance(label_vector p1, label_vector p2, float upper_bound)
{
  float sum = 0.;
  float d12;
  
  for(int i=0; i<dim; i++){
       d12 = p1.coord[i] - p2.coord[i];
       d12 *= d12;
       sum += d12;
  }
  
  return sqrt(sum);
}
