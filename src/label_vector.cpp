#include "label_vector.h"
#include <cmath>

const int batch = 120;//must be a multiple of 8
int point_len = 0;

//Assumption: points are a multiples of 8 long
float distance_vector(vector p1, vector p2, float upper_bound)
{
  float sum = 0.;
  float *end = p1 + point_len;
  upper_bound *= upper_bound;
  for (float *batch_end = p1 + batch; batch_end <= end; batch_end += batch)
  {
      for (; p1 != batch_end; p1+=2, p2+=2)
    	{
    	  float d1 = *p1 - *p2;
    	  float d2 = *(p1+1) - *(p2+1);
    	  d1 *= d1;
    	  d2 *= d2;
    	  sum = sum + d1 + d2;
    	}
      if (sum > upper_bound)
	       return sqrt(sum);
  }
  for (; p1 != end; p1+=2, p2+=2)
	{
	  float d1 = *p1 - *p2;
	  float d2 = *(p1+1) - *(p2+1);
	  d1 *= d1;
	  d2 *= d2;
	  sum = sum + d1 + d2;
	}
  return sqrt(sum);
}

bool operator < ( const Id_dist &a, const Id_dist &b)
{
  return a.dist < b.dist;
};

float distance(label_vector p1, label_vector p2, float upper_bound)
{
  return distance_vector(p1.coord, p2.coord, upper_bound);
}
