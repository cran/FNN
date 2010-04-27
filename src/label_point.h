#ifndef _LABEL_POINT_H
#define _LABEL_POINT_H

#include <cmath>
#include "stack.h"

typedef double coord_type;

class label_point{
	public:
	int label;
  const coord_type* coord;
};
class Id_dist  
{
  public:
     int id;
     float dist;

     Id_dist(int ind, float dis){
          id = ind;
          dist = dis;
     }
    friend bool operator < (const Id_dist &a, const Id_dist &b);
     
};
float distance(label_point p1, label_point p2, float upper_bound);
void print(label_point &p);

coord_type* parse_points(char *filename);
void print_neighbor(v_array<v_array<label_point> > res);
 
#endif
