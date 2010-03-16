#ifndef LABEL_VECTOR_H
#define LABEL_VECTOR_H

typedef float* vector;
float distance_vector(vector v1, vector v2, float upper_bound);

class label_vector{
	public:
	int label;
	vector coord;
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
bool operator < ( const Id_dist &a, const Id_dist &b);

float distance(label_vector v1, label_vector v2, float upper_bound);
#endif
