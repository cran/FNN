#ifndef _LABEL_POINT_H
#define _LABEL_POINT_H

#include <memory.h>
#include <cstdio>

#include <cmath>
#include "stack.h"

#ifdef USING_R 
  #include "R.h"
  #ifdef USING_RPRINT
    #define printf Rprintf
    #define exit error
  //Rprintf only used within R.
  //Not for standalone program  linked with FNN library. (It will crash on Windows)
  #endif  
#endif 

extern int N;
extern int dim;

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

void print_neighbor(v_array<v_array<label_point> > res);

template<class P>
P* parse_points(char *filename)
{
  FILE *input = fopen(filename,"r");
  v_array<P * > parsed;
  
  char c;

  int ret;

  v_array<P> p;

  N=0;

  while ( (c = getc(input)) != EOF )
    {
      ungetc(c,input);

      while ((c = getc(input)) != '\n' )
	{
	  while (c != '0' && c != '1' && c != '2' && c != '3'
		 && c != '4' && c != '5' && c != '6' && c != '7'
		 && c != '8' && c != '9' && c != '\n' && c != EOF && c != '-')
	    c = getc(input);
	  if (c != '\n' && c != EOF) {
	    ungetc(c,input);
	    float f;
	    ret = fscanf(input, "%f",&f);
	    if(ret>0) push(p, (P)f);
	  }
	  else
	    if (c == '\n')
	      ungetc(c,input);
	}

      P *new_p= (P*)malloc(sizeof(P)*p.index);

      memcpy(new_p, p.elements,sizeof(P)*p.index);

      if (dim > 0 && dim!= p.index)
	   {
	      printf("Can't handle vectors of differing length, bailing\n");	      
          exit(0);
	   }

      dim = p.index;
      p.index = 0;
      push(parsed,new_p);
      N++;

    }

    P *data= new P[N*dim];
    int ptr=0;
    for(int i=0;i<N;i++){
      for(int j=0; j<dim; j++){
            data[ptr] = parsed.elements[i][j];
            ptr++;
      }
      free(parsed.elements[i]);
    }
    free(parsed.elements);

    return data;
}

 
#endif
