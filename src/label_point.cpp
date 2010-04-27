#include "label_point.h"
#include <cstdio>
#include <memory.h>

#ifdef USING_RPRINTF
  #include "R.h"
  #define printf Rprintf   //only used in 
#endif  

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

int N;

coord_type* parse_points(char *filename)
{
  FILE *input = fopen(filename,"r");
  v_array<coord_type * > parsed;
  int ret;
  
  char c;

  v_array<coord_type> p;

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
	    push(p, (coord_type)f);
	  }
	  else
	    if (c == '\n')
	      ungetc(c,input);
	}

      coord_type *new_p= (coord_type*)malloc(sizeof(coord_type)*p.index);

      memcpy(new_p, p.elements,sizeof(coord_type)*p.index);

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

    coord_type *data= new coord_type[N*dim];
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

