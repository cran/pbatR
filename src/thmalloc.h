// seriously, this is ridiculous

#ifndef __THMALLOC_H__
#define __THMALLOC_H__

// simple class for allocating data
template <typename T>
class thmalloc{
  public:
    T *data;
    thmalloc(unsigned int n){
      data = new T[n];
    }
    ~thmalloc(){
      delete [] data;
    }
};

// and then a macro
//#define THMALLOC(vartype, varname, size) thmalloc<vartype> ____varname(size); vartype* (varname) = ____varname.data;
#define THMALLOC(vartype, _varname, varname, size) thmalloc<vartype> _varname(size); vartype* varname = _varname.data;

// how to use?
// usually you might write
//   int foo[n]
// now you write
//   THMALLOC(int, foo, n)
// easy, and passes -pedantic

template <typename T>
class thmalloc2{
  public:
    T **data;
    unsigned int N1;
    thmalloc2(unsigned int n1, unsigned int n2){
      //data = new T[n1][n2];
      data = new T * [n1];
      for(unsigned int i=0; i<n1; i++)
        data[i] = new T[n2];
      N1 = n1; // needed for destructor
    }
    ~thmalloc2(){
      for(unsigned int i=0; i<N1; i++)
        delete [] data[i];
      delete [] data;
    }
};

#define THMALLOC2(vartype, _varname, varname, size1, size2) thmalloc2<vartype> _varname(size1, size2); vartype** varname = _varname.data;

#endif
