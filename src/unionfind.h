#ifndef unionfind_H_
#define unionfind_H_

// Modified union-find algorithm from here.
// https://github.com/kartikkukreja/blog-codes/blob/master/src

// TODO replace graph terminology with set language. 
// comps to sets 
// lcc to maximal set or similar 
// e for edge to mergecount or similar
// isolated with singleton
class unionfind 
{
  int *id, *sz;  // set id and size

  public:
    unionfind(int N){
      comps = N; 
      lcc = 1;
      e = 0;
      isolated = N;
      id = new int[N];
      sz = new int[N];
      for(int i = 0; i < N; i++){
        id[i] = i;
        sz[i] = 1; 
      }
    }

    int comps, e, isolated, lcc;

    // Return the id of component corresponding to object p.
    int find(int p){
      int root = p;
      while(root != id[root]) root = id[root];
      while(p != root){
        int newp = id[p]; 
        id[p] = root; 
        p = newp;
      }
      return root;
    }

    // Replace sets containing x and y with their union.
    void merge(int x, int y){
      int i = find(x); 
      int j = find(y); 
      ++e;
      if(i == j) return;

      if(sz[i] == 1) isolated--;
      if(sz[j] == 1) isolated--;

      // Make smaller root point to larger one.
      if(sz[i] < sz[j]){
        id[i] = j;
        sz[j] += sz[i]; 
        if(sz[j] > lcc) lcc = sz[j];
      }else{
        id[j] = i;
        sz[i] += sz[j];
        if(sz[i] > lcc) lcc = sz[i];
      }
      comps--;
    }
};
#endif
