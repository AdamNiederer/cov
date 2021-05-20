(
 ("/test/lcov/same-dir/extra.c" . (
  (3 2) ;  void extra(int a, int b, int c)
  (5 10) ;    for(int i = 0; i &lt; a; ++i)
  (6 48) ;      for(int j = 0; j &lt; b; ++j)
  (7 280) ;        for(int k = 0; k &lt; c; ++k)
  (8 240) ;          printf(&quot;%x%x%x\n&quot;, i, j, k);
  (9 2) ;  }
  ))
 ("/test/lcov/same-dir/main.c" . (
  (5 2) ;  int main(int argc, char* argv[])
  (7 2) ;    extra(4, 5, 6);
  (8 2) ;    if (argc &gt; 2){
  (9 1) ;      return atoi(argv[1]) + atoi(argv[2]);
  (11 1) ;    return 0;
  ))
)
