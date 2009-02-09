#define f() 1 g()
#define g() 2 f() h()
#define h() 3
g()
