#define f() 1 g()
#define g() 2 f() h()
#define h() 3
g()

 #define z \
#define y 123
z
y


#define foo(
) bar
foo()

