/* Tests */

#define f() 1 g()
#define g() 2 f() h()
#define h() 3
g()

 #define z \
#define y 123
z
y

#define bar abc
#define foo(x, y) bar(x + y)
foo (,)


#define x a
#define a 1
x
#define a 2
x
#undef a
x

#define x __LINE__
x
x

#define f() macro f is defined on line __LINE__.
f()
f()

#define line __LINE__
#define f() line = __LINE__
f()
f()

#define __LINE__ __LINE__
__LINE__
