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

 #define xyz 456
xyz

#if 1 + 1 == \
             2
yes
#else
no
#endif


#define z1(x,y) x y
#define z2(x,y) x y
z1(z2(a,b),z2(c,d))
z1(z3(a,b),z3(c,d))
z1((a,b),(c,d))
z1({a,b},{c,d})

#define TOTO to\
to
TOTO

#ifndef x
"x" is not defined
#else
"x" is defined
#endif
