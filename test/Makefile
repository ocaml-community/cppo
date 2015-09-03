TESTS = ext comments cond tuple version
.PHONY: all clean $(TESTS)

all: $(TESTS)

ext:
	../cppo -x rot13:"tr '[a-z]' '[n-za-m]'" \
		-x source:"./source.sh '%F' %B %E" \
		ext.cppo > ext.out
	diff -u ext.ref ext.out

comments:
	../cppo comments.cppo > comments.out
	diff -u comments.ref comments.out

cond:
	../cppo cond.cppo > cond.out
	diff -u cond.ref cond.out

tuple:
	../cppo tuple.cppo > tuple.out
	diff -u tuple.ref tuple.out

loc:
	../cppo loc.cppo > loc.out
	diff -u loc.ref loc.out

version:
	../cppo -V X:123.05.2-alpha.1+foo-2.1 version.cppo > version.out

clean:
	rm -f *~ *.out
