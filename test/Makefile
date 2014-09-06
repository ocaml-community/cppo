.PHONY: all ext comments

all: ext comments

ext:
	../cppo -x rot13:"tr '[a-z]' '[n-za-m]'" \
		-x source:"./source.sh '%F' %B %E" \
		ext.cppo > ext.out
	diff -u ext.ref ext.out

comments:
	../cppo comments.cppo > comments.out
	diff -u comments.ref comments.out
