.PHONY: all ext

all: ext
	../cppo -x rot13:"tr '[a-z]' '[n-za-m]'" \
		-x source:"./source.sh '%F' %B %E" \
		ext.cppo > ext.out
	diff -u ext.ref ext.out
