all:
	@jbuilder build @install @DEFAULT

test:
	@jbuilder runtest

check: test

.PHONY: clean all check test

clean:
	jbuilder clean
