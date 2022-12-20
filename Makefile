SRCS := $(wildcard */day*.lean)
OBJS = $(SRCS:.lean=.c)
EXES = $(OBJS:.c=.out)


%.c: %.lean
	lean -c $@ $<

%: %.c
	leanc -o $@ $<

all: $(EXES)
	echo $(SRCS)
	echo $(SRCS:lean=c)

clean:
	rm $(EXES)
