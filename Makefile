SRCS := $(wildcard */day*.lean)
OBJS = $(SRCS:.lean=.c)
EXES = $(OBJS:.c=)


%.c: %.lean
	lean -c $@ $<

%: %.c
	leanc -o $@ $<

all: $(EXES)
	echo $(SRCS)
	echo $(SRCS:lean=c)
