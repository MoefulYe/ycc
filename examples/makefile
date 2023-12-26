SYSY_SRCS := $(wildcard *.sy.c)
SYSY_SRCS_NAME := $(foreach FILE, $(SYSY_SRCS), $(basename $(basename $(FILE))))

all: $(SYSY_SRCS_NAME)

$(SYSY_SRCS_NAME): %: %.sy.c
	ycc $< -t obj -o $@.o && clang get.c put.c $@.o -o $@.out
