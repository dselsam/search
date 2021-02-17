PKG = Search
CPPFLAGS = -O3
include /home/dselsam/omega/lean4/build/current/stage1/share/lean/lean.mk

CPP_SRCS = inspect.cpp
CPP_OBJS = $(addprefix $(OUT)/testcpp/,$(CPP_SRCS:.cpp=.o))

all: $(BIN_OUT)/test

$(OUT)/testcpp/%.o: %.cpp
	@mkdir -p "$(@D)"
	c++ -std=c++14 -c -o $@ $< $(CPPFLAGS) `leanc -print-cflags`

$(BIN_OUT)/test: $(LIB_OUT)/libSearch.a $(CPP_OBJS) | $(BIN_OUT)
	c++ -rdynamic -o $@ $^ `leanc -print-ldflags`

clear:
	rm -f $(shell find . \( -name "*~" -o -name "*#" \))
