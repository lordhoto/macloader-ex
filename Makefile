CXX ?= g++
CXXFLAGS ?= -Wall -g
MKDIR ?= mkdir -p
DEPDIR ?= .deps
OBJECTS := code0.o emulation.o mac-memory.o macresfork.o main.o traps.o util.o
BIN := macloader-ex

$(BIN): $(OBJECTS)
	$(CXX) $+ -o $@

-include $(wildcard $(addsuffix /*.d,$(DEPDIR)))

%.o: %.cpp
	$(MKDIR) $(DEPDIR)
	$(CXX) -MMD -MF "$(DEPDIR)/$(*F).d" -MQ "$@" -MP $(CXXFLAGS) -c $(<) -o $*.o

clean:
	rm -f $(BIN)
	rm -f $(OBJECTS)
	rm -fR $(DEPDIR)

all: $(BIN)
