# for using prolog
ARCH := x86_64_linux
# You need to have set ECLIPSE_DIR environment variable to your Eclipse prolog installation directory!
ECLIPSE_LIB_DIR := $(ECLIPSE_DIR)/lib/$(ARCH)
ECLIPSE_INCLUDE_DIR := $(ECLIPSE_DIR)/include/$(ARCH)

# using the static basicplayer library
BASIC_PLAYER_DIR := BasicPlayer
BASIC_PLAYER_LIB := basicplayer.a
LIB := $(BASIC_PLAYER_DIR)/$(BASIC_PLAYER_LIB)

# setup compiler and linker flags
CPPFLAGS := -I$(ECLIPSE_INCLUDE_DIR) -I$(BASIC_PLAYER_DIR)
CXXFLAGS := -Wall -Wextra -O2 #-g
LD_FLAGS := $(CXXFLAGS) -L$(ECLIPSE_LIB_DIR)
LD_LIBS := -leclipse

# used compiler
LD := g++

SRC := main.cc playera.cc
OBJ := $(SRC:.cc=.o)
DEP := $(SRC:.cc=.d)

.PHONY: clean checkthis run

myPlayer: $(OBJ) $(LIB)
	$(LD) -o $@ $^ $(LD_FLAGS) $(LD_LIBS)

$(LIB): checkthis
	$(MAKE) -C $(BASIC_PLAYER_DIR) $(BASIC_PLAYER_LIB)

checkthis:

# additional dependencies
main.o: playera.hh $(BASIC_PLAYER_DIR)/GameHttpServer.hh $(BASIC_PLAYER_DIR)/PrologConnector.hh $(BASIC_PLAYER_DIR)/EclipseTerm.hh
playera.o: playera.hh $(BASIC_PLAYER_DIR)

$(OBJ): Makefile

clean:
	$(MAKE) -C $(BASIC_PLAYER_DIR) $@
	rm -f $(OBJ) myPlayer

run: myPlayer
	./myPlayer 4001
