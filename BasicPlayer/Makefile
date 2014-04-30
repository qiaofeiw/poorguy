
#for using prolog
ARCH := x86_64_macosx
# You need to have set ECLIPSE_DIR environment variable to your Eclipse prolog installation directory!
ECLIPSE_INCLUDE_DIR := $(ECLIPSE_DIR)/include/$(ARCH)

CPPFLAGS := -I$(ECLIPSE_INCLUDE_DIR)
CXXFLAGS := -Wextra -Wall -O2 # -g

SRC := GameHttpServer.cc PrologConnector.cc EclipseTerm.cc player.cc
OBJ := $(SRC:.cc=.o)

#create static library with basicplayer object files
basicplayer.a: $(OBJ)
	ar -r $@ $^
	ranlib $@

# additional dependencies
$(OBJ): Makefile

GameHttpServer.o: GameHttpServer.hh
PrologConnector.o: PrologConnector.hh EclipseTerm.hh
EclipseTerm.o: EclipseTerm.hh
player.o: player.hh EclipseTerm.hh PrologConnector.hh

clean:
	rm -f $(OBJ) basicplayer.a
