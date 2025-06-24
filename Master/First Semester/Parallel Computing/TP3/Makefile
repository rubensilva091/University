SHELL = /bin/sh
BIN_NAME = sim_fluid

CXX = nvcc
LD  = nvcc

#UM Cluster  -arch=sm_35
#RTX 3060  -arch=sm_86

#CXXFLAGS   = -O2 -g -std=c++11 -arch=sm_35 -Wno-deprecated-gpu-targets
CXXFLAGS   = -O2 -g -std=c++11 -arch=sm_86 -Wno-deprecated-gpu-targets

SRC_DIR = src
BIN_DIR = bin
BUILD_DIR = build
SRC = $(wildcard $(SRC_DIR)/*.cu)
OBJ = $(patsubst %.cu,$(BUILD_DIR)/%.o,$(notdir $(SRC)))
BIN = $(BIN_NAME)

vpath %.cu $(SRC_DIR)

################################################################################
# Rules
################################################################################

.DEFAULT_GOAL = all

all: checkdirs $(BIN_DIR)/$(BIN_NAME)

$(BUILD_DIR)/%.o: %.cu
	$(CXX) -c $(CXXFLAGS) $(INCLUDES) $< -o $@ $(LIBS)

$(BIN_DIR)/$(BIN_NAME): $(OBJ)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $@ $(OBJ) $(LIBS)

checkdirs:
	@mkdir -p $(BUILD_DIR) $(BIN_DIR)

clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)

run:
	sbatch run.sh

################################################################################
# Dependencies (Optional)
################################################################################
$(BUILD_DIR)/main.o: src/EventManager.h src/fluid_solver.h
$(BUILD_DIR)/EventManager.o: src/EventManager.h
$(BUILD_DIR)/fluid_solver.o: src/fluid_solver.h
