# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.12

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/martin/.local/share/JetBrains/Toolbox/apps/CLion/ch-0/182.3684.76/bin/cmake/linux/bin/cmake

# The command to remove a file.
RM = /home/martin/.local/share/JetBrains/Toolbox/apps/CLion/ch-0/182.3684.76/bin/cmake/linux/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/clion_first.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/clion_first.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/clion_first.dir/flags.make

CMakeFiles/clion_first.dir/main.c.o: CMakeFiles/clion_first.dir/flags.make
CMakeFiles/clion_first.dir/main.c.o: ../main.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/clion_first.dir/main.c.o"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clion_first.dir/main.c.o   -c /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/main.c

CMakeFiles/clion_first.dir/main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clion_first.dir/main.c.i"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/main.c > CMakeFiles/clion_first.dir/main.c.i

CMakeFiles/clion_first.dir/main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clion_first.dir/main.c.s"
	/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/main.c -o CMakeFiles/clion_first.dir/main.c.s

# Object files for target clion_first
clion_first_OBJECTS = \
"CMakeFiles/clion_first.dir/main.c.o"

# External object files for target clion_first
clion_first_EXTERNAL_OBJECTS =

clion_first: CMakeFiles/clion_first.dir/main.c.o
clion_first: CMakeFiles/clion_first.dir/build.make
clion_first: CMakeFiles/clion_first.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable clion_first"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/clion_first.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/clion_first.dir/build: clion_first

.PHONY : CMakeFiles/clion_first.dir/build

CMakeFiles/clion_first.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/clion_first.dir/cmake_clean.cmake
.PHONY : CMakeFiles/clion_first.dir/clean

CMakeFiles/clion_first.dir/depend:
	cd /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug /home/martin/uni/melbourne/parallel-and-multicore-computing/work/clion-first/cmake-build-debug/CMakeFiles/clion_first.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/clion_first.dir/depend

