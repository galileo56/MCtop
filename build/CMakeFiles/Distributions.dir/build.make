# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.7

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
CMAKE_COMMAND = /opt/local/bin/cmake

# The command to remove a file.
RM = /opt/local/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/vmateu/GitHub/MCtop

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/vmateu/GitHub/MCtop/build

# Include any dependencies generated for this target.
include CMakeFiles/Distributions.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Distributions.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Distributions.dir/flags.make

CMakeFiles/Distributions.dir/src/Distributions.F90.o: CMakeFiles/Distributions.dir/flags.make
CMakeFiles/Distributions.dir/src/Distributions.F90.o: ../src/Distributions.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/Distributions.dir/src/Distributions.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/src/Distributions.F90 -o CMakeFiles/Distributions.dir/src/Distributions.F90.o

CMakeFiles/Distributions.dir/src/Distributions.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/Distributions.dir/src/Distributions.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/src/Distributions.F90 > CMakeFiles/Distributions.dir/src/Distributions.F90.i

CMakeFiles/Distributions.dir/src/Distributions.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/Distributions.dir/src/Distributions.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/src/Distributions.F90 -o CMakeFiles/Distributions.dir/src/Distributions.F90.s

CMakeFiles/Distributions.dir/src/Distributions.F90.o.requires:

.PHONY : CMakeFiles/Distributions.dir/src/Distributions.F90.o.requires

CMakeFiles/Distributions.dir/src/Distributions.F90.o.provides: CMakeFiles/Distributions.dir/src/Distributions.F90.o.requires
	$(MAKE) -f CMakeFiles/Distributions.dir/build.make CMakeFiles/Distributions.dir/src/Distributions.F90.o.provides.build
.PHONY : CMakeFiles/Distributions.dir/src/Distributions.F90.o.provides

CMakeFiles/Distributions.dir/src/Distributions.F90.o.provides.build: CMakeFiles/Distributions.dir/src/Distributions.F90.o


# Object files for target Distributions
Distributions_OBJECTS = \
"CMakeFiles/Distributions.dir/src/Distributions.F90.o"

# External object files for target Distributions
Distributions_EXTERNAL_OBJECTS =

Distributions: CMakeFiles/Distributions.dir/src/Distributions.F90.o
Distributions: CMakeFiles/Distributions.dir/build.make
Distributions: lib/liblibMCtop.a
Distributions: libsubdir_mods.a
Distributions: CMakeFiles/Distributions.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable Distributions"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Distributions.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Distributions.dir/build: Distributions

.PHONY : CMakeFiles/Distributions.dir/build

CMakeFiles/Distributions.dir/requires: CMakeFiles/Distributions.dir/src/Distributions.F90.o.requires

.PHONY : CMakeFiles/Distributions.dir/requires

CMakeFiles/Distributions.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Distributions.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Distributions.dir/clean

CMakeFiles/Distributions.dir/depend:
	cd /Users/vmateu/GitHub/MCtop/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build/CMakeFiles/Distributions.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Distributions.dir/depend

