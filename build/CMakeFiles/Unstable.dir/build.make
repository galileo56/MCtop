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
include CMakeFiles/Unstable.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Unstable.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Unstable.dir/flags.make

CMakeFiles/Unstable.dir/src/Unstable.F90.o: CMakeFiles/Unstable.dir/flags.make
CMakeFiles/Unstable.dir/src/Unstable.F90.o: ../src/Unstable.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/Unstable.dir/src/Unstable.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/src/Unstable.F90 -o CMakeFiles/Unstable.dir/src/Unstable.F90.o

CMakeFiles/Unstable.dir/src/Unstable.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/Unstable.dir/src/Unstable.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/src/Unstable.F90 > CMakeFiles/Unstable.dir/src/Unstable.F90.i

CMakeFiles/Unstable.dir/src/Unstable.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/Unstable.dir/src/Unstable.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/src/Unstable.F90 -o CMakeFiles/Unstable.dir/src/Unstable.F90.s

CMakeFiles/Unstable.dir/src/Unstable.F90.o.requires:

.PHONY : CMakeFiles/Unstable.dir/src/Unstable.F90.o.requires

CMakeFiles/Unstable.dir/src/Unstable.F90.o.provides: CMakeFiles/Unstable.dir/src/Unstable.F90.o.requires
	$(MAKE) -f CMakeFiles/Unstable.dir/build.make CMakeFiles/Unstable.dir/src/Unstable.F90.o.provides.build
.PHONY : CMakeFiles/Unstable.dir/src/Unstable.F90.o.provides

CMakeFiles/Unstable.dir/src/Unstable.F90.o.provides.build: CMakeFiles/Unstable.dir/src/Unstable.F90.o


# Object files for target Unstable
Unstable_OBJECTS = \
"CMakeFiles/Unstable.dir/src/Unstable.F90.o"

# External object files for target Unstable
Unstable_EXTERNAL_OBJECTS =

Unstable: CMakeFiles/Unstable.dir/src/Unstable.F90.o
Unstable: CMakeFiles/Unstable.dir/build.make
Unstable: lib/liblibMCtop.a
Unstable: libsubdir_mods.a
Unstable: CMakeFiles/Unstable.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable Unstable"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Unstable.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Unstable.dir/build: Unstable

.PHONY : CMakeFiles/Unstable.dir/build

CMakeFiles/Unstable.dir/requires: CMakeFiles/Unstable.dir/src/Unstable.F90.o.requires

.PHONY : CMakeFiles/Unstable.dir/requires

CMakeFiles/Unstable.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Unstable.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Unstable.dir/clean

CMakeFiles/Unstable.dir/depend:
	cd /Users/vmateu/GitHub/MCtop/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build/CMakeFiles/Unstable.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Unstable.dir/depend
