# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.6

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
include CMakeFiles/Legendre.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Legendre.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Legendre.dir/flags.make

CMakeFiles/Legendre.dir/src/Legendre.F90.o: CMakeFiles/Legendre.dir/flags.make
CMakeFiles/Legendre.dir/src/Legendre.F90.o: ../src/Legendre.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/Legendre.dir/src/Legendre.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/src/Legendre.F90 -o CMakeFiles/Legendre.dir/src/Legendre.F90.o

CMakeFiles/Legendre.dir/src/Legendre.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/Legendre.dir/src/Legendre.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/src/Legendre.F90 > CMakeFiles/Legendre.dir/src/Legendre.F90.i

CMakeFiles/Legendre.dir/src/Legendre.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/Legendre.dir/src/Legendre.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/src/Legendre.F90 -o CMakeFiles/Legendre.dir/src/Legendre.F90.s

CMakeFiles/Legendre.dir/src/Legendre.F90.o.requires:

.PHONY : CMakeFiles/Legendre.dir/src/Legendre.F90.o.requires

CMakeFiles/Legendre.dir/src/Legendre.F90.o.provides: CMakeFiles/Legendre.dir/src/Legendre.F90.o.requires
	$(MAKE) -f CMakeFiles/Legendre.dir/build.make CMakeFiles/Legendre.dir/src/Legendre.F90.o.provides.build
.PHONY : CMakeFiles/Legendre.dir/src/Legendre.F90.o.provides

CMakeFiles/Legendre.dir/src/Legendre.F90.o.provides.build: CMakeFiles/Legendre.dir/src/Legendre.F90.o


# Object files for target Legendre
Legendre_OBJECTS = \
"CMakeFiles/Legendre.dir/src/Legendre.F90.o"

# External object files for target Legendre
Legendre_EXTERNAL_OBJECTS =

Legendre: CMakeFiles/Legendre.dir/src/Legendre.F90.o
Legendre: CMakeFiles/Legendre.dir/build.make
Legendre: lib/liblibMCtop.a
Legendre: libsubdir_mods.a
Legendre: CMakeFiles/Legendre.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable Legendre"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Legendre.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Legendre.dir/build: Legendre

.PHONY : CMakeFiles/Legendre.dir/build

CMakeFiles/Legendre.dir/requires: CMakeFiles/Legendre.dir/src/Legendre.F90.o.requires

.PHONY : CMakeFiles/Legendre.dir/requires

CMakeFiles/Legendre.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Legendre.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Legendre.dir/clean

CMakeFiles/Legendre.dir/depend:
	cd /Users/vmateu/GitHub/MCtop/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build/CMakeFiles/Legendre.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Legendre.dir/depend

