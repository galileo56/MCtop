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
CMAKE_SOURCE_DIR = /Users/vicent/GitHubProjects/MCtop

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/vicent/GitHubProjects/MCtop/build

# Include any dependencies generated for this target.
include CMakeFiles/subdir_mods.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/subdir_mods.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/subdir_mods.dir/flags.make

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o: CMakeFiles/subdir_mods.dir/flags.make
CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o: ../lib/MCtop.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vicent/GitHubProjects/MCtop/lib/MCtop.F90 -o CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/subdir_mods.dir/lib/MCtop.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vicent/GitHubProjects/MCtop/lib/MCtop.F90 > CMakeFiles/subdir_mods.dir/lib/MCtop.F90.i

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/subdir_mods.dir/lib/MCtop.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vicent/GitHubProjects/MCtop/lib/MCtop.F90 -o CMakeFiles/subdir_mods.dir/lib/MCtop.F90.s

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.requires:

.PHONY : CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.requires

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.provides: CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.requires
	$(MAKE) -f CMakeFiles/subdir_mods.dir/build.make CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.provides.build
.PHONY : CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.provides

CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.provides.build: CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o


CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o: CMakeFiles/subdir_mods.dir/flags.make
CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o: ../lib/Mathlink.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vicent/GitHubProjects/MCtop/lib/Mathlink.F90 -o CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o

CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vicent/GitHubProjects/MCtop/lib/Mathlink.F90 > CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.i

CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vicent/GitHubProjects/MCtop/lib/Mathlink.F90 -o CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.s

CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.requires:

.PHONY : CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.requires

CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.provides: CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.requires
	$(MAKE) -f CMakeFiles/subdir_mods.dir/build.make CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.provides.build
.PHONY : CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.provides

CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.provides.build: CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o


CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o: CMakeFiles/subdir_mods.dir/flags.make
CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o: ../lib/MatrixElements.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vicent/GitHubProjects/MCtop/lib/MatrixElements.F90 -o CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o

CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vicent/GitHubProjects/MCtop/lib/MatrixElements.F90 > CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.i

CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vicent/GitHubProjects/MCtop/lib/MatrixElements.F90 -o CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.s

CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.requires:

.PHONY : CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.requires

CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.provides: CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.requires
	$(MAKE) -f CMakeFiles/subdir_mods.dir/build.make CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.provides.build
.PHONY : CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.provides

CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.provides.build: CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o


CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o: CMakeFiles/subdir_mods.dir/flags.make
CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o: ../lib/SpecFun.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vicent/GitHubProjects/MCtop/lib/SpecFun.F90 -o CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o

CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vicent/GitHubProjects/MCtop/lib/SpecFun.F90 > CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.i

CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vicent/GitHubProjects/MCtop/lib/SpecFun.F90 -o CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.s

CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.requires:

.PHONY : CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.requires

CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.provides: CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.requires
	$(MAKE) -f CMakeFiles/subdir_mods.dir/build.make CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.provides.build
.PHONY : CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.provides

CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.provides.build: CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o


CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o: CMakeFiles/subdir_mods.dir/flags.make
CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o: ../lib/Vegas.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vicent/GitHubProjects/MCtop/lib/Vegas.F90 -o CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o

CMakeFiles/subdir_mods.dir/lib/Vegas.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/subdir_mods.dir/lib/Vegas.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vicent/GitHubProjects/MCtop/lib/Vegas.F90 > CMakeFiles/subdir_mods.dir/lib/Vegas.F90.i

CMakeFiles/subdir_mods.dir/lib/Vegas.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/subdir_mods.dir/lib/Vegas.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vicent/GitHubProjects/MCtop/lib/Vegas.F90 -o CMakeFiles/subdir_mods.dir/lib/Vegas.F90.s

CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.requires:

.PHONY : CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.requires

CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.provides: CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.requires
	$(MAKE) -f CMakeFiles/subdir_mods.dir/build.make CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.provides.build
.PHONY : CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.provides

CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.provides.build: CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o


# Object files for target subdir_mods
subdir_mods_OBJECTS = \
"CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o" \
"CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o" \
"CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o" \
"CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o" \
"CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o"

# External object files for target subdir_mods
subdir_mods_EXTERNAL_OBJECTS =

libsubdir_mods.a: CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/build.make
libsubdir_mods.a: CMakeFiles/subdir_mods.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/vicent/GitHubProjects/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Linking Fortran static library libsubdir_mods.a"
	$(CMAKE_COMMAND) -P CMakeFiles/subdir_mods.dir/cmake_clean_target.cmake
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/subdir_mods.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/subdir_mods.dir/build: libsubdir_mods.a

.PHONY : CMakeFiles/subdir_mods.dir/build

CMakeFiles/subdir_mods.dir/requires: CMakeFiles/subdir_mods.dir/lib/MCtop.F90.o.requires
CMakeFiles/subdir_mods.dir/requires: CMakeFiles/subdir_mods.dir/lib/Mathlink.F90.o.requires
CMakeFiles/subdir_mods.dir/requires: CMakeFiles/subdir_mods.dir/lib/MatrixElements.F90.o.requires
CMakeFiles/subdir_mods.dir/requires: CMakeFiles/subdir_mods.dir/lib/SpecFun.F90.o.requires
CMakeFiles/subdir_mods.dir/requires: CMakeFiles/subdir_mods.dir/lib/Vegas.F90.o.requires

.PHONY : CMakeFiles/subdir_mods.dir/requires

CMakeFiles/subdir_mods.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/subdir_mods.dir/cmake_clean.cmake
.PHONY : CMakeFiles/subdir_mods.dir/clean

CMakeFiles/subdir_mods.dir/depend:
	cd /Users/vicent/GitHubProjects/MCtop/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/vicent/GitHubProjects/MCtop /Users/vicent/GitHubProjects/MCtop /Users/vicent/GitHubProjects/MCtop/build /Users/vicent/GitHubProjects/MCtop/build /Users/vicent/GitHubProjects/MCtop/build/CMakeFiles/subdir_mods.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/subdir_mods.dir/depend

