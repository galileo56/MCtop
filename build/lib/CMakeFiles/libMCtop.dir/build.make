# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9

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
include lib/CMakeFiles/libMCtop.dir/depend.make

# Include the progress variables for this target.
include lib/CMakeFiles/libMCtop.dir/progress.make

# Include the compile flags for this target's objects.
include lib/CMakeFiles/libMCtop.dir/flags.make

lib/CMakeFiles/libMCtop.dir/MCtop.F90.o: lib/CMakeFiles/libMCtop.dir/flags.make
lib/CMakeFiles/libMCtop.dir/MCtop.F90.o: ../lib/MCtop.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object lib/CMakeFiles/libMCtop.dir/MCtop.F90.o"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/lib/MCtop.F90 -o CMakeFiles/libMCtop.dir/MCtop.F90.o

lib/CMakeFiles/libMCtop.dir/MCtop.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/libMCtop.dir/MCtop.F90.i"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/lib/MCtop.F90 > CMakeFiles/libMCtop.dir/MCtop.F90.i

lib/CMakeFiles/libMCtop.dir/MCtop.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/libMCtop.dir/MCtop.F90.s"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/lib/MCtop.F90 -o CMakeFiles/libMCtop.dir/MCtop.F90.s

lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.requires:

.PHONY : lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.requires

lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.provides: lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.requires
	$(MAKE) -f lib/CMakeFiles/libMCtop.dir/build.make lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.provides.build
.PHONY : lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.provides

lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.provides.build: lib/CMakeFiles/libMCtop.dir/MCtop.F90.o


lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o: lib/CMakeFiles/libMCtop.dir/flags.make
lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o: ../lib/Mathlink.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/lib/Mathlink.F90 -o CMakeFiles/libMCtop.dir/Mathlink.F90.o

lib/CMakeFiles/libMCtop.dir/Mathlink.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/libMCtop.dir/Mathlink.F90.i"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/lib/Mathlink.F90 > CMakeFiles/libMCtop.dir/Mathlink.F90.i

lib/CMakeFiles/libMCtop.dir/Mathlink.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/libMCtop.dir/Mathlink.F90.s"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/lib/Mathlink.F90 -o CMakeFiles/libMCtop.dir/Mathlink.F90.s

lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.requires:

.PHONY : lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.requires

lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.provides: lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.requires
	$(MAKE) -f lib/CMakeFiles/libMCtop.dir/build.make lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.provides.build
.PHONY : lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.provides

lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.provides.build: lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o


lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o: lib/CMakeFiles/libMCtop.dir/flags.make
lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o: ../lib/MatrixElements.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/lib/MatrixElements.F90 -o CMakeFiles/libMCtop.dir/MatrixElements.F90.o

lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/libMCtop.dir/MatrixElements.F90.i"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/lib/MatrixElements.F90 > CMakeFiles/libMCtop.dir/MatrixElements.F90.i

lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/libMCtop.dir/MatrixElements.F90.s"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/lib/MatrixElements.F90 -o CMakeFiles/libMCtop.dir/MatrixElements.F90.s

lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.requires:

.PHONY : lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.requires

lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.provides: lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.requires
	$(MAKE) -f lib/CMakeFiles/libMCtop.dir/build.make lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.provides.build
.PHONY : lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.provides

lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.provides.build: lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o


lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o: lib/CMakeFiles/libMCtop.dir/flags.make
lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o: ../lib/SpecFun.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/lib/SpecFun.F90 -o CMakeFiles/libMCtop.dir/SpecFun.F90.o

lib/CMakeFiles/libMCtop.dir/SpecFun.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/libMCtop.dir/SpecFun.F90.i"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/lib/SpecFun.F90 > CMakeFiles/libMCtop.dir/SpecFun.F90.i

lib/CMakeFiles/libMCtop.dir/SpecFun.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/libMCtop.dir/SpecFun.F90.s"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/lib/SpecFun.F90 -o CMakeFiles/libMCtop.dir/SpecFun.F90.s

lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.requires:

.PHONY : lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.requires

lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.provides: lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.requires
	$(MAKE) -f lib/CMakeFiles/libMCtop.dir/build.make lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.provides.build
.PHONY : lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.provides

lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.provides.build: lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o


lib/CMakeFiles/libMCtop.dir/Vegas.F90.o: lib/CMakeFiles/libMCtop.dir/flags.make
lib/CMakeFiles/libMCtop.dir/Vegas.F90.o: ../lib/Vegas.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object lib/CMakeFiles/libMCtop.dir/Vegas.F90.o"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/vmateu/GitHub/MCtop/lib/Vegas.F90 -o CMakeFiles/libMCtop.dir/Vegas.F90.o

lib/CMakeFiles/libMCtop.dir/Vegas.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/libMCtop.dir/Vegas.F90.i"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/vmateu/GitHub/MCtop/lib/Vegas.F90 > CMakeFiles/libMCtop.dir/Vegas.F90.i

lib/CMakeFiles/libMCtop.dir/Vegas.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/libMCtop.dir/Vegas.F90.s"
	cd /Users/vmateu/GitHub/MCtop/build/lib && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/vmateu/GitHub/MCtop/lib/Vegas.F90 -o CMakeFiles/libMCtop.dir/Vegas.F90.s

lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.requires:

.PHONY : lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.requires

lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.provides: lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.requires
	$(MAKE) -f lib/CMakeFiles/libMCtop.dir/build.make lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.provides.build
.PHONY : lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.provides

lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.provides.build: lib/CMakeFiles/libMCtop.dir/Vegas.F90.o


# Object files for target libMCtop
libMCtop_OBJECTS = \
"CMakeFiles/libMCtop.dir/MCtop.F90.o" \
"CMakeFiles/libMCtop.dir/Mathlink.F90.o" \
"CMakeFiles/libMCtop.dir/MatrixElements.F90.o" \
"CMakeFiles/libMCtop.dir/SpecFun.F90.o" \
"CMakeFiles/libMCtop.dir/Vegas.F90.o"

# External object files for target libMCtop
libMCtop_EXTERNAL_OBJECTS =

lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/MCtop.F90.o
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/Vegas.F90.o
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/build.make
lib/liblibMCtop.a: lib/CMakeFiles/libMCtop.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/vmateu/GitHub/MCtop/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Linking Fortran static library liblibMCtop.a"
	cd /Users/vmateu/GitHub/MCtop/build/lib && $(CMAKE_COMMAND) -P CMakeFiles/libMCtop.dir/cmake_clean_target.cmake
	cd /Users/vmateu/GitHub/MCtop/build/lib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/libMCtop.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
lib/CMakeFiles/libMCtop.dir/build: lib/liblibMCtop.a

.PHONY : lib/CMakeFiles/libMCtop.dir/build

lib/CMakeFiles/libMCtop.dir/requires: lib/CMakeFiles/libMCtop.dir/MCtop.F90.o.requires
lib/CMakeFiles/libMCtop.dir/requires: lib/CMakeFiles/libMCtop.dir/Mathlink.F90.o.requires
lib/CMakeFiles/libMCtop.dir/requires: lib/CMakeFiles/libMCtop.dir/MatrixElements.F90.o.requires
lib/CMakeFiles/libMCtop.dir/requires: lib/CMakeFiles/libMCtop.dir/SpecFun.F90.o.requires
lib/CMakeFiles/libMCtop.dir/requires: lib/CMakeFiles/libMCtop.dir/Vegas.F90.o.requires

.PHONY : lib/CMakeFiles/libMCtop.dir/requires

lib/CMakeFiles/libMCtop.dir/clean:
	cd /Users/vmateu/GitHub/MCtop/build/lib && $(CMAKE_COMMAND) -P CMakeFiles/libMCtop.dir/cmake_clean.cmake
.PHONY : lib/CMakeFiles/libMCtop.dir/clean

lib/CMakeFiles/libMCtop.dir/depend:
	cd /Users/vmateu/GitHub/MCtop/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/vmateu/GitHub/MCtop /Users/vmateu/GitHub/MCtop/lib /Users/vmateu/GitHub/MCtop/build /Users/vmateu/GitHub/MCtop/build/lib /Users/vmateu/GitHub/MCtop/build/lib/CMakeFiles/libMCtop.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : lib/CMakeFiles/libMCtop.dir/depend

