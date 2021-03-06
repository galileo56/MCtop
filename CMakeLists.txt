cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

project(MCtop C Fortran)

###############
## REQUIREMENTS & SETUP

## check whether this is a linux system
if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(OperatingSystem "Linux")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
	set(OperatingSystem "Mac OS X")
endif()
message(STATUS "Operating system: ${OperatingSystem}")

## set correct RPath for external (dynamic) libraries
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH true)

## find mathematica on the system
## Sascha Kratky's FindMathematica cmake module: https://github.com/sakra/FindMathematica/ [accessed 2015-03-06 01:00 AM]
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/tools")
find_package(Mathematica REQUIRED)

#FIND_PACKAGE(OpenMP REQUIRED)

###############
## COMPILER FLAGS

## additional flags for c++ compiler - for c++ and clang++ (should also take effect for the C compiler)
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(addition_cxx "-std=c++11")
    # set(addition_cxx "-std=c++11  ${OpenMP_CXX_FLAGS} -fopenmp")
# elseif ("" ... "") ## additional flags for other compilers
endif()

## additional flags for the fortran compiler - for gfortran
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")

  set(addition_fortran "-Waliasing -Wampersand -Wconversion -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Winteger-division -Wreal-q-constant -Wunused")
# set(addition_fortran "-Waliasing -Wampersand -Wconversion -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Winteger-division -Wreal-q-constant -Wunused -ffixed-line-length-none ${OpenMP_Fortran_FLAGS} ")

endif()

## message("cxx ${CMAKE_CXX_COMPILER_ID} c ${CMAKE_C_COMPILER_ID} OpenMP ${OpenMP_C_FLAGS}")

## let the flag changes take effect
set(CMAKE_CXX_FLAGS "${addition_cxx}" CACHE STRING "Flags used by the compiler during all build types." FORCE)
set(CMAKE_Fortran_FLAGS "${addition_fortran}" CACHE STRING "Flags used by the compiler during all build types." FORCE)
# set(CMAKE_C_FLAGS "-fopenmp" CACHE STRING "Flags used by the compiler during all build types." FORCE)

###############
## LIBS

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory(lib)

include_directories(${Library_MODDIR})
FILE(GLOB libSources lib/*.F90)
add_library(subdir_mods ${libSources})

###############
## EXE & LINKING & INSTALL

message(STATUS "Checking for mathlink library")
if(Mathematica_MathLink_LIBRARY)
	message(STATUS "Checking for mathlink library -- done")
	include_directories(${Mathematica_MathLink_INCLUDE_DIR})
	find_library(M_LIB m)
	find_library(DL_LIB dl)

	if("${OperatingSystem}" STREQUAL "Linux")
		find_library(UUID_LIB uuid)
		find_library(RT_LIB rt)
	endif()

	## build mathlink
	execute_process(COMMAND ${Mathematica_MathLink_MPREP_EXECUTABLE} -o ${CMAKE_CURRENT_SOURCE_DIR}/src/MCtop.tm.c ${CMAKE_CURRENT_SOURCE_DIR}/src/MCtop.tm OUTPUT_QUIET)

	add_executable(MCtop src/MCtop.tm.c)
	target_link_libraries(MCtop libMCtop ${Mathematica_MathLink_LIBRARY} ${M_LIB} ${DL_LIB} ${UUID_LIB} ${RT_LIB})

	else(Mathematica_MathLink_LIBRARY)
	message(STATUS "Checking for mathlink library -- not found, proceeding without mathlink")
endif()

	add_executable(ThrustLegendre src/ThrustLegendre.F90)
	add_executable(Stable src/Stable.F90)
	add_executable(Unstable src/Unstable.F90)
	add_executable(ThrustDistro src/ThrustDistro.F90)
	target_link_libraries(ThrustLegendre libMCtop subdir_mods)
	target_link_libraries(Stable libMCtop subdir_mods)
	target_link_libraries(Unstable libMCtop subdir_mods)
	target_link_libraries(ThrustDistro libMCtop subdir_mods)

###############
## INSTALL

if(Mathematica_MathLink_LIBRARY)
	install(TARGETS MCtop DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin)

	if("${OperatingSystem}" STREQUAL "Mac OS X")
		install(CODE "execute_process(COMMAND install_name_tool -change @executable_path/../Frameworks/mathlink.framework/Versions/${Mathematica_MathLink_VERSION}/mathlink ${Mathematica_MathLink_LIBRARY}/mathlink ${CMAKE_CURRENT_SOURCE_DIR}/bin/MCtop OUTPUT_QUIET)")
		install(CODE "execute_process(COMMAND install_name_tool -change @executable_path/../Frameworks/mathlink.framework/Versions/4.36/mathlink ${Mathematica_MathLink_LIBRARY}/mathlink ${CMAKE_CURRENT_SOURCE_DIR}/bin/MCtop OUTPUT_QUIET)")
	endif()
endif()

install(TARGETS Stable DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin)
install(TARGETS Unstable DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin)
install(TARGETS ThrustLegendre DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin)
install(TARGETS ThrustDistro DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin)
