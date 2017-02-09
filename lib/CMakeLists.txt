FILE(GLOB libSources *.F90)
add_library(libMCtop STATIC ${libSources})
install(TARGETS libMCtop DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../lib)
