cmake_minimum_required (VERSION 2.8)

project (main Fortran)


include_directories("/Users/hwy_pro/.packman/netcdf/c_4.3.3.1_cxx_4.2.1_fortran_4.4.2/1/include")
link_directories("/Users/hwy_pro/.packman/netcdf/c_4.3.3.1_cxx_4.2.1_fortran_4.4.2/1/lib")

set (source_directories
    "${PROJECT_SOURCE_DIR}/src"
)

foreach (dir ${source_directories})
    include_directories ("${dir}")
    file (GLOB code "${dir}/*.F90")
    list (APPEND sources ${code})
endforeach ()

add_executable (main ${sources})
target_link_libraries (main netcdf netcdff)
