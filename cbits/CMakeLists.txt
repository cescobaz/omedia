cmake_minimum_required(VERSION 3.10)

project(oimage)

# https://stackoverflow.com/questions/29191855/what-is-the-proper-way-to-use-pkg-config-from-cmake
find_package(PkgConfig REQUIRED)
pkg_check_modules(VIPS REQUIRED IMPORTED_TARGET vips)

add_library(oimage image_vips.c)
target_link_libraries(oimage PkgConfig::VIPS)
