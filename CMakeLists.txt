cmake_minimum_required(VERSION 3.10)
project(static_ring_vector VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

add_library(static_ring_vector INTERFACE)

target_include_directories(
        static_ring_vector
        INTERFACE
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}>"
)

set(config_install_dir "lib/cmake/${PROJECT_NAME}")
set(include_install_dir "include")

set(generated_dir "${CMAKE_CURRENT_BINARY_DIR}/generated")

set(version_config "${generated_dir}/${PROJECT_NAME}ConfigVersion.cmake")
set(project_config "${generated_dir}/${PROJECT_NAME}Config.cmake")
set(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
set(namespace "sunstone_rtls::")

include(CMakePackageConfigHelpers)

write_basic_package_version_file(
        "${version_config}"
        VERSION "${PROJECT_VERSION}"
        COMPATIBILITY
        SameMajorVersion
)

configure_package_config_file(
        "${CMAKE_CURRENT_LIST_DIR}/Config.cmake.in"
        "${project_config}"
        INSTALL_DESTINATION "${config_install_dir}"
)

install(
        TARGETS static_ring_vector
        EXPORT "${TARGETS_EXPORT_NAME}"
        LIBRARY DESTINATION "lib"
        ARCHIVE DESTINATION "lib"
        RUNTIME DESTINATION "bin"
        INCLUDES DESTINATION "${include_install_dir}"
        PUBLIC_HEADER DESTINATION "${include_install_dir}"
        BUNDLE DESTINATION "bin"
)


install(
        FILES "${project_config}" "${version_config}"
        DESTINATION "${config_install_dir}"
)

install(
        EXPORT "${TARGETS_EXPORT_NAME}"
        NAMESPACE "${namespace}"
        DESTINATION "${config_install_dir}"
)

install(DIRECTORY "${PROJECT_SOURCE_DIR}/sunstone_rtls" DESTINATION "${include_install_dir}")
