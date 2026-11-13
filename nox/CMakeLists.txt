option(MODE "DEBUG(0) or RELEASE(1) build mode" 0) # 0 for DEBUG; 1 for RELEASE

cmake_minimum_required(VERSION 3.20)
project(no LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(${MODE} EQUAL 0)
    message(STATUS "Building in DEBUG mode! (MODE is set to: ${MODE})")
    # Enable warnings
    add_compile_options(-O0 -g2)
    add_compile_options(
        -pedantic                   # Warn  : if ISO C99 standards are not followed
        -pedantic-errors            # Kill  : if ISO C99 standards are not followed
        -Wshadow                    # Warn  : if variables are shadowed
        -Wformat=2                  # Warn  : if string formats are misused
        -Wunreachable-code          # Warn  : if dead code can not be reached
        -Wswitch-default            # Warn  : if default case is not provided in swtich statement
        -Wextra
        -Wall
    )
    # Enable sanitizers
    add_compile_options(
        -fsanitize=undefined        # Enable sanitizers
        -fsanitize=address          # Enable sanitizers to find memory bugs
        -fsanitize=leak             # Enable sanitizers to find memory leaks
        -fsanitize=bounds           # Enable sanitizers to find access out of bounds
        -fno-omit-frame-pointer     # Preserve frame pointer to enable better debugging
    )
    add_link_options(
        -fsanitize=undefined        # Enable sanitizers
        -fsanitize=address          # Enable sanitizers to find memory bugs
        -fsanitize=leak             # Enable sanitizers to find memory leaks
        -fsanitize=bounds           # Enable sanitizers to find access out of bounds
    )
endif()

if(${MODE} EQUAL 1)
    message(STATUS "Building in RELEASE mode! (MODE is set to: ${MODE})")
    add_compile_options(-O2)        # Enable safe amount of optimizations!
    add_compile_options(-fstack-protector-strong) # Insert safety checks for stack buffer overflows!
    add_compile_options(-static)    # Build a statically linked executable!
    add_link_options(-static)       # Don't forget to link it statically!
    add_link_options(-no-pie)       # Don't generate PIE executable!
endif()

# Disable annoying C++ garbage that makes our lives living hell!
add_compile_options(-fno-exceptions)# Disable exceptions (Crash on spot instead!)
add_compile_options(-fno-rtti)      # Disable runtime type information (blegh!)
add_compile_options(-nostdlib)      # Disable C++ STL library

# Disable annoying warnings
add_compile_options(-Wno-unused-parameter)
add_compile_options(-Wno-unused-function)
add_compile_options(-Wno-unused-but-set-variable)
add_compile_options(-Wno-unused-variable)
add_compile_options(-Wno-switch-enum)
add_compile_options(-Wno-switch)
add_compile_options(-Wno-missing-field-initializers)

# Optimizations
add_compile_options(-fvisibility=hidden)    # Hide everything
add_compile_options(-pipe)                  # Enable RAM storage

# Compile the code as a library
file(GLOB_RECURSE SOURCES
    "lib/core/*.cc"
)
add_library(nox STATIC ${SOURCES})

# Compile the compiler
add_executable(no "main.cc")
target_link_libraries(no PRIVATE nox)
