set(MODE 0) # 0 for DEBUG; 1 for RELEASE

cmake_minimum_required(VERSION 3.20)
project(no LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(${MODE} EQUAL 0)
    # Enable warnings
    add_compile_options(-O0 -g3)
    add_compile_options(
        -pedantic                   # Warn  : if ISO C99 standards are not followed
        -pedantic-errors            # Kill  : if ISO C99 standards are not followed
        -Wshadow                    # Warn  : if variables are shadowed
        -Wformat=2                  # Warn  : if string formats are misused
        -Wunreachable-code          # Warn  : if dead code can not be reached
        -Wunused-parameter          # Warn  : if arguments are not used
        -Wunused-variable           # Warn  : if variables are not used
        -Wswitch-default            # Warn  : if default case is not provided in swtich statement
        -Wswitch-enum               # Warn  : if all enum cases are not checked
        -Wextra
        -Wall
    )
    # Enable sanitizers
    add_compile_options(
        -fsanitize=undefined        # Enable undefined sanitizers
        -fsanitize=address          # Enable address sanitizers to find memory bugs
        -fsanitize=leak             # Enable address sanitizers to find memory leaks
        -fno-omit-frame-pointer     # Preserve frame pointer to enable better debugging
    )
    add_link_options(
        -fsanitize=undefined        # Enable undefined sanitizers
        -fsanitize=address          # Enable address sanitizers to find memory bugs
        -fsanitize=leak             # Enable address sanitizers to find memory leaks
    )
else()
    add_compile_options(-O3)        # Enable maximum optimizations!
endif()
# Disable annoying warnings
add_compile_options(-Wno-unused-parameter)
add_compile_options(-Wno-unused-function)

# Compile the compiler
file(GLOB_RECURSE SOURCES "src/*.c")
add_executable(no ${SOURCES})
