set(MODE 0) # 0 for DEBUG; 1 for RELEASE

cmake_minimum_required(VERSION 3.20)
project(no LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(${MODE} EQUAL 0)
    # Enable warnings
    add_compile_options(-O0 -g2)
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
else()
    add_compile_options(-O3)        # Enable maximum optimizations!
endif()
# Disable annoying warnings
add_compile_options(-Wno-unused-parameter)
add_compile_options(-Wno-unused-function)
add_compile_options(-Wno-unused-variable)
add_compile_options(-Wno-switch-enum)
add_compile_options(-Wno-switch)

# Optimizations
add_compile_options(-fvisibility=hidden)    # Hide everything
add_compile_options(-pipe)                  # Enable RAM storage

# Compile the code
file(GLOB_RECURSE SOURCES
    "src/ast/*.c"
    "src/parse/*.c"
    "src/*.c"
)
add_executable(no ${SOURCES})
