# Enforce C/C++ standard level
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enforce to disable any compiler-specific extensions
set(CMAKE_C_EXTENSIONS NO)
set(CMAKE_CXX_EXTENSIONS NO)

# Flags that apply to both C and C++
set(COMMON_WARN_FLAGS "${COMMON_WARN_FLAGS} -pedantic -Wall -Wextra \
-Wcast-align -Wcast-qual -Wconversion -Wdisabled-optimization -Wdouble-promotion -Wduplicated-cond \
-Wduplicated-branches -Wfloat-equal -Wformat=2 -Winit-self -Winline -Winvalid-pch -Wlogical-op \
-Wmissing-declarations -Wmissing-format-attribute -Wmissing-include-dirs -Wno-unused -Wnull-dereference -Wodr \
-Wpointer-arith -Wredundant-decls -Wrestrict -Wshadow -Wsign-conversion -Wstrict-overflow=5 -Wswitch-default \
-Wswitch-enum -Wwrite-strings -Wundef -Wuninitialized -Wunreachable-code")

# Flags that apply only to C OR C++
set(C_WARN_FLAGS "${COMMON_WARN_FLAGS} -Wbad-function-cast -Wmissing-prototypes -Wnested-externs \
-Wold-style-definition -Wstrict-prototypes")
set(CXX_WARN_FLAGS "${COMMON_WARN_FLAGS} -Wctor-dtor-privacy -Wnoexcept -Wold-style-cast -Woverloaded-virtual \
-Wsign-promo -Wstrict-null-sentinel -Wuseless-cast -Wzero-as-null-pointer-constant")

# Pass them back to the CMake variable
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_WARN_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_WARN_FLAGS}")

if (LOG_VERBOSE)
    message("-------------Warning Flags--------------")
    message(STATUS "C   : ${C_WARN_FLAGS}")
    message(STATUS "CXX : ${CXX_WARN_FLAGS}")
endif ()
