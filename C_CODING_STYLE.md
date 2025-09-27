<!--
 Copyright 2025 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM C Coding Style Guide (AVMCCS Guide)

**Version**: 1.0

## Overview

This document establishes coding conventions for modern C projects, emphasizing clarity, consistency, and best practices. The style prioritizes readability and maintainability while leveraging C's type system for enhanced safety and expressiveness.

Each rule has a unique identifier (e.g., AVMCCS-L001) for easy reference in reviews and tooling.

This guide uses US English spelling conventions (e.g., "Color" not "Colour", "initialize" not "initialise").

**Note**: This guide complements but does not replace domain-specific standards. For safety-critical systems, applicable standards like MISRA C take precedence. This guide's purpose focuses on coding style and common practices rather than comprehensive coverage of all C programming topics.

**Note on examples**: Code snippets throughout this guide are simplified to illustrate specific concepts. They may omit elements required by other rules (module prefixes, static keywords, error handling, etc.) for clarity. Always apply all relevant rules when writing production code.

### Modern C Practices

This guide assumes C11 or later (C99 at minimum):

- **Declare variables where needed**, not just at function start [AVMCCS-L001]

  ```c
  // Good: Declare at first use
  for (int i = 0; i < n; i++) { ... }
  const char *name = get_name();

  // Old style: Avoid
  int i;
  const char *name;
  /* ... other code ... */
  for (i = 0; i < n; i++) { ... }
  ```

- **Empty parameter lists**: Use `(void)` for functions with no parameters [AVMCCS-L002]

  ```c
  // Good: Explicit zero parameters (type-safe)
  int get_count(void);
  void initialize(void);

  // Also use same style for function definitions, for improved consistency
  int foo(void)
  {
      return 42;
  }

  // Bad: Old K&R style - unspecified parameters (dangerous)
  int get_count();     // Accepts ANY number of arguments!
  void initialize();   // No type checking at call site
  ```

  **Safety note**: In C99/C11, `()` means "unspecified parameters" and disables type checking. The compiler won't catch errors like `get_count(42, "wrong")`. Always use `(void)` for zero-parameter functions.

  **C23 update**: C23 changes `()` to mean the same as `(void)`. However, for compatibility with C99/C11/C17, continue using `(void)`.

- **Use modern headers**: `<stdint.h>`, `<stdbool.h>`, etc. [AVMCCS-L003]

## Indentation

### Style Rules

- **K&R style variant**: [AVMCCS-F001]
  - Opening braces on new line for functions, structs, enums, and unions
  - Opening braces on same line for flow control (if, for, while, switch)
- **Mandatory braces**: Always use braces, even for single-statement blocks [AVMCCS-F002]
- **4 spaces**: Use 4 space indentation (no tabs) [AVMCCS-F003]

Good:

```c
typedef struct
{
    int x;
    int y;
} Point;

void f(bool reverse)
{
    if (reverse) {
        puts("!dlroW olleH");
    } else {
        puts("Hello World");
    }
}
```

Bad:

```c
typedef struct {
    int x;
    int y;
} Point;

void f(bool reverse) {
    if (reverse)
        puts("!dlroW olleH");
    else
        puts("Hello World");
}
```

## Formatting

### General Rules

- **Pointer placement**: The `*` belongs with the variable name, not the type [AVMCCS-F004]

  ```c
  char *name;         // Good
  char* name;         // Bad
  ```

- **Variable declarations**: Generally declare one variable per line [AVMCCS-F005]

  ```c
  // Preferred: One declaration per line
  char *first;
  char *last;
  int count;

  // Acceptable for tightly coupled variables
  int i, j, k;        // Loop indices
  char *src, *dst;    // Paired pointers

  // Avoid mixing types or unrelated variables
  int x, *ptr, arr[10];  // Bad: confusing
  ```

- **Line length**: Keep lines below 100 columns [AVMCCS-F006]
  - Use intermediate variables with meaningful names to break up complex expressions [AVMCCS-F007]
  - Split long function calls across multiple lines

- **Function spacing**: Separate function definitions with exactly one empty line [AVMCCS-F008]

  ```c
  void first_function(void)
  {
      // implementation
  }

  void second_function(void)
  {
      // implementation
  }
  ```

- **Readability over brevity**: Prefer clear, self-documenting code

  ```c
  // Good: Clear intermediate variables
  DateTime current_time = get_current_time();
  DateTime deadline = project_deadline();
  int64_t seconds_remaining = date_time_diff_seconds(&current_time, &deadline);

  // Bad: Hard to read one-liner
  int64_t remaining = date_time_diff_seconds(&get_current_time(), &project_deadline());
  ```

### Whitespace Rules

- **No trailing whitespace**: Lines must not end with spaces or tabs [AVMCCS-F009]
- **Files end with newline**: All source files must end with a newline character [AVMCCS-F010]
- **Clean git commits**: Configure your editor to remove trailing whitespace

### Empty Lines and Spacing

- **Return statements**: Separate with empty line when function body > 3 lines [AVMCCS-F011]

  ```c
  // Good: Empty line before return for clarity
  int calculate_result(void)
  {
      process_data();
      validate_state();
      update_cache();

      return 42;
  }

  // Bad: No separation
  int calculate_result(void)
  {
      process_data();
      validate_state();
      update_cache();
      return 42;
  }
  ```

- **Logical blocks**: Use empty lines to separate steps within functions [AVMCCS-F012]

  ```c
  // Good: Clear logical sections
  void process_request(Request *req)
  {
      // Validation phase
      if (!validate_request(req)) {
          return;
      }
      check_permissions(req->user);

      // Processing phase
      normalize_data(req->data);
      apply_transformations(req);

      // Storage phase
      save_to_database(req);
      update_cache(req);
      send_notification(req->user);
  }
  ```

- **Control flow blocks**: Compact style for short blocks, spaced for longer ones [AVMCCS-F013]

  ```c
  // Good: Compact for short blocks
  if (cond1) {
      action1();
  } else if (cond2) {
      action2();
  } else {
      action3();
  }

  // Good: Spaced for longer blocks (>3 lines)
  if (cond1) {
      setup();
      process();
      validate();
      cleanup();

  } else if (cond2) {
      alternative_action();

  } else {
      default_handler();
  }
  ```

- **Switch statements**: Separate cases with empty lines when > 3 lines per case [AVMCCS-F014]

  ```c
  switch (command) {
      case CMD_PROCESS:
          validate_input();
          transform_data();
          store_results();
          log_completion();

      case CMD_RESET:
          clear_buffers();
          reset_state();
          reinitialize();
          notify_listeners();

      default:
          log_error("Unknown command");
          break;
  }
  ```

### Spacing

- **Control structures**: Space after keywords, before opening brace [AVMCCS-F015]

  ```c
  // Good
  if (condition) {
  while (running) {
  for (int i = 0; i < n; i++) {

  // Bad
  if(condition){     // Missing spaces
  while(running){    // Missing spaces
  ```

- **Function calls**: No space between function name and parentheses [AVMCCS-F016]

  ```c
  // Good
  printf("Hello");
  int result = calculate(x, y);

  // Bad
  printf ("Hello");  // Extra space
  calculate (x, y);  // Extra space
  ```

- **Comments**: Space after comment markers [AVMCCS-F017]

  ```c
  // Good: This is a comment
  /* Good: Block comment */

  //Bad: Missing space
  /*Bad: Missing space*/
  ```

- **Casts**: Space after closing parenthesis [AVMCCS-F018]

  ```c
  // Good
  int *ptr = (int *) &value;
  char c = (char) integer;

  // Bad
  int *ptr = (int *)&value;   // Missing space
  char c = (char)integer;     // Missing space
  ```

- **Hexadecimal numbers**: Use uppercase letters [AVMCCS-F019]

  ```c
  // Good
  uint32_t mask = 0xDEADBEEF;
  uint16_t flags = 0xCAFE;

  // Bad
  uint32_t mask = 0xdeadbeef;  // Use uppercase
  uint16_t flags = 0xcafe;     // Use uppercase
  ```

- **Binary operators**: Use spaces around binary operators [AVMCCS-F020]

  ```c
  // Good
  int sum = a + b;
  if (x == y && z > 0) {
      result = (a * b) / c;
  }

  // Bad
  int sum = a+b;           // Missing spaces
  if (x==y&&z>0) {         // Unreadable
      result = (a*b)/c;    // Cramped
  }
  ```

- **Initializer braces**: Always place on the same line as the declaration [AVMCCS-F021]

  ```c
  // Good: Braces on same line for initializers
  int values[] = {1, 2, 3, 4, 5};
  static const char *const names[] = {
      "Alice", "Bob", "Charlie"
  };

  // Bad: Don't use new line for initializers
  int values[] =
  {
      1, 2, 3, 4, 5
  };
  ```

  Note: This differs from struct/function definitions which use new-line braces. Initializers are value assignments, not type definitions.

## Language Constructs

### Declarations

#### Declaration Order [AVMCCS-L004]

Follow consistent order for declaration specifiers:

**Functions**: `[storage] [function-specifier] [return-type]`

```c
// Good: Consistent order
static inline void helper(void);
static inline void fast_helper(void);
extern void public_api(void);
static inline const char *get_name(void);
static void process_data(void);
inline void fast_operation(void);

// Bad: Inconsistent order
inline static void helper(void);      // Wrong: inline before static
void static process_data(void);        // Wrong: static after void
const static char *get_name(void);     // Wrong: const before static
```

**Variables**: `[storage] [type-qualifier] [type]`

```c
// Good: Consistent order
static const int max_value = 100;
extern volatile sig_atomic_t signal_flag;
static _Thread_local int thread_counter;
static const char *const messages[] = { "Hello", "World" };
extern const size_t buffer_size;
static volatile uint32_t *hardware_reg;

// Bad: Inconsistent order
const static int max_value = 100;           // Wrong: const before static
volatile extern sig_atomic_t signal_flag;    // Wrong: volatile before extern
const char static *const messages[] = {};    // Wrong: static in middle
```

**Additional specifiers**:
- `restrict`: Use for pointer parameters that don't alias [AVMCCS-L005]

  ```c
  void copy_buffer(char *restrict dest, const char *restrict src, size_t n);
  ```

- `_Atomic`: For atomic operations (C11) [AVMCCS-L006]

  ```c
  _Atomic int counter = ATOMIC_VAR_INIT(0);
  ```

- `volatile`: For hardware registers or signal handlers [AVMCCS-L007]

  ```c
  volatile uint32_t *hardware_register = (volatile uint32_t *) 0x40000000;
  ```

### Operators

#### Increment/Decrement Usage [AVMCCS-L008]

Prefer post-increment (`i++`) over pre-increment (`++i`):

```c
// Good: Idiomatic C style
for (int i = 0; i < n; i++) { ... }

// Avoid using increment/decrement for their return values
// Bad: Confusing
int b = ++a;
array[++index] = value;

// Good: Separate statements for clarity
a++;
int b = a;

// Exception: Idiomatic patterns are OK
while (*dst++ = *src++) { }  // String copy idiom
```

**Note**: This is a stylistic preference for readability and C idiom consistency. For primitive types like `int`, modern compilers generate identical code for `i++` and `++i` when the return value is not used (this is true for C with an optimized release build). The post-increment form `i++` is more commonly seen in C code and reads more naturally. This guide chooses `i++` for consistency with established C conventions.

### Control Flow

#### Early Exit Over Nesting [AVMCCS-L009]

Use guard clauses to reduce indentation:

```c
// Good: Early exit pattern
int process_value(int n)
{
    if (n < 0) {
        return -1;
    }

    // Main logic with reduced nesting
    compute_result(n);
    return n * 2;
}

// Bad: Unnecessary nesting
int process_value(int n)
{
    if (n >= 0) {
        compute_result(n);
        return n * 2;
    }
    return -1;
}
```

#### Switch Over If-Else Chains [AVMCCS-L010]

Use switch when testing one variable for 3+ values:

```c
// OK: Two comparisons
if (state == 1) {
    handle_init();
} else if (state == 2) {
    handle_running();
} else {
    handle_error();
}

// Bad: Should use switch for 3+ cases
if (state == 1) {
    handle_init();
} else if (state == 2) {
    handle_running();
} else if (state == 3) {
    handle_shutdown();
} else {
    handle_error();
}

// Good: Switch for multiple cases
switch (state) {
    case 1:
        handle_init();
        break;
    case 2:
        handle_running();
        break;
    case 3:
        handle_shutdown();
        break;
    default:
        handle_error();
        break;
}
```

#### Goto for Cleanup [AVMCCS-L011]

Use `goto` for clean error handling and resource cleanup:

```c
// Good: Initialize pointers to NULL for clean cleanup
parse_config_result_t parse_config(const char *path, Config **out)
{
    FILE *file = fopen(path, "r");
    if (!file) {
        return ParseConfigFileOpenFailed;
    }

    char *buffer = NULL;
    Config *config = NULL;
    parse_config_result_t result = ParseConfigOk;

    buffer = malloc(BUFFER_SIZE);
    if (!buffer) {
        result = ParseConfigOutOfMemory;
        goto cleanup;
    }

    config = calloc(1, sizeof(Config));
    if (!config) {
        result = ParseConfigOutOfMemory;
        goto cleanup;
    }

    // ... parsing logic ...

    *out = config;
    config = NULL;  // Transfer ownership

cleanup:
    free(config);   // Safe: free(NULL) is a no-op
    free(buffer);
    fclose(file);
    return result;
}
```

**Important**: Use `goto` only for well-established patterns:
- Error handling and resource cleanup (shown above)
- Breaking out of deeply nested loops when refactoring isn't practical
- State machines in performance-critical code

Never use `goto` for general flow control, jumping backwards, or creating spaghetti code. The cleanup pattern works because it follows strict rules: labels at the end, only jumping forward, and maintaining clear resource ownership. If you're unsure whether your `goto` usage is appropriate, refactor to use functions or structured control flow instead.

### Static Assertions

Use `_Static_assert` for compile-time validation:

```c
// Verify struct sizes and alignments
_Static_assert(sizeof(DateTime) == 24, "DateTime size changed");
_Static_assert(offsetof(Packet, data) == 8, "Packet layout assumption violated");

// Verify configuration consistency
_Static_assert(MAX_BUFFER_SIZE >= MIN_BUFFER_SIZE, "Buffer size configuration error");

// Type size assumptions
_Static_assert(sizeof(int) == 4, "Code assumes 32-bit int");

// Verify enum values don't overflow storage
_Static_assert(ConnectionStateMax <= 255, "connection_state_t values exceed uint8_t");
```

Where to use:
- After type definitions to verify layout assumptions [AVMCCS-L012]
- In implementation files to verify platform assumptions [AVMCCS-L013]
- To ensure configuration constants are consistent [AVMCCS-L014]

### Macro Safety

Macros require special care to avoid common pitfalls:

- Always parenthesize macro parameters [AVMCCS-L015]
- Parenthesize the entire macro expression [AVMCCS-L016]
- Use `do { ... } while (0)` for multi-statement macros [AVMCCS-L017]
- Avoid expressions with side effects as macro arguments [AVMCCS-L018]
- Protect against variable shadowing in complex macros [AVMCCS-L019]

```c
// Good: All parameters parenthesized
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

// Bad: Unprotected parameters
#define MAX(a, b) (a > b ? a : b)     // Wrong: MAX(x&1, y) breaks
#define SQUARE(x) x * x               // Wrong: SQUARE(a+b) = a+b*a+b

// Watch for side effects in macro usage:
int x = 5;
int bad = MAX(x++, 10);    // Bug: x may increment twice
int good = MAX(x, 10);     // Safe: no side effects
x++;                       // Increment separately if needed

// Good: Multi-statement macros use do-while
#define SWAP(a, b) do { \
    typeof(a) tmp = (a); \
    (a) = (b); \
    (b) = tmp; \
} while (0)

// Bad: No do-while protection
#define SWAP(a, b) { \
    typeof(a) tmp = (a); \
    (a) = (b); \
    (b) = tmp; \
}
// This breaks: if (x) SWAP(x, y); else ...

// Note: typeof was a GCC extension, now part of the standard since C23
```

**Side Effects Warning**: Function-like macros evaluate arguments multiple times

```c
// WARNING: Function-like macros evaluate arguments multiple times
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

// Dangerous: Side effects happen multiple times!
int y = MAX(x++, 10);                          // x incremented twice if x > 10
int z = MIN(expensive_function(), threshold);   // function called twice

// Safe: Use intermediate variables
int tmp = expensive_function();
int z = MIN(tmp, threshold);
```

**Variable Shadowing in Macros**:

```c
// Problem: Macro variables can shadow caller's variables
#define SWAP(a, b) do { \
    int tmp = (a);      \
    (a) = (b);          \
    (b) = tmp;          \
} while (0)

// If caller has 'tmp', unexpected behavior occurs:
int tmp = 42;
SWAP(x, tmp);  // Macro's 'tmp' shadows caller's 'tmp'

// For general-use macros in headers, use unique names:
#define SWAP(a, b) do { \
    typeof(a) SWAP_tmp_ = (a); \
    (a) = (b); \
    (b) = SWAP_tmp_; \
} while (0)

// Note: For local macros in .c files with controlled usage,
// simple readable names may be acceptable with careful review
```

**General Guidance**: Prefer inline functions over function-like macros when possible. Macros bypass type checking and can have surprising behavior with side effects. Use macros only when compile-time computation, token manipulation, or generic programming is essential. When macros are necessary, use them with careful consideration of their limitations.

## Naming Conventions

### Casing Rules

#### Word Boundary Preservation [AVMCCS-N001]

When converting between casing styles, preserve word boundaries with underscores:
- `DateTime` type → `date_time` module → `DATE_TIME` constants (not `DATETIME`)
- `buffer_size` → `BUFFER_SIZE` (not `BUFFERSIZE`)
- `max_retry_count` → `MAX_RETRY_COUNT` (not `MAXRETRYCOUNT`)

**Acronyms**: In PascalCase, keep acronyms capitalized. When converting to other cases, treat the acronym as a single word: [AVMCCS-N002]
- `CSVParser` (PascalCase) → `csv_parser` (snake_case) → `CSV_PARSER` (SCREAMING_SNAKE_CASE)
- `URLHandler` → `url_handler` → `URL_HANDLER`
- `IOManager` → `io_manager` → `IO_MANAGER`
- Never split acronyms: `C_S_V_PARSER` is wrong

This ensures consistency between related identifiers:

```c
// Type: DateTime (PascalCase, no underscores)
typedef struct { ... } DateTime;

// Module: date_time (preserves word boundary)
int64_t date_time_diff_seconds(...);

// Constants: DATE_TIME (preserves word boundary)
#define DATE_TIME_MAX_YEAR 9999
#define DATE_TIME_EPOCH 1970
```

#### PascalCase

- **Composite types**: Use PascalCase for non-scalar types (structs, unions) [AVMCCS-N003]

  ```c
  typedef struct
  {
      int year;
      int month;
      int day;
  } DateTime;
  ```

- **Enumeration values**: Use PascalCase for enum constants representing exclusive alternatives [AVMCCS-N004]

  ```c
  typedef enum
  {
      DigitalLow,
      DigitalHigh,
      DigitalReadFailed
  } digital_read_result_t;

  typedef enum
  {
      ConnectionDisconnected,
      ConnectionConnected,
      ConnectionAuthenticating,
      ConnectionAuthenticated
  } connection_state_t;
  ```

#### SCREAMING_SNAKE_CASE

- **Manifest constants**: All compile-time constants, regardless of definition method [AVMCCS-N005]

  ```c
  #define BUFFER_SIZE 1024
  #define PI 3.14159265359

  enum
  {
      MAX_FILENAME_LENGTH = 255,
      DEFAULT_TIMEOUT_MS = 5000
  };
  ```

- **Flag constants**: Bit flags for bitwise operations [AVMCCS-N006]

  ```c
  typedef enum
  {
      FONT_REGULAR    = 0x00,
      FONT_BOLD       = 0x01,
      FONT_ITALIC     = 0x02,
      FONT_UNDERLINE  = 0x04
  } font_style_t;

  // Usage: FONT_BOLD | FONT_ITALIC
  ```

#### lower_snake_case

- **Functions**: All function names [AVMCCS-N007]
- **Variables**: Local and global variables (including struct fields) [AVMCCS-N008]
- **Function parameters**: All parameter names [AVMCCS-N009]
- **Type aliases for scalars**: When creating scalar type aliases [AVMCCS-N010]

### Naming Patterns

#### Arrays and Collections

Clear distinction between arrays and other collection types prevents confusion:

```c
// Arrays: Use plural names with matching _len suffix
typedef struct
{
    int user_ids[MAX_USERS];      // Plural for array
    size_t user_ids_len;          // Length matches array name

    char *names[];                // Plural for array
    size_t names_len;             // Length matches array name
} ArrayExample;

// Usage is clear:
example->user_ids[0] = 42;       // Valid: plural indicates array
example->names[i] = strdup(name); // Valid: plural indicates array

// Non-array collections: Use descriptive names with type suffix
typedef struct
{
    List user_list;               // Not "users" - can't index directly
    Map name_to_id_map;          // Descriptive of contents
    Set permission_set;          // Clear it's a set
    Tree category_tree;          // Clear it's a tree
} CollectionExample;

// Usage makes type obvious:
list_append(&example->user_list, user);    // Can't do user_list[0]
map_get(&example->name_to_id_map, "Alice"); // Clearly a map operation

// Avoid ambiguous plurals for non-arrays
typedef struct
{
    List users;     // Bad: looks like array (users[0] seems valid)
    Set permissions; // Bad: looks like array
} BadExample;
```

**Key rules**:
- Arrays get plural names (`users[]`, `paths[]`) [AVMCCS-N011]
- Array lengths match the array name (`users_len`, `paths_len`) [AVMCCS-N012]
- Non-array collections get descriptive names with type suffix (`user_list`, `id_set`) [AVMCCS-N013]
- This prevents confusion about direct indexability

#### Conversion Functions [AVMCCS-N014]

- **`_to` / `_from`**: Use for infallible, straightforward type conversions

  ```c
  // Note: Examples show naming pattern only - in practice pass and return by pointer
  // See also: AVMCCS-A007

  // Module to other type
  hsv_t color_to_hsv(Color color);
  rgb_t color_to_rgb(Color color);

  // Other type to module
  Color color_from_hsv(hsv_t hsv);
  Color color_from_rgb(rgb_t rgb);
  ```

- **Requirements**:
  - Conversions must be infallible (cannot fail)
  - Destination type has trivial initialization (`_init` pattern or simpler)
  - No complex parsing or validation needed

- **Avoid `_to`/`_from` when**:
  - Conversion can fail (use `parse`, `decode`, etc.)
  - Destination requires `_create` or `_new` (implies resource management)
  - Complex validation or parsing is involved

#### Predicate Functions [AVMCCS-N015]

- **`_is` / `_has` / `_can`**: Return `bool` and take `const` pointers

  ```c
  // State queries
  bool date_time_is_valid(const DateTime *dt);
  bool buffer_is_empty(const Buffer *buffer);

  // Property queries
  bool string_has_spaces(const char *str);
  bool user_has_permission(const User *user, permission_t perm);

  // Capability queries
  bool text_input_can_undo(const TextInput *input);
  bool connection_can_send(const Connection *conn);
  ```

- **Naming pattern**: `module_predicate_property`
- **Always**: Take `const` pointers when checking object state
- **Return**: Only `bool` (use other patterns for error codes)

#### String and Buffer Naming [AVMCCS-N016]

- **`string`**: Always means NULL-terminated C string

  ```c
  char *int32_to_string(int32_t v);

  int32_to_string(42); // -> "42", that is {'4', '2', '\0'}
  ```

- **`_buf` suffix**: Non-NULL-terminated buffer (requires length)

  ```c
  // Buffer variants require explicit length
  size_t count_spaces_in_buf(const char buf[], size_t buf_len);
  ```

- **No `cstring` prefix/suffix**: Redundant - strings are C strings by default
- **Omit `string` when obvious**: `parse_iso8601(void)` not `parse_iso8601_string(void)`

  ```c
  // Default assumes string (NULL-terminated)
  size_t count_spaces(const char *str);
  bool starts_with(const char *str, const char *prefix);
  ```

- **Conversely**: `parse_iso8601_buf()` needs `_buf` suffix when parsing from a buffer

#### Module Prefixing [AVMCCS-N017]

Functions must be prefixed with their logical module name to establish clear namespacing:

```c
// DateTime module
int64_t date_time_diff_seconds(const DateTime *a, const DateTime *b);
bool date_time_is_valid(const DateTime *dt);
// ...

// Buffer utilities module
bool buffer_append(Buffer *buffer, const char *data);
void buffer_clear(Buffer *buffer);
// ...
```

#### Static Functions [AVMCCS-N018]

Static functions (file-local) should not use module prefixes since they don't pollute the global namespace:

```c
// In date_time.c
static bool is_date_valid(int year, int month, int day);
static int days_in_month(int month, int year);

// Public functions still use module prefix
bool date_time_is_valid(const DateTime *dt)
{
    return is_date_valid(dt->year, dt->month, dt->day);
}
```

**Naming pattern**: Consider verb-first naming for static helpers to distinguish from public APIs:

```c
// Public API: module prefix first
bool date_time_is_valid(const DateTime *dt);

// Static helper: verb first
static bool is_valid_date_time(const DateTime *dt);
static int get_days_in_month(int month, int year);
```

#### Macros

All macros use SCREAMING_SNAKE_CASE [AVMCCS-N019]:

```c
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#define DEFAULT_BUFFER_SIZE 1024
```

#### Type Suffix Convention [AVMCCS-N020]

- **Scalar type aliases**: Use `_t` suffix for type definitions representing scalar or opaque types

  ```c
  typedef int32_t user_id_t;
  typedef uint64_t timestamp_t;
  typedef void *handle_t;
  typedef struct FileImpl *file_handle_t;  // Opaque pointer
  typedef union {
      float f;
      int32_t i;
  } float_or_int32_t;  // Type-punning union
  ```

  **Note on POSIX compliance**: POSIX reserves the `_t` suffix for its own type definitions. However, this convention is widely adopted in C projects for its clarity. If strict POSIX compliance is required, consider project-specific prefixes. For most applications, the clarity benefit of `_t` outweighs the theoretical namespace conflict.

  **Semantic hint**: The `_t` suffix signals that a type is a scalar or pointer that can be efficiently passed by value, stored on the stack, and used directly without dynamic allocation. When you see `user_id_t id;`, you know it's safe to declare it as a local variable, pass it by value to functions, and return it by value - no `malloc()` needed. This convention helps distinguish simple value types from complex structures that require pointer passing and careful memory management.

- **Composite types**: No suffix required (already using PascalCase)

  ```c
  typedef struct
  {
      user_id_t id;
      char name[64];
  } User;  // Not User_t
  ```

#### Enumeration Namespace Management [AVMCCS-N021]

Enum values exist in the global namespace and can cause collisions. Use a common prefix derived from the enum type name to avoid conflicts:

```c
typedef enum
{
    DigitalLow,
    DigitalHigh,
    DigitalReadFailed
} digital_read_result_t;

typedef enum
{
    ConnectionDisconnected,
    ConnectionConnected,
    ConnectionAuthenticating,
    ConnectionAuthenticated
} connection_state_t;
```

#### Enum and Flag Naming

**Naming Convention**:
- Use singular names for exclusive state enums [AVMCCS-N022]
- Use singular + `_flags_t` suffix for combined flags types [AVMCCS-N023]

```c
// Exclusive states: use singular (only one at a time)
typedef enum {
    FileOpen,
    FileClosed,
    FileError
} file_state_t;  // Singular

// Combinable flags: enum is singular, flags type is plural
typedef enum {
    PERM_READ  = 0x01,
    PERM_WRITE = 0x02,
    PERM_EXEC  = 0x04
} permission_t;  // Singular for the enum

typedef uint32_t permission_flags_t;  // Singular + flags_t suffix
```

#### Measurement Units [AVMCCS-N024]

Include units in names to prevent errors and improve clarity:

```c
// Functions: unit suffix indicates primary parameter or return value units
void delay_ms(uint32_t milliseconds);
void set_timeout_ms(uint32_t timeout);  // Action on parameter
int64_t get_timestamp_us(void);             // Return value unit
float calculate_distance_km(Point a, Point b);

// Parameters: make units explicit
void screen_rotate_deg(int rotation_deg);
void motor_set_speed_rpm(int speed_rpm);

// No units needed when using enum types
void screen_set_orientation(screen_orientation_t orientation);

// Variables and constants: prevent unit confusion
const int MAX_SPEED_KMH = 120;
float temperature_celsius = 25.5;
uint32_t file_size_bytes = 1024;
```

Common unit suffixes:
- Time: `_ms`, `_us`, `_ns`, `_sec`, `_min`
- Distance: `_mm`, `_cm`, `_m`, `_km`
- Angles: `_deg`, `_rad`
- Data: `_bytes`, `_kib`, `_mib`, `_gib` (use binary prefixes for clarity)
- Frequency: `_hz`, `_khz`, `_mhz`
- Speed: `_rpm`, `_kmh`, `_mps`

**Note**: Bytes can be omitted for common memory operations where the unit is obvious:

```c
void *malloc(size_t size);          // Not malloc_bytes
buffer_create(&buf, 1024);          // Not buffer_create_bytes
#define MAX_BUFFER_SIZE 4096        // Not MAX_BUFFER_SIZE_BYTES
```

#### Size Terminology

Use consistent suffixes for counts and sizes [AVMCCS-N025]:
- **`len`/`length`**: Number of elements in arrays/buffers
- **`size`**: Number of bytes (as in `sizeof`)
- **`count`**: Number of items in any collection

```c
void process_items(Item items[], size_t items_len);      // Not items_size
void *buffer_allocate(size_t buffer_size);               // In bytes
size_t get_user_count(const Database *db);               // Number of users
```

## API Design

### Function Parameter Ordering [AVMCCS-A001]

Follow this consistent parameter order for better API clarity:

```
func(target, inputs, outputs, options, environment)
```

#### Parameter Types

1. **target**: The primary parameter being operated on (must be initialized)

   ```c
   bool date_time_is_valid(const DateTime *dt);
   void date_time_add_days(DateTime *dt, int days);
   void buffer_append(Buffer *buf, const char *data);
   ```

   Exception: `_init` and `_create` functions initialize the target parameter

2. **inputs**: Additional input data with `const` qualification

   ```c
   // Arrays/buffers must always be followed by their length
   void hash_update(Hash *h, const uint8_t data[], size_t data_len);
   int string_find_all(const char *haystack, const char *needle,
                       int positions[], size_t positions_len);
   ```

3. **outputs**: Usually uninitialized output parameters (come after inputs)

   ```c
   // Parse functions: input data first, then output location
   parse_int_result_t parse_int(const char *str, int32_t *out);
   base64_decode_result_t base64_decode(const char *encoded,
                                        uint8_t decoded[], size_t *decoded_len);
   ```

   Note: Additional read-write parameters may precede pure output parameters when it makes semantic sense

4. **options**: Function options (prefer enums for clarity)

   ```c
   void user_message_show(Display *display, const char *msg,
                          user_message_style_t style);
   csv_parse_result_t parse_csv(const char *data, Table *out,
                                csv_options_t options);
   ```

5. **environment**: Context or environment pointers (avoid globals)

   ```c
   void log_message(Logger *logger, const char *msg, log_level_t level,
                    Environment *env);
   ```

#### Key Principle

The target parameter must be initialized (except for `_init`/`_create`), while output parameters are usually uninitialized:

```c
// acc must be initialized - it's the target parameter
void int32_add_parsed(int32_t *acc, const char *str);

// out is uninitialized - it's an output parameter
parse_int_result_t int32_parse(const char *str, int32_t *out);
```

### API Clarity

#### Prefer Enums Over Bools [AVMCCS-A002]

Use enums for function options to create self-documenting APIs:

```c
// Good: Intent is clear at call site
user_message_show(&display, "Hello", UserMessageBlinking);
connection_open(&conn, "example.com", ConnectionSecure);

// Bad: Unclear boolean parameters
user_message_show(&display, "Hello", true);  // What does true mean?
connection_open(&conn, "example.com", false); // Encrypted? Async?
```

Enums are also more maintainable - adding a third option doesn't break existing code.

#### Return Values for Error Handling [AVMCCS-A003]

Use return values for status/errors, output parameters for actual results:

```c
// Good: Can check errors inline
if (parse_int("123", &value) != ParseIntOk) {
    handle_error();
    return;
}
// value is now safe to use

// Follows standard C patterns
if (sscanf(input, "%d", &number) != 1) {
    // Handle parse error
}
```

Consider using function-specific or module-specific result enums for clearer error handling:

```c
// Function-specific result enum
typedef enum
{
    ParseConfigOk,
    ParseConfigFileOpenFailed,
    ParseConfigOutOfMemory,
    ParseConfigInvalidSyntax,
    ParseConfigMissingRequired
} parse_config_result_t;

// Usage provides clear, specific error information
parse_config_result_t result = parse_config("app.conf", &config);
switch (result) {
    case ParseConfigOk:
        break;
    case ParseConfigFileOpenFailed:
        log_error("Cannot open configuration file");
        break;
    case ParseConfigInvalidSyntax:
        log_error("Configuration syntax error");
        break;
    // ...
}
```

This pattern provides context-specific errors without polluting the global namespace.

### Array vs Pointer Syntax [AVMCCS-A004]

Use syntax that communicates intent:

- **Array syntax** for multiple elements: `void sort_numbers(int32_t arr[], size_t len)`
- **Pointer syntax** for single objects: `void increment(int32_t *value)`
- **Pointer syntax** for strings: `size_t strlen(const char *str)`

The `[]` syntax in parameters documents that multiple items are expected.

### Global State Management

Global variables should be avoided. Instead, use a context structure [AVMCCS-A005]:

```c
// Define a context structure for all "global" state
typedef struct
{
    Logger *logger;
    Database *db;
    Config *config;
    int request_count;
    bool system_initialized;
} GlobalContext;

// Pass the context to functions that need it
void process_request(Request *req, GlobalContext *ctx)
{
    ctx->request_count++;
    log_message(ctx->logger, "Processing request");
    database_query(ctx->db, req->query);
}
```

This approach provides:
- **Clear dependencies**: Functions explicitly receive context
- **Easier testing**: Can create test contexts
- **Better thread safety**: Each thread can have its own context
- **No hidden global state**: All dependencies are visible

Note: Sometimes globals cannot be avoided (e.g., signal handlers, certain embedded systems constraints), but the context pattern should be the default approach.

### Flag Type Design

Define separate types for flag combinations to avoid semantic issues [AVMCCS-A006]:

```c
// Define individual flags in an enum (for grouping and documentation)
typedef enum
{
    FONT_REGULAR   = 0x00,
    FONT_BOLD      = 0x01,
    FONT_ITALIC    = 0x02,
    FONT_UNDERLINE = 0x04
} font_style_t;

// Define a separate type for combined flags
typedef uint32_t font_style_flags_t;

// Functions take the flags type, not the enum type
void text_render(const char *text, font_style_flags_t style_flags);

// Usage is clear and type-safe
font_style_flags_t style = FONT_BOLD | FONT_ITALIC;
text_render("Hello", style);
```

This pattern solves a fundamental type safety issue: when you OR flags together (`FONT_BOLD | FONT_ITALIC`), the result is not a member of the enum type. By using a separate `_flags_t` type, we acknowledge this reality while maintaining clarity.

**API clearly shows intent**:

```c
void file_set_state(file_state_t state);              // One state
void file_set_permissions(permission_flags_t perms);  // Multiple flags
```

**C23 Enhancement**: When C23 is available, specify the enum's underlying type for perfect compatibility:

```c
// C23: Ensures enum and flags type are compatible
typedef enum font_style_t : uint32_t
{
    FONT_REGULAR   = 0x00,
    FONT_BOLD      = 0x01,
    FONT_ITALIC    = 0x02,
    FONT_UNDERLINE = 0x04
} font_style_t;

typedef uint32_t font_style_flags_t;  // Guaranteed compatible with enum
```

### Struct Parameter Passing

Always use pointers for structs (efficiency and API consistency) [AVMCCS-A007]:

```c
// Good: Pass structs by pointer
void date_time_add_days(DateTime *dt, int days);
bool user_is_valid(const User *user);
Config *config_new(const ConfigOptions *options);

// Bad: Pass by value (inefficient, inconsistent)
DateTime date_time_add_days(DateTime dt, int days);
bool user_validate(User user);
```

If pass-by-value cannot be avoided for any reason, structure should be small (≤ 16 bytes):

```c
// Acceptable only for very small structs
typedef struct
{
    uint8_t r, g, b, a;
} Color;  // 4 bytes - OK for pass-by-value if necessary

// Too large for pass-by-value
typedef struct
{
    double x, y, z;
} Vector3D;  // 24 bytes - always use pointers
```

## Type Selection and Usage

### Integer Type Selection

#### Size and Index Types [AVMCCS-T001]

Use `size_t` for sizes, lengths, and array indices:

```c
// Good: Correct type for sizes
size_t buffer_size = 1024;
for (size_t i = 0; i < array_len; i++) { ... }

// Bad: Using int for sizes
int buffer_size = 1024;  // Can't represent large sizes
```

**Warning**: Be careful with reverse iteration using `size_t`:

```c
// Bad: This is an infinite loop!
for (size_t i = len - 1; i >= 0; i--) { ... }  // i is never < 0

// Good: Alternative approaches
for (size_t i = len; i > 0; i--) {
    process(array[i - 1]);
}

// Good: Use signed type when available
for (ssize_t i = len - 1; i >= 0; i--) { ... }
```

#### Pointer Arithmetic [AVMCCS-T002]

Use proper types for pointer operations:

```c
// Good: Types designed for pointer operations
uintptr_t addr = (uintptr_t) ptr;
ptrdiff_t offset = ptr2 - ptr1;

// Bad: Using long for pointers (not portable)
long addr = (long) ptr;  // Wrong size on some platforms
```

#### Fixed-Width Types [AVMCCS-T003]

Use explicit-width types from `<stdint.h>` for portable size guarantees:

```c
// Good: Clear about size requirements
uint32_t crc32_calculate(const uint8_t *data, size_t len);
int64_t timestamp_microseconds(void);

// Bad: Platform-dependent sizes
unsigned long flags;  // 32 or 64 bits?
int counter;         // Usually 32 bits, but not guaranteed
```

### Pointer Type Usage [AVMCCS-T004]

Choose the appropriate pointer type based on semantic intent:

- **`void *`**: Generic memory operations

  ```c
  void *malloc(size_t size);
  void free(void *ptr);
  void *memcpy(void *dest, const void *src, size_t n);
  ```

- **`uint8_t *`**: Binary data buffers

  ```c
  // For raw binary data including null bytes, binary protocols
  void hash_update(Hash *h, const uint8_t data[], size_t data_len);
  size_t base64_encode(const uint8_t src[], size_t src_len,
                       char dst[], size_t dst_len);
  ```

- **`char *`**: C strings or character buffers

  ```c
  // For text data (NULL-terminated or buffer with length)
  char *strcpy(char *dest, const char *src);
  size_t utf8_validate(const char buf[], size_t buf_len);
  ```

- **`char8_t *`**: UTF-8 strings (C23)

  ```c
  // When UTF-8 encoding must be explicit
  size_t utf8_strlen(const char8_t *str);
  char8_t *utf8_normalize(const char8_t *str);
  ```

### Const Correctness

Guidelines:
- Always use `const` for read-only parameters [AVMCCS-T005]
- Use `const` for arrays of strings to ensure they're stored in read-only memory [AVMCCS-T006]
- Place `const` before the type for readability [AVMCCS-T007]

```c
const char *ptr;        // Preferred: pointer to const data
char const *ptr;        // Equivalent but avoid

// Const pointer to const data
const char *const ptr;

// Common patterns:
void string_print(const char *str);        // Can't modify string contents
void buffer_process(const uint8_t *data, size_t len);  // Can't modify buffer

// Making string arrays fully const for .rodata section
static const char *const month_names[] = {
    "January", "February", "March", "April",
    "May", "June", "July", "August",
    "September", "October", "November", "December"
};
```

### Typedef Usage

- **Public API types**: Use typedef for types exposed in headers [AVMCCS-T008]

  ```c
  // In public header (date_time.h)
  typedef struct
  {
      int year;
      int month;
      int day;
  } DateTime;

  typedef enum
  {
      ResultOk,
      ResultError,
      ResultTimeout,
      ResultFileOpenFailed,
      ResultOutOfMemory
  } result_t;
  ```

- **Internal types**: Use explicit struct/enum/union keywords for implementation details

  ```c
  // In implementation file (date_time.c)
  struct ParserState
  {
      const char *input;
      size_t position;
      bool has_error;
  };

  // Usage requires struct keyword
  struct ParserState state = {0};
  ```

- **Function pointers**: Always use typedef for readability [AVMCCS-T009]

  ```c
  // Good: Clear and readable
  typedef void (*event_handler_t)(Event *event);
  typedef int (*comparator_t)(const void *a, const void *b);

  // Bad: Hard to read without typedef
  void register_callback(void (*handler)(Event *));  // Avoid

  // Good: Much clearer with typedef
  void register_callback(event_handler_t handler);
  ```

### Forward Declarations

Use forward declarations to minimize dependencies and hide implementation details:

- **Opaque types in public headers**: Hide struct internals from API users [AVMCCS-T010]

  ```c
  // In public header (buffer.h)
  typedef struct Buffer Buffer;  // Users can't see internal fields

  // API works with pointers only
  Buffer *buffer_new(size_t initial_size);
  void buffer_delete(Buffer *buf);
  size_t buffer_length(const Buffer *buf);

  // In implementation (buffer.c)
  struct Buffer
  {
      uint8_t *data;
      size_t length;
      size_t capacity;
  };  // Definition hidden from users
  ```

- **Minimize header dependencies**: Forward declare to avoid unnecessary includes [AVMCCS-T011]

  ```c
  // In header - don't need full definition
  struct Logger;  // Forward declaration
  void process_with_logging(Data *data, struct Logger *logger);

  // Only include full header in .c file where needed
  ```

- **Break circular dependencies**: Use forward declarations when types reference each other [AVMCCS-T012]

  ```c
  // Forward declarations allow mutual references
  typedef struct Node Node;
  typedef struct Tree Tree;

  struct Node {
      Tree *owner;     // Can reference Tree
      Node *next;
  };

  struct Tree {
      Node *root;      // Can reference Node
  };
  ```

Benefits:
- **Faster compilation**: Fewer headers to parse
- **Better encapsulation**: Implementation details stay private
- **Stable APIs**: Internal changes don't affect users

## Memory Layout

### Structure Alignment

Proper structure member ordering improves performance and reduces memory usage:

```c
// Good: Largest to smallest ordering
typedef struct
{
    double value;        // 8 bytes
    int64_t timestamp;   // 8 bytes
    int32_t id;         // 4 bytes
    int16_t flags;      // 2 bytes
    char status;        // 1 byte
    char type;          // 1 byte
} Measurement;  // 24 bytes, well-packed

// Bad: Poor ordering wastes memory
typedef struct
{
    char status;        // 1 byte + 7 padding
    double value;       // 8 bytes
    char type;          // 1 byte + 3 padding
    int32_t id;         // 4 bytes
} WastefulMeasurement;  // 24 bytes with 10 bytes wasted
```

**Order structure members from largest to smallest** [AVMCCS-M001]

**Benefits of proper alignment:**
- **Cache-efficient**: Less padding means more data fits in cache lines
- **Required on some platforms**: Misaligned access can crash
- **Space-efficient**: Critical for arrays - saving 8 bytes per struct means 8MB saved per million elements

### Bit Field Conventions

Use bit fields sparingly for hardware interfaces, wire protocols, and advanced space saving [AVMCCS-M002]:

```c
// Good: Hardware register mapping
typedef struct
{
    uint32_t enable : 1;
    uint32_t mode : 3;
    uint32_t priority : 4;
    uint32_t reserved : 24;  // Explicit padding
} ControlRegister;

// Good: Network protocol
typedef struct
{
    uint8_t version : 4;
    uint8_t header_length : 4;
} PacketHeader;
```

**Note**: These examples assume natural alignment. For packed structures or specific alignment requirements, use compiler-specific attributes.

**Rules:**
- **Always use unsigned types** for bit fields
- **Explicitly pad** to natural boundaries
- **Avoid signed bit fields** - behavior is confusing

```c
// Bad: Signed bit field
struct BadExample
{
    int flag : 1;  // Can only hold -1 or 0, not 0 or 1!
};
```

### External Data Considerations

**Byte order (endianness)**: Network and file data may differ from CPU native order.
Convert network/file data to host byte order [AVMCCS-M003]

```c
// Network data is big-endian, most CPUs are little-endian
uint32_t network_value = recv_from_network();
uint32_t host_value = ntohl(network_value);  // Network to host long
```

**Alignment of external data**: Direct access to network/file buffers can cause crashes.
Use memcpy for potentially unaligned external data [AVMCCS-M004]

```c
// Bad: Direct cast of network buffer (may be misaligned)
uint32_t *value = (uint32_t *) &network_buffer[offset];  // Dangerous!

// Good: Use memcpy for potentially unaligned data
uint32_t value;
memcpy(&value, &network_buffer[offset], sizeof(value));
value = ntohl(value);  // Then convert byte order
```

## Object Management

### Memory Management

#### Heap Allocation

Always check allocation failures [AVMCCS-M005]:

```c
// Good: Check and handle failure appropriately
void *buffer = malloc(size);
if (!buffer) {
    // For embedded systems: propagate error
    return ERROR_OUT_OF_MEMORY;

    // For applications: abort may be acceptable
    abort();  // Or log_fatal("Out of memory");
}

// Consider calloc for zero-initialized memory
User *users = calloc(count, sizeof(User));  // Already zeroed
```

Every allocation must have a corresponding deallocation [AVMCCS-M006]:

```c
// Good: Balanced allocation and deallocation
char *buffer = malloc(buffer_size);
if (!buffer) {
    return ERROR_OUT_OF_MEMORY;
}
// ... use buffer ...
free(buffer);  // Always free what you malloc

// Good: Track ownership carefully
typedef struct
{
    char *name;           // Owned by this struct
    const char *ref;      // Borrowed reference, don't free
} Example;

Example *example_new(const char *name, const char *ref)
{
    Example *ex = malloc(sizeof(Example));
    if (!ex) {
        return NULL;
    }

    ex->name = strdup(name);  // We allocate, we must free
    ex->ref = ref;            // Borrowed, don't free

    if (!ex->name) {
        free(ex);  // Clean up on failure
        return NULL;
    }

    return ex;
}

void example_delete(Example *ex)
{
    if (ex) {
        free(ex->name);  // Free what we allocated
        // Don't free ex->ref - we don't own it
        free(ex);
    }
}
```

**Key principles:**
- Every `malloc()`/`calloc()`/`realloc()` must have a matching `free()`
- Document ownership in structs and function documentation
- Free in reverse order of allocation when possible
- Set pointers to NULL after freeing to avoid use-after-free

**Key principles:**
- Every `malloc()`/`calloc()`/`realloc()` must have a matching `free()`
- Document ownership in structs and function documentation
- Free in reverse order of allocation when possible
- Set pointers to NULL after freeing to avoid use-after-free

#### Stack Allocation Limits

Avoid large stack allocations and variable-length arrays (VLAs):

```c
// Bad: VLAs are dangerous
void process(int n)
{
    int data[n];  // Stack overflow risk, no compile-time checks
}

// Bad: Large stack arrays
void parse(void)
{
    char buffer[8192];  // Too large for stack
}

// Good: Use heap for large or variable sizes
void process(int n)
{
    int *data = malloc(n * sizeof(int));
    if (!data) {
        // handle error here
        return;
    }
    // ...
    free(data);
}
```

**Guidelines:**
- Limit stack arrays to ~256 bytes [AVMCCS-M007]
- Only use big stack arrays in leaf functions (don't call other functions) [AVMCCS-M008]
- Never use VLAs or `alloca()` - they prevent static stack analysis [AVMCCS-M009]
- Prefer fixed-size arrays over VLAs - predictable is better than minimal [AVMCCS-M010]

**Memory tips:**
- `free(NULL)` is safe - no need to check
- Initialize pointers to NULL for clean cleanup paths
- `malloc(0)` is valid but implementation-defined - avoid if possible

### Initialization and Cleanup Patterns

C structures often require initialization and cleanup. Use consistent function naming to distinguish between simple initialization, complex resource management, and heap allocation:

#### Simple Initialization [AVMCCS-M011]

- **`_init`**: Initialize an object in-place (no cleanup needed)

```c
// Simple initialization - no cleanup required
DateTime my_date;
date_time_init(&my_date, 2025, 4, 25);
// No cleanup needed - can safely go out of scope
```

#### Stack-Friendly Resource Management [AVMCCS-M012]

- **`_create`**: Initialize complex object with internal resources (no malloc)
- **`_destroy`**: Clean up internal resources (no free)

```c
// Stack allocation with resource management
Buffer buffer;
buffer_create(&buffer, 1024);  // Allocates internal buffer
// ... use buffer
buffer_destroy(&buffer);        // Frees internal buffer
```

#### Heap-Allocated Objects [AVMCCS-M013]

- **`_new`**: Allocate and initialize (malloc + create)
- **`_delete`**: Clean up and deallocate (destroy + free)

```c
// Heap allocation pattern
User *user = user_new("Alice");
// ... use user
user_delete(user);  // Full cleanup and deallocation
```

### Usage Guidelines

Use `_init` when:
- Object requires only simple field initialization
- No internal resources are allocated
- No corresponding cleanup is needed

Use `_create`/`_destroy` when:
- Object is already allocated (e.g., on the stack)
- Internal resources need allocation/cleanup (buffers, file handles, etc.)
- **Important**: `_destroy` must be called before the object goes out of scope

Use `_new`/`_delete` when:
- Dynamic heap allocation is needed or preferred
- Working with opaque types where size is not known to the caller
- Object lifetime spans multiple scopes

## Documentation and Comments

### Documentation Philosophy

Documentation should enhance code clarity without stating the obvious. Focus on contracts, safety requirements, and non-obvious behavior rather than restating what the code already expresses.

#### What to Document

**Always document:**
- All public APIs (minimum: brief description) [AVMCCS-D001]
- Parameter relationships (array/length pairs, interdependencies) [AVMCCS-D002]
- Memory ownership and lifetime requirements [AVMCCS-D003]
- Units of measurement (even when already in parameter names) [AVMCCS-D004]
- Preconditions and postconditions [AVMCCS-D005]
- Side effects and thread safety [AVMCCS-D006]
- Error handling behavior [AVMCCS-D007]
- Non-obvious algorithmic complexity [AVMCCS-D008]

**Avoid documenting:**
- Self-evident information (e.g., "increments the counter" for `counter_increment()`)
- Implementation details in public API docs
- Obvious parameters (e.g., "the string to process" for a `str` parameter)
- `[in]` direction for any parameters (it's the default)

#### Public vs Private APIs

**Public APIs** (in headers):
- Use formal documentation system (e.g., Doxygen) [AVMCCS-D009]
- Use Javadoc-style comments (`/**`) for Doxygen [AVMCCS-D010]
- Complete parameter documentation with directions [AVMCCS-D011]
- Include examples for non-trivial usage [AVMCCS-D012]
- Document all error conditions [AVMCCS-D013]

**Private APIs** (static functions, internal headers):
- Use informal comments when clarification needed
- Focus on "why" rather than "what"
- Document only non-obvious behavior
- Keep documentation close to code

```c
// Public API in header - formal documentation
/**
 * @brief Calculate CRC32 checksum of data buffer
 *
 * @param data Buffer containing data to checksum
 * @param data_len Number of bytes in data buffer
 * @return CRC32 checksum value
 */
uint32_t crc32_calculate(const uint8_t data[], size_t data_len);

// Private API in implementation - informal comment
// Uses lookup table for speed - see Sarwate's algorithm
static uint32_t crc32_byte(uint32_t crc, uint8_t byte);
```

#### Documentation Levels

Choose documentation depth based on API complexity and safety requirements:

1. **Minimal**: Self-documenting functions need only brief description

   ```c
   /**
    * @brief Check if buffer is empty
    */
   bool buffer_is_empty(const Buffer *buf);
   ```

2. **Standard**: Add parameters, return values, and specific error codes [AVMCCS-D014]

   ```c
   /**
    * @brief Parse integer from string
    *
    * @param str String containing integer representation
    * @param[out] out Parsed integer value (unchanged on error)
    * @return ParseIntOk on success, specific error code on failure
    * @retval ParseIntOk Successfully parsed
    * @retval ParseIntOverflow Value exceeds int32_t range
    * @retval ParseIntInvalidFormat Not a valid integer format
    */
   parse_int_result_t parse_int(const char *str, int32_t *out);
   ```

3. **Safety-Critical**: Add preconditions, warnings, and detailed behavior [AVMCCS-D015]

   ```c
   /**
    * @brief Resize dynamic buffer
    *
    * @param[in,out] buf Buffer to resize (must be initialized)
    * @param new_size New size in bytes
    * @pre buf != NULL && buffer_is_valid(buf)
    * @post buf->capacity >= new_size || return != ResizeOk
    * @warning Existing data preserved up to min(old_size, new_size)
    * @warning Pointers to buffer data invalidated after resize
    * @return ResizeOk on success, error code on failure
    */
   resize_result_t buffer_resize(Buffer *buf, size_t new_size);
   ```

#### Key Documentation Patterns

**Parameter Direction**: Only annotate output and bidirectional parameters [AVMCCS-D016]

```c
/**
 * @brief Parse configuration from string
 *
 * @param config_str Configuration string to parse
 * @param[in,out] line_count On input: max lines; on output: lines parsed
 * @param[out] result Parsed configuration (undefined on error)
 */
config_parse_result_t config_parse(const char *config_str, size_t *line_count,
                                   Config *result);
```

**Array/Length Parameters**: Always document the relationship [AVMCCS-D017]

```c
/**
 * @brief Hash multiple data buffers
 *
 * @param[in,out] hash Hash context (must be initialized)
 * @param chunks Array of data buffers to hash
 * @param sizes Array of buffer sizes (sizes[i] = length of chunks[i])
 * @param count Number of chunks (length of both arrays)
 */
void hash_update_multiple(Hash *hash, const uint8_t *chunks[],
                         const size_t sizes[], size_t count);
```

**Dual-Purpose Parameters**: Clearly explain both uses [AVMCCS-D018]

```c
/**
 * @brief Decode base64 data
 *
 * @param encoded Base64 encoded string
 * @param[out] decoded Output buffer for decoded data
 * @param[in,out] decoded_len On input: buffer capacity; on output: bytes written
 * @warning Output buffer must have capacity for at least (strlen(encoded) * 3/4) bytes
 */
base64_result_t base64_decode(const char *encoded, uint8_t decoded[],
                              size_t *decoded_len);
```

**Memory Ownership**: Be explicit about allocation and lifetime [AVMCCS-D019]

```c
/**
 * @brief Get configuration value
 *
 * @param config Configuration object
 * @param key Configuration key to look up
 * @return Pointer to configuration value (borrowed reference)
 * @warning Returned pointer valid only while config object exists
 * @warning Do not free returned pointer
 */
const char *config_get(const Config *config, const char *key);

/**
 * @brief Take ownership of allocated buffer
 *
 * @param[in,out] pool Memory pool
 * @param size Buffer size to allocate
 * @return Allocated buffer (caller owns memory)
 * @post Caller must free returned buffer with memory_pool_free()
 * @retval NULL Allocation failed
 */
void *memory_pool_alloc(MemoryPool *pool, size_t size);
```

**Units**: Document even when parameter names include units [AVMCCS-D020]

```c
/**
 * @brief Set network timeout
 *
 * @param[in,out] conn Connection object
 * @param timeout_ms Timeout in milliseconds (0 = no timeout)
 * @note Actual timeout may be rounded to system timer resolution
 */
void connection_set_timeout_ms(Connection *conn, uint32_t timeout_ms);
```

**Macro Documentation**: All public macros require documentation [AVMCCS-D021]

```c
/**
 * @def MAX_PATH_LENGTH
 * @brief Maximum path length including null terminator
 */
#define MAX_PATH_LENGTH 4096

/**
 * @def ARRAY_SIZE(arr)
 * @brief Calculate number of elements in array
 * @param arr Array variable (not pointer)
 * @warning Only works with actual arrays, not pointers
 */
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
```

### Code Comments

#### Comment Style

Use comments to explain why, not what. Default to comments above code blocks [AVMCCS-D022]:

```c
// Good: Explains why, not what
// Client expects response within 100ms, so we try cheaper cache first
result = cache_lookup(key);
if (!result) {
    result = database_query(key);
    cache_store(key, result);
}

// Bad: States the obvious
// Look up in cache, then database
result = cache_lookup(key);
if (!result) {
    result = database_query(key);
}
```

Use end-of-line comments sparingly, only for emphasis [AVMCCS-D023]:

```c
// Good: Emphasizing critical detail
do1();
cache_invalidate();  // Must invalidate after do1, before do2
do2();

// Bad: Regular comments at end of line
int counter;         // Counter variable
```

#### Comment Tags

Use standardized tags for actionable comments [AVMCCS-D024]:
- **TODO**: Planned improvements or missing features
- **FIXME**: Known bugs that need fixing
- **HACK**: Temporary workarounds that should be improved
- **WORKAROUND**: Permanent workarounds for external issues

```c
// TODO: Add support for IPv6 addresses
// FIXME: Buffer overflow when input > 1024 bytes
// HACK: Sleep to avoid race condition (remove after fixing event system)
// WORKAROUND: Library bug requires manual byte swapping
```

#### What to Avoid

Never commit commented-out code without explanation [AVMCCS-D025]:

```c
// Bad: No explanation
// old_function();
// more_old_code();

// Good: Explained reference
/*
 * Previous algorithm kept for reference (O(n²) but clearer):
 * for (i = 0; i < n; i++)
 *     for (j = 0; j < n; j++)
 *         process(i, j);
 */
```

Avoid decorative comment blocks [AVMCCS-D026]:

```c
// Bad: Unnecessary decoration
/************************
 * END OF INCLUDES      *
 ************************/

// Good: Simple section marker if needed
// === Public API ===
```

## File Structure

### Style Exception Documentation

When a source file deviates from these guidelines, document the exception at the file's beginning [AVMCCS-S001]:

```c
/* Copyright Header */

/* STYLE EXCEPTION: This file uses camelCase to maintain compatibility
 * with the third-party FooBar API. Function parameter order follows
 * FooBar conventions rather than our standard order.
 */

/* STYLE EXCEPTION: Legacy code - gradual migration in progress.
 * New functions follow the style guide.
 */
```

Common exceptions:
- Third-party API compatibility
- Platform-specific conventions
- Legacy code under gradual migration
- Generated code that cannot be modified

Document exceptions clearly to prevent "fixing" during maintenance.

### Header Files (.h)

Headers should follow this organization [AVMCCS-S002]:

```c
/* Copyright Header */

#ifndef _DATE_TIME_H_
#define _DATE_TIME_H_

#ifdef __cplusplus
extern "C" {
#endif

/* System includes (alphabetical) */
#include <stdbool.h>
#include <stdint.h>

/* Project includes (alphabetical) */
#include <mylib/result.h>

/* Macro definitions */
#define DATE_TIME_MAX_YEAR 9999
#define DATE_TIME_MIN_YEAR 1970

/* Type definitions */
typedef struct
{
    int year;
    int month;
    int day;
} DateTime;

/* Function declarations */
bool date_time_is_valid(const DateTime *dt);
int64_t date_time_diff_seconds(const DateTime *a, const DateTime *b);

#ifdef __cplusplus
}
#endif

#endif /* _DATE_TIME_H_ */
```

### Source Files (.c)

Source files should follow this organization [AVMCCS-S003]:

```c
/* Copyright Header */

/* Own header first */
#include "date_time.h"

/* System includes (grouped and alphabetical) */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* POSIX/platform includes */
#include <unistd.h>

/* Project includes */
#include <mylib/logging.h>
#include <mylib/validation.h>

/* Private macro definitions */
#define DAYS_PER_WEEK 7
#define MONTHS_PER_YEAR 12

/* Private type definitions */
struct ParserState
{
    const char *input;
    size_t position;
};

/* Static function declarations (if needed) */
static bool is_leap_year(int year);

/* Static function definitions (helpers first) */
static bool is_leap_year(int year)
{
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

/* Public function definitions (API functions last) */
bool date_time_is_valid(const DateTime *dt)
{
    if (dt->year < DATE_TIME_MIN_YEAR || dt->year > DATE_TIME_MAX_YEAR) {
        return false;
    }
    // ... validation logic
    return true;
}

int64_t date_time_diff_seconds(const DateTime *a, const DateTime *b)
{
    // ... implementation
}
```

### Include Guards

Use traditional include guards for maximum compatibility [AVMCCS-S004]:

```c
#ifndef _MODULE_NAME_H_
#define _MODULE_NAME_H_
/* content */
#endif /* _MODULE_NAME_H_ */
```

Modern compilers may use `#pragma once` as an alternative.

### Include Ordering

Group includes by origin, with alphabetical ordering within each group [AVMCCS-S005]:

- **Own module header** (first in .c files only)
- **C standard library** (`<stdio.h>`, `<string.h>`, etc.)
- **POSIX/system headers** (`<unistd.h>`, `<fcntl.h>`, etc.)
- **Third-party libraries** (`<curl/curl.h>`, `<sqlite3.h>`, etc.)
- **Project headers** (`<mylib/...>` or `"..."` for local)

Separate each group with a blank line for clarity.

### Header File Naming

Header files should match their module names using underscores [AVMCCS-S006]:
- Module `date_time` → header file `date_time.h`
- Not `datetime.h` or `DateTime.h`

This maintains consistency with the module naming convention.

## Rationale

### Type Safety Through Explicit Interfaces

Using `(void)` for zero-parameter functions enables crucial compiler type checking. In pre-C23 versions of C, empty parentheses `()` create an old-style function declaration that accepts any number of arguments without type checking. This is a dangerous legacy feature that can hide serious bugs:

```c
// Dangerous: compiler won't catch errors
void process();          // Old K&R style - unspecified parameters
process(42, "error");    // Compiles without warning!

// Safe: compiler enforces zero parameters
void process(void);      // Explicit zero parameters
process(42, "error");    // Compiler error: too many arguments
```

While C23 changes `()` to mean zero parameters, using `(void)` ensures compatibility across all modern C versions and makes the intent explicit.

### Type Safety Through Naming

The `_t` suffix distinguishes scalar type aliases from composite types, making type relationships explicit. This enhances code readability and helps prevent type confusion:

```c
// Clear distinction between scalar and composite types
user_id_t id = 42;           // Scalar alias
User user = {.id = id};      // Composite type
```

### Type Suffix Convention and POSIX

While POSIX technically reserves the `_t` suffix for its own type definitions, this convention has become so widespread in C programming that avoiding it would be counterproductive. The `_t` suffix clearly distinguishes type aliases from variables and functions, improving code readability. Most modern C projects use this convention without issues, as name collisions with POSIX types are rare in practice when using descriptive, project-specific type names (e.g., `rgb_t` is unlikely to conflict with any POSIX type).

For projects requiring strict POSIX compliance, consider project-specific prefixes like `avm_rgb_t`.

This guide recommends continuing with `_t` for its widespread recognition and clarity benefits.

### Namespace Simulation

Module prefixing simulates namespaces in C, reducing naming conflicts and improving code organization. This makes it easy to:
- Find all functions related to a module with grep
- Avoid naming collisions in large projects
- Understand code dependencies at a glance

```c
// Clear module boundaries
DateTime start_time, end_time;
// ...
int64_t elapsed = date_time_diff_seconds(&start_time, &end_time);
```

### Implementation Flexibility

Using SCREAMING_SNAKE_CASE for all constants provides implementation flexibility - switching between `#define` and `enum` definitions is transparent to client code:

```c
// These are interchangeable from a naming perspective
#define MAX_RETRIES 3
enum { MAX_RETRIES = 3 };
```

### Visual Type Distinction

PascalCase for composite types creates immediate visual distinction, making it clear when a variable represents a structured data type rather than a scalar value:

```c
DateTime meeting_time;    // Clearly a struct/composite type
int32_t user_id;         // Clearly a scalar value
```

### Enumeration vs Flag Semantics

The casing distinction between enum values (PascalCase) and flag constants (SCREAMING_SNAKE_CASE) signals different usage patterns:

```c
// PascalCase enums: finite, mutually exclusive - use switch statements
switch (connection_state) {
    case ConnectionDisconnected: /* handle */ break;
    case ConnectionConnected: /* handle */ break;
    case ConnectionAuthenticating: /* handle */ break;
    // Compiler can warn about missing cases
}

// SCREAMING_SNAKE_CASE flags: combinable - use bit operations
font_style_flags_t style = FONT_BOLD | FONT_ITALIC;
if (style & FONT_BOLD) {
    // Handle bold styling
}
```

### Early Return Pattern

The early return pattern reduces cognitive load by handling edge cases first, leaving the main logic unindented and easier to follow. This mirrors how we naturally think about problems - deal with special cases, then focus on the core logic.

### Goto for Cleanup

Using `goto` for cleanup provides a structured, predictable pattern that:
- Avoids code duplication (single cleanup path)
- Handles all error cases consistently
- Follows a well-established C idiom
- Is more maintainable than nested if-else chains

### Context Structs Over Globals

The context struct pattern provides numerous benefits:
- **Testability**: Easy to create isolated test contexts
- **Thread safety**: Each thread can have its own context
- **Explicit dependencies**: Functions clearly state what they need
- **No hidden state**: All dependencies are visible in function signatures

### Separate Flag Types

Distinguishing between enum definitions and flag types solves a fundamental type safety issue in C. When you OR flags together (`FONT_BOLD | FONT_ITALIC`), the result is not a member of the enum. By using a separate `_flags_t` type, we acknowledge this reality while maintaining clarity about what values are acceptable.

### Documentation as Contract

Well-documented APIs form contracts between implementers and users. Documentation that focuses on preconditions, postconditions, and invariants helps understand safe usage patterns. This is especially critical in C where type systems cannot express ownership, lifetime, or thread safety requirements.

### Avoiding Documentation Debt

Over-documentation creates maintenance burden and obscures important information. By documenting only what adds value beyond the code itself, we keep documentation synchronized with implementation. Self-documenting code through good naming reduces documentation needs - when `buffer_is_empty()` clearly returns a boolean indicating emptiness, extensive documentation adds no value.

### Public/Private Distinction

Using formal documentation only for public APIs creates clear boundaries in generated documentation. This helps users focus on intended interfaces without being distracted by implementation details. Private functions can still have informal comments for maintainers without cluttering public documentation.

This convention system creates self-documenting code where the naming pattern immediately conveys the semantic role of each identifier.

## Quick Reference

### Formatting Rules

| Rule | Description | Example |
|------|-------------|---------|
| Braces | K&R variant (new line for functions/types) | `void f(void)\n{` |
| Indentation | 4 spaces, no tabs | `    if (x) {` |
| Pointer `*` | With variable name | `char *name` not `char* name` |
| Line length | < 100 columns | Use intermediate variables |
| Hex numbers | Uppercase letters | `0xDEADBEEF` not `0xdeadbeef` |
| Variable declarations | One per line (generally) | `char *first;\nchar *last;` |
| Function spacing | One empty line between | See AVMCCS-F008 |
| Initializer braces | Same line as declaration | `int values[] = {1, 2, 3};` |

### Spacing Rules

| Context | Rule | Good | Bad |
|---------|------|------|-----|
| Control structures | Space after keyword | `if (x)` | `if(x)` |
| Function calls | No space before `(` | `func()` | `func ()` |
| Comments | Space after marker | `// text` | `//text` |
| Operators | Spaces around binary ops | `a + b` | `a+b` |
| Braces | Space before `{` | `if (x) {` | `if (x){` |
| Casts | Space after `)` | `(int *) ptr` | `(int *)ptr` |

### Empty Line Guidelines

| Location | When to Use |
|----------|-------------|
| Before return | Functions > 3 lines |
| Between logical blocks | Separate steps in functions |
| Between cases | Switch cases > 3 lines |
| After guard clauses | Separate validation from logic |
| Control flow blocks | Compact for ≤3 lines, spaced for longer |
| Between functions | Exactly one empty line |

### Language Constructs

| Rule | Description | Example |
|------|-------------|---------|
| Variable declaration | Declare at first use (C99+) | `for (int i = 0; i < n; i++)` |
| Empty parameters | Use `(void)` not `()` | `int get_count(void);` |
| Modern headers | Use standard headers | `<stdint.h>`, `<stdbool.h>` |
| Declaration order | storage, qualifiers, type | `static const int` |
| Increment preference | Post-increment for idiom | `for (int i = 0; i < n; i++)` |
| Early exit | Guard clauses reduce nesting | `if (!valid) { return; }` |
| Switch preference | Use for 3+ comparisons | Clearer than if-else chains |
| Goto usage | Only for cleanup patterns | `goto cleanup;` |
| Static assertions | Compile-time validation | `_Static_assert(sizeof(T) == 24, "msg");` |

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Composite types | PascalCase | `DateTime`, `User` |
| Enum types | lower_snake_case + `_t` | `result_t`, `connection_state_t` |
| Enum values | PascalCase + prefix | `ConnectionConnected`, `DigitalReadFailed` |
| Flag enums | Singular + `_t` | `permission_t`, `font_style_t` |
| Flag types | Singular + `_flags_t` | `permission_flags_t`, `font_style_flags_t` |
| Constants/Flags | SCREAMING_SNAKE_CASE | `MAX_RETRIES`, `FONT_BOLD` |
| Functions/Variables | lower_snake_case | `date_time_diff`, `user_id` |
| Scalar type aliases | lower_snake_case + `_t` suffix | `user_id_t`, `float_or_int32_t` |
| Function prefixes | Module-based | `date_time_*`, `buffer_*` |
| Static functions | No module prefix | `validate_date()` not `date_time_validate_date()` |
| Struct fields | lower_snake_case | `hour_offset` not `hourOffset` |
| Macros | SCREAMING_SNAKE_CASE | `MAX()`, `ARRAY_SIZE()` |
| Internal structs | PascalCase with `struct` | `struct ParserState` |

### Acronym Handling

| Context | Rule | Example |
|---------|------|---------|
| PascalCase | Keep capitalized | `CSVParser`, `URLHandler` |
| To snake_case | Treat as one word | `CSVParser` → `csv_parser` |
| To SCREAMING_SNAKE | Treat as one word | `CSVParser` → `CSV_PARSER` |

### Declaration Order

| Element | Order | Example |
|---------|-------|---------|
| Functions | storage, function-spec, return-type | `static inline void` |
| Variables | storage, type-qualifier, type | `static const int` |
| Constants | storage, const, type | `static const char *const` |
| With restrict | After other qualifiers | `const char *restrict src` |
| With volatile | Between storage and type | `static volatile sig_atomic_t` |
| With _Atomic | As type qualifier | `_Atomic int counter` |

### Flag Type Summary

| Type | Usage | Naming |
|------|-------|--------|
| State enums | Mutually exclusive states | Singular: `file_state_t` |
| Flag enum definition | Individual flag constants | Singular: `permission_t` |
| Combined flags type | Bitwise combinations | Singular + `_flags_t`: `permission_flags_t` |

### Collection Naming

| Context | Convention | Example |
|---------|------------|---------|
| Arrays | Plural with `[]` | `User users[]`, `Node nodes[]` |
| Array lengths | Match array name + `_len` | `users_len`, `nodes_len` |
| Non-array collections | Type suffix recommended | `List user_list`, `Set id_set` |
| Element counts | Singular + `_count` | `user_count`, `node_count` |
| Functions returning one | Singular | `find_user()`, `get_node()` |
| Functions returning many | Plural | `get_users()`, `find_nodes()` |

### Function Naming Patterns

| Pattern | Usage | Example |
|---------|-------|---------|
| `_to` / `_from` | Infallible conversions | `color_to_hsv()`, `color_from_rgb()` |
| `_is` / `_has` / `_can` | Predicates (return bool) | `date_time_is_valid()`, `user_has_permission()` |
| `_buf` suffix | Non-NULL-terminated buffers | `count_spaces_in_buf(buf, buf_len)` |
| Unit suffixes | Measurement clarity | `delay_ms()`, `rotate_deg()` |
| No `string` suffix | Default is C string | `parse_iso8601()` not `parse_iso8601_string()` |

### Object Management

| Function Pattern | Allocation | Cleanup Required | Usage |
|------------------|------------|------------------|--------|
| `_init` | Stack-friendly | No | `date_time_init(&obj, 2025, 4, 25)` |
| `_create` / `_destroy` | Stack with resources | Yes | `buffer_create(&obj, 1024)`, `buffer_destroy(&obj)` |
| `_new` / `_delete` | Heap-allocated | Yes | `user_new("Alice")`, `user_delete(obj)` |

### Pointer Type Usage

| Type | Purpose | Example |
|------|---------|---------|
| `void *` | Generic memory operations | `malloc()`, `memcpy()` |
| `uint8_t *` | Binary data buffers | `hash_compute()`, `base64_encode()` |
| `char *` | C strings or text buffers | `strcpy()`, `utf8_validate()` |
| `char8_t *` | Explicit UTF-8 (C23) | `utf8_normalize()` |

### Const Placement

| Pattern | Meaning | Example |
|---------|---------|---------|
| `const T *ptr` | Pointer to const data | `const char *str` |
| `T *const ptr` | Const pointer | `char *const buffer` |
| `const T *const ptr` | Const pointer to const data | `const char *const names[]` |

### Control Flow Guidelines

| Pattern | When to Use | Note |
|---------|-------------|------|
| Early exit | Reduce nesting | Guard clause with braces |
| Switch | Testing one variable for 3+ values | More efficient and clear |
| Empty lines in switch | Cases > 3 lines | Improves readability |
| Goto for cleanup | Resource management | Single cleanup path |

### API Design Patterns

| Pattern | Description | Example |
|---------|-------------|---------|
| Parameter order | target, inputs, outputs, options, environment | `parse_csv(data, len, &table, opts, env)` |
| Array + length | Arrays always followed by length | `process(arr, arr_len)` |
| Error handling | Return status, output via parameters | `if (parse_int(str, &val) != ParseIntOk)` |
| Enums over bools | Self-documenting options | `UserMessageBlinking` not `true` |
| Array syntax | `[]` for multiple items, `*` for single | `void sort(int arr[], size_t len)` |
| Global state | Use context struct, not globals | `process(Request *req, GlobalContext *ctx)` |
| Struct parameters | Always pass by pointer | `void update(User *user)` |

### Type Selection

| Purpose | Type | Example |
|---------|------|---------|
| Sizes/lengths | `size_t` | `size_t len = strlen(str)` |
| Pointer as integer | `uintptr_t` | `uintptr_t addr = (uintptr_t)ptr` |
| Pointer difference | `ptrdiff_t` | `ptrdiff_t off = p2 - p1` |
| Fixed sizes | `uint32_t`, `int64_t` | `uint32_t crc32` |
| Binary data | `uint8_t *` | `hash_compute(uint8_t data[])` |
| Function pointers | Always typedef | `typedef void (*callback_t)(int)` |

### Macro Safety

| Rule | Example |
|------|---------|
| Parenthesize parameters | `#define SQUARE(x) ((x) * (x))` |
| Parenthesize entire expression | `#define MAX(a, b) ((a) > (b) ? (a) : (b))` |
| Multi-statement pattern | `#define SWAP(a, b) do { ... } while (0)` |
| Watch for side effects | Don't use with `++` or function calls |
| Avoid variable shadowing | Use unique names in macros |

### Memory Guidelines

| Topic | Rule | Note |
|-------|------|------|
| Stack arrays | ~256 bytes max | Larger needs heap allocation |
| VLAs | Never use | Security risk, prevents analysis |
| Allocation checks | Always check malloc | Handle or abort |
| Allocation pairing | Every malloc needs a free | Track ownership carefully |
| Cleanup | Use goto pattern | Initialize pointers to NULL |
| `free(NULL)` | Safe, no check needed | Simplifies cleanup |
| Struct alignment | Largest to smallest | Minimize padding |
| Bit fields | Always use unsigned types | Signed bit fields have confusing behavior |

### Static Assertions

| Use Case | Example |
|----------|---------|
| Struct size verification | `_Static_assert(sizeof(T) == 24, "Size changed")` |
| Layout assumptions | `_Static_assert(offsetof(T, field) == 8, "Layout changed")` |
| Config validation | `_Static_assert(MAX >= MIN, "Invalid config")` |
| Platform assumptions | `_Static_assert(sizeof(int) == 4, "Needs 32-bit int")` |
| Enum bounds | `_Static_assert(StateMax <= 255, "Exceeds uint8_t")` |

### Header Naming Convention

| Module Name | Header File | Not |
|-------------|-------------|-----|
| `date_time` | `date_time.h` | `datetime.h`, `DateTime.h` |
| `csv_parser` | `csv_parser.h` | `csvparser.h`, `CSVParser.h` |
| `buffer_pool` | `buffer_pool.h` | `BufferPool.h`, `buffer-pool.h` |

### Common Unit Suffixes

| Category | Suffixes | Examples |
|----------|----------|----------|
| Time | `_ms`, `_us`, `_ns`, `_sec`, `_min` | `delay_ms(100)`, `timeout_sec` |
| Distance | `_mm`, `_cm`, `_m`, `_km` | `distance_km`, `gap_mm` |
| Angles | `_deg`, `_rad` | `rotate_deg(90)`, `angle_rad` |
| Data | `_bytes`, `_kib`, `_mib`, `_gib` | `buffer_size_bytes`, `cache_mib` |
| Frequency | `_hz`, `_khz`, `_mhz` | `sample_rate_khz`, `clock_mhz` |
| Speed | `_rpm`, `_kmh`, `_mps` | `motor_rpm`, `velocity_mps` |

### Typedef and Forward Declarations

| Pattern | Usage | Example |
|---------|-------|---------|
| Public API types | Use typedef in headers | `typedef struct { ... } DateTime;` |
| Internal types | Use explicit struct keyword | `struct ParserState state;` |
| Function pointers | Always use typedef | `typedef void (*handler_t)(Event *);` |
| Opaque types | Forward declare in headers | `typedef struct Buffer Buffer;` |
| Circular dependencies | Use forward declarations | `typedef struct Node Node;` |

### External Data Handling

| Issue | Solution | Example |
|-------|----------|---------|
| Byte order | Convert to host order | `value = ntohl(network_value)` |
| Alignment | Use memcpy for safety | `memcpy(&val, &buffer[off], 4)` |

### Comment Tags

| Tag | Usage | Example |
|-----|-------|---------|
| TODO | Planned improvements | `// TODO: Add IPv6 support` |
| FIXME | Known bugs | `// FIXME: Buffer overflow risk` |
| HACK | Temporary workarounds | `// HACK: Sleep to avoid race` |
| WORKAROUND | Permanent fixes for external issues | `// WORKAROUND: Library bug #123` |

### Documentation Guidelines

| What to Document | When | Level of Detail |
|------------------|------|-----------------|
| All public APIs | Always | Minimum: @brief description |
| Parameter directions | Output/bidirectional pointers | @param[out] or [in,out] |
| Array/length relationships | Always | Explicit connection in @param |
| Memory ownership | When not obvious | Who allocates, who frees |
| Lifetime requirements | Borrowed pointers | How long reference is valid |
| Units | Always | Even if in parameter name |
| Error behavior | Non-trivial functions | What happens to outputs on error |
| Preconditions | When assumptions exist | @pre conditions |
| Side effects | Always | Threading, global state, etc. |
| Complexity | When not O(1) or O(n) | Big-O notation |

| What NOT to Document | Example |
|---------------------|---------|
| Obvious parameters | `@param str the string` |
| Self-evident behavior | `increments the counter` for `counter_increment()` |
| Implementation details | Internal algorithms in public API docs |
| Input direction [in] | `@param[in] str` redundant (it's the default) |

| Documentation Patterns | Usage |
|-----------------------|-------|
| Minimal (brief only) | Simple predicates, getters |
| Standard (+params, return) | Most public functions |
| Safety-critical (+pre/post/warning) | Functions with preconditions |
| Complete (+examples, see also) | Complex APIs, templates for users |

### File Organization

**Header files (.h):**
1. Copyright header
2. Include guard (`#ifndef`)
3. C++ guard (`#ifdef __cplusplus`)
4. System includes (alphabetical)
5. Project includes (alphabetical)
6. Macro definitions
7. Type definitions
8. Function declarations
9. C++ guard close
10. Include guard close (`#endif`)

**Source files (.c):**
1. Copyright header
2. Own header include
3. System includes (alphabetical)
4. POSIX/platform includes
5. Project includes
6. Private macro definitions
7. Private type definitions
8. Static function declarations
9. Static function definitions
10. Public function definitions

## Appendix: Tool Configuration

### clang-format

This style guide can be largely enforced using clang-format. Create a `.clang-format` file in your project root:

```yaml
# AtomVM C Coding Style Guide - clang-format configuration
# Place this in .clang-format at your project root

BasedOnStyle: LLVM

# Indentation (rules: AVMCCS-F003)
IndentWidth: 4
TabWidth: 4
UseTab: Never

# Line length (rules: AVMCCS-F006)
ColumnLimit: 100

# Brace placement (rules: AVMCCS-F001, AVMCCS-F002)
BreakBeforeBraces: Custom
BraceWrapping:
  AfterFunction: true
  AfterStruct: true
  AfterEnum: true
  AfterUnion: true
  AfterControlStatement: false
  BeforeElse: false
  BeforeWhile: false

# Use braces for single-statement blocks (rule: AVMCCS-F002)
InsertBraces: true

# Spacing (rules: AVMCCS-F004, AVMCCS-F015, AVMCCS-F016, AVMCCS-F018, AVMCCS-F020)
PointerAlignment: Right                     # AVMCCS-F004: * with variable
SpaceAfterCStyleCast: true                  # AVMCCS-F018: Space after cast
SpacesInCStyleCastParentheses: false
SpaceBeforeParens: ControlStatements        # AVMCCS-F015: Space after keywords
SpaceBeforeAssignmentOperators: true       # AVMCCS-F020: Spaces around operators
SpaceInEmptyParentheses: false              # AVMCCS-F016: No space in function calls

# Line breaking (rules: AVMCCS-F002)
AllowShortFunctionsOnASingleLine: None
AllowShortIfStatementsOnASingleLine: Never
AllowShortLoopsOnASingleLine: false
AlwaysBreakAfterReturnType: None
BreakBeforeBinaryOperators: None

# Other formatting (rules: AVMCCS-F008, AVMCCS-F017)
MaxEmptyLinesToKeep: 1                      # AVMCCS-F008: One empty line between functions
SpacesBeforeTrailingComments: 1             # AVMCCS-F017: Space after //

# Include ordering (rules: AVMCCS-S005)
SortIncludes: CaseSensitive
IncludeBlocks: Preserve
IncludeCategories:
  - Regex:           '^".*\.h"'              # Own headers
    Priority:        1
  - Regex:           '^<(assert|ctype|errno|float|limits|locale|math|setjmp|signal|stdarg|stddef|stdio|stdlib|string|time)\.h>'
    Priority:        2                       # C standard library
  - Regex:           '^<(aio|arpa/|cpio|dirent|dlfcn|fcntl|fmtmsg|fnmatch|ftw|glob|grp|iconv|langinfo|libgen|monetary|mqueue|ndbm|net/|netdb|netinet/|nl_types|poll|pthread|pwd|regex|sched|search|semaphore|spawn|strings|stropts|sys/|syslog|tar|termios|trace|ulimit|unistd|utime|utmpx|wordexp)\.h>'
    Priority:        3                       # POSIX headers
  - Regex:           '^<.*>'
    Priority:        4                       # Third-party libraries
  - Regex:           '.*'
    Priority:        5                       # Project headers
```

**Usage**:

```bash
# Format a single file
clang-format -i source.c

# Format all C files in project
find . -name "*.c" -o -name "*.h" | xargs clang-format -i

# Check formatting without modifying
clang-format --dry-run --Werror source.c
```

### Limitations of clang-format

clang-format cannot enforce many aspects of this style guide:

**Not enforced by clang-format:**
- Naming conventions (functions, variables, types) - use clang-tidy instead
- Header file organization and include grouping semantics beyond alphabetical sorting
- Function parameter ordering patterns (AVMCCS-A001)
- Semantic spacing (empty lines between logical blocks within functions - AVMCCS-F012)
- Empty lines before return statements (AVMCCS-F011)
- Comment quality or presence
- Static assertion placement
- Memory management patterns
- API design patterns (enums vs bools)
- Declaration order semantics (AVMCCS-L004)
- Array vs pointer syntax choices (AVMCCS-A004)
- Hexadecimal letter case (AVMCCS-F019) - clang-format preserves existing case
- Initializer brace placement (AVMCCS-F021) - limited control available

**These require additional tools:**
- **clang-tidy**: For naming conventions and deeper analysis
- **Code review**: For semantic patterns and API design
- **Custom linters**: For project-specific rules
- **Developer discipline**: For consistency

### clang-tidy Configuration

To enforce naming conventions and additional checks, create `.clang-tidy`:

```yaml
# AtomVM C Coding Style Guide - clang-tidy configuration

# Enable only specific checks (avoid using wildcards)
Checks: >
  -*,
  readability-identifier-naming,
  readability-isolate-declaration,
  readability-else-after-return,
  readability-braces-around-statements,
  misc-definitions-in-headers,
  bugprone-macro-parentheses

CheckOptions:
  # Naming conventions (rules: AVMCCS-N003, AVMCCS-N004, AVMCCS-N005, AVMCCS-N007, AVMCCS-N008, AVMCCS-N009, AVMCCS-N019, AVMCCS-N020)
  - key: readability-identifier-naming.FunctionCase
    value: lower_case
  - key: readability-identifier-naming.VariableCase
    value: lower_case
  - key: readability-identifier-naming.ParameterCase
    value: lower_case
  - key: readability-identifier-naming.GlobalVariableCase
    value: lower_case
  - key: readability-identifier-naming.LocalVariableCase
    value: lower_case
  - key: readability-identifier-naming.StructCase
    value: CamelCase
  - key: readability-identifier-naming.UnionCase
    value: CamelCase
  - key: readability-identifier-naming.EnumCase
    value: lower_case
  - key: readability-identifier-naming.EnumConstantCase
    value: CamelCase
  - key: readability-identifier-naming.MacroDefinitionCase
    value: UPPER_CASE
  - key: readability-identifier-naming.TypedefCase
    value: lower_case
  - key: readability-identifier-naming.TypedefSuffix
    value: '_t'

  # One variable per declaration (rule: AVMCCS-F005)
  - key: readability-isolate-declaration.Enabled
    value: true

  # Early return pattern (rule: AVMCCS-L009)
  - key: readability-else-after-return.WarnOnUnfixable
    value: false

  # Always use braces (rule: AVMCCS-F002)
  - key: readability-braces-around-statements.ShortStatementLines
    value: '0'
```

**Usage**:

```bash
# Run clang-tidy on a file
clang-tidy source.c -- -I./include

# Run on entire project with compile_commands.json
clang-tidy -p build/ source.c
```

### Limitations of clang-tidy

clang-tidy cannot enforce all AVMCCS rules:

**Not enforced by clang-tidy:**
- Empty parameter lists using `(void)` (AVMCCS-L002) - no C-specific check available
- Specific module prefixing patterns (AVMCCS-N017, AVMCCS-N018)
- Complex naming patterns like array pluralization (AVMCCS-N011, AVMCCS-N012)
- API parameter ordering (AVMCCS-A001)
- Context structure usage over globals (AVMCCS-A005)
- Memory allocation size limits (AVMCCS-M007, AVMCCS-M008)
- Goto cleanup patterns (AVMCCS-L011)
- Documentation requirements (AVMCCS-D001 through AVMCCS-D026)
- File structure organization (AVMCCS-S002, AVMCCS-S003)

**Limited enforcement:**
- Some checks are C++-oriented and may produce false positives for C code
- Macro safety (AVMCCS-L015-L017) is only partially checked by `bugprone-macro-parentheses`
- Memory management patterns require multiple checks that may not cover all cases

### Editor Integration

Most modern editors support clang-format integration:

- **VS Code**: Install the C/C++ extension, format on save
- **Vim**: Use vim-clang-format plugin
- **Emacs**: Use clang-format.el
- **CLion**: Built-in support under Code Style settings

### Git Hooks

Add a pre-commit hook to ensure consistent formatting:

```bash
#!/bin/sh
# .git/hooks/pre-commit

# Format staged C files
for file in $(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(c|h)$'); do
    clang-format -i "$file"
    git add "$file"
done
```

### Static Analysis

Consider using additional tools alongside formatting:

- **cppcheck**: For additional static analysis
- **include-what-you-use**: To maintain clean includes
- **Custom scripts**: For project-specific naming patterns and conventions

## Appendix: Doxygen Documentation Examples

This appendix provides Doxygen documentation examples using Javadoc-style comments (`/**`). All examples follow standard Doxygen syntax without custom extensions.

### Basic Predicate Documentation

```c
/**
 * @brief Check if date/time is valid
 *
 * @param dt DateTime structure to validate
 * @return true if all fields contain valid values, false otherwise
 */
bool date_time_is_valid(const DateTime *dt);

/**
 * @brief Test if user has specific permission
 *
 * @param user User object to check
 * @param perm Permission flag to test
 * @return true if user has permission, false otherwise
 */
bool user_has_permission(const User *user, permission_t perm);
```

### Standard Function Documentation

```c
/**
 * @brief Parse ISO 8601 date/time from buffer
 *
 * Parses timestamps in RFC 3339 format from a non-null-terminated buffer.
 * Supports fractional seconds up to microsecond precision and timezone
 * offsets. The buffer may contain additional data after the timestamp.
 *
 * @param buf Buffer containing ISO 8601 formatted string
 * @param buf_len Length of buffer in bytes
 * @param[out] dt Parsed DateTime structure (undefined on error)
 *
 * @return Parsing result code
 * @retval ParseIso8601Ok Successfully parsed complete timestamp
 * @retval ParseIso8601InvalidFormat Format does not match ISO 8601
 * @retval ParseIso8601InvalidDate Date values out of valid range
 * @retval ParseIso8601BufferTooSmall Buffer smaller than minimum timestamp
 *
 * @note Accepts formats: "2025-04-25T10:30:00Z", "2025-04-25T10:30:00.123456Z",
 *       "2025-04-25T10:30:00+05:30", "2025-04-25T10:30:00.123-08:00"
 *
 * @code
 * // Parse timestamp from network packet
 * DateTime dt;
 * parse_iso8601_result_t result = date_time_parse_iso8601_buf(packet->data,
 *                                                             packet->data_len, &dt);
 * if (result != ParseIso8601Ok) {
 *     log_error("Invalid timestamp: %s",
 *               parse_iso8601_result_string(result));
 *     return PROTOCOL_ERROR;
 * }
 * // dt now contains parsed timestamp
 * @endcode
 *
 * @see date_time_parse_iso8601() for null-terminated string variant
 * @see date_time_format_iso8601() for formatting timestamps
 */
parse_iso8601_result_t date_time_parse_iso8601_buf(const char buf[], size_t buf_len,
                                                   DateTime *dt);
```

### Complex API Documentation with Safety Requirements

```c
/**
 * @brief Create hash context for incremental hashing
 *
 * Allocates and initializes a new hash context for the specified algorithm.
 * The context allows incremental hashing of data through multiple update calls.
 *
 * @param algorithm Hashing algorithm to use
 * @param options Algorithm-specific options (may be NULL for defaults)
 *
 * @return Newly allocated hash context
 * @retval NULL Allocation failed or unsupported algorithm
 *
 * @post Caller must free context with hash_delete()
 * @post Context is initialized and ready for hash_update() calls
 *
 * @warning Do not use context after hash_finalize() without reset
 * @warning Context is not thread-safe; use one context per thread
 *
 * @code
 * // Hash a large file in chunks
 * Hash *hash = hash_new(HashSHA256, NULL);
 * if (!hash) {
 *     return ERROR_OUT_OF_MEMORY;
 * }
 *
 * uint8_t buffer[4096];
 * size_t bytes_read;
 * while ((bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0) {
 *     hash_update(hash, buffer, bytes_read);
 * }
 *
 * uint8_t result[HASH_SHA256_SIZE];
 * hash_finalize(hash, result);
 * hash_delete(hash);
 * @endcode
 *
 * @see hash_update() to add data to hash
 * @see hash_finalize() to compute final hash value
 * @see hash_reset() to reuse context for new hash
 * @since v2.0
 */
Hash *hash_new(hash_algorithm_t algorithm, const HashOptions *options);

/**
 * @brief Update hash with additional data
 *
 * Processes additional data through the hash algorithm. Can be called
 * multiple times to hash data incrementally.
 *
 * @param[in,out] hash Hash context (must be initialized)
 * @param data Data buffer to hash
 * @param data_len Number of bytes to hash from data buffer
 *
 * @pre hash != NULL && hash_is_valid(hash)
 * @pre data != NULL || data_len == 0
 * @pre hash_finalize() not yet called on this context
 *
 * @note Zero-length updates are valid but do not change hash state
 * @warning Data is processed immediately; buffer can be reused after return
 */
void hash_update(Hash *hash, const uint8_t data[], size_t data_len);

/**
 * @brief Finalize hash calculation and retrieve result
 *
 * Completes the hash calculation and writes the result to the output buffer.
 * After finalization, the context cannot accept more data without reset.
 *
 * @param[in,out] hash Hash context with data to finalize
 * @param[out] result Output buffer for hash result
 *
 * @pre hash != NULL && hash_is_valid(hash)
 * @pre result != NULL && has sufficient size for algorithm
 * @post Context is finalized; hash_update() will fail until reset
 * @post result contains computed hash value
 *
 * @warning Output buffer must be at least hash_size(hash->algorithm) bytes
 * @note Context remains allocated; use hash_delete() to free
 *
 * @code
 * uint8_t sha256_hash[HASH_SHA256_SIZE];
 * hash_finalize(hash, sha256_hash);
 * // sha256_hash now contains the computed SHA-256 hash
 * @endcode
 */
void hash_finalize(Hash *hash, uint8_t result[]);
```

### Memory Management Documentation

```c
/**
 * @brief Allocate buffer from memory pool
 *
 * Allocates a buffer of the requested size from the pool. The allocation
 * is tracked by the pool and must be freed using memory_pool_free() or released
 * when the entire pool is destroyed.
 *
 * @param[in,out] pool Memory pool to allocate from
 * @param size Number of bytes to allocate
 * @param alignment Required alignment (must be power of 2)
 *
 * @return Pointer to allocated memory
 * @retval NULL Allocation failed (pool exhausted or size too large)
 *
 * @pre pool != NULL && memory_pool_is_valid(pool)
 * @pre alignment > 0 && (alignment & (alignment - 1)) == 0
 * @post Returned memory is aligned to requested boundary
 * @post Returned memory is uninitialized
 *
 * @warning Memory becomes invalid when pool is destroyed
 * @warning Do not use standard free() on returned pointer
 *
 * @note Consider memory_pool_calloc() for zero-initialized memory
 * @note Pool may allocate more than requested for alignment
 *
 * @code
 * // Allocate aligned buffer for SIMD operations
 * float *vectors = memory_pool_alloc_aligned(pool, 100 * sizeof(float), 16);
 * if (!vectors) {
 *     return ERROR_OUT_OF_MEMORY;
 * }
 * // vectors is now 16-byte aligned for SSE operations
 * @endcode
 *
 * @see memory_pool_free() to release individual allocations
 * @see memory_pool_destroy() to release entire pool
 */
void *memory_pool_alloc_aligned(MemoryPool *pool, size_t size, size_t alignment);
```

### Array Parameter Documentation

```c
/**
 * @brief Sort array of integers in-place
 *
 * Uses introsort algorithm for O(n log n) worst-case performance.
 *
 * @param[in,out] values Array of integers to sort
 * @param values_len Number of elements in values array
 * @param compare Comparison function (NULL for ascending order)
 *
 * @pre values != NULL || values_len == 0
 * @post values[0] <= values[1] <= ... <= values[values_len-1] (for default compare)
 *
 * @code
 * int numbers[] = {5, 2, 8, 1, 9};
 * int_array_sort(numbers, ARRAY_SIZE(numbers), NULL);
 * // numbers is now {1, 2, 5, 8, 9}
 *
 * // Custom comparison for descending order
 * int compare_desc(const void *a, const void *b)
 * {
 *     return *(const int *)b - *(const int *)a;
 * }
 * int_array_sort(numbers, ARRAY_SIZE(numbers), compare_desc);
 * // numbers is now {9, 8, 5, 2, 1}
 * @endcode
 */
void int_array_sort(int values[], size_t values_len,
                    int (*compare)(const void *, const void *));

/**
 * @brief Find all occurrences of pattern in text
 *
 * Searches for all non-overlapping occurrences of the pattern within
 * the text and stores their positions in the output array.
 *
 * @param text Text to search within
 * @param text_len Length of text in bytes
 * @param pattern Pattern to search for
 * @param pattern_len Length of pattern in bytes
 * @param[out] positions Array to store match positions
 * @param positions_capacity Maximum matches to store
 * @param[out] match_count Number of matches found (may exceed capacity)
 *
 * @return Search result status
 * @retval TextSearchOk All matches found and stored
 * @retval TextSearchTruncated More matches exist than positions_capacity
 * @retval TextSearchInvalidInput NULL pointers or zero lengths
 *
 * @note positions array contains byte offsets from start of text
 * @note match_count is set even if positions array is too small
 *
 * @code
 * const char *text = "the quick the brown the fox";
 * const char *pattern = "the";
 * size_t positions[10];
 * size_t count;
 *
 * text_search_result_t result = text_find_all_occurrences(
 *     text, strlen(text),
 *     pattern, strlen(pattern),
 *     positions, ARRAY_SIZE(positions),
 *     &count
 * );
 *
 * // positions[0] = 0, positions[1] = 10, positions[2] = 20
 * // count = 3
 * @endcode
 */
text_search_result_t text_find_all_occurrences(const char text[], size_t text_len,
                                                const char pattern[], size_t pattern_len,
                                                size_t positions[], size_t positions_capacity,
                                                size_t *match_count);
```

### Macro Documentation Examples

```c
/**
 * @def MIN(a, b)
 * @brief Return minimum of two values
 *
 * @param a First value
 * @param b Second value
 * @return The smaller of a and b
 *
 * @warning Arguments are evaluated twice; avoid side effects
 * @note Type-generic; works with any comparable types
 *
 * @code
 * int x = MIN(10, 20);        // x = 10
 * float y = MIN(3.14f, 2.71f); // y = 2.71f
 *
 * // WARNING: Don't do this (undefined behavior)
 * int z = MIN(i++, j++);  // Multiple evaluation!
 * @endcode
 */
#define MIN(a, b) ((a) < (b) ? (a) : (b))

/**
 * @def CONTAINER_OF(ptr, type, member)
 * @brief Get pointer to containing structure from member pointer
 *
 * @param ptr Pointer to the member
 * @param type Type of the containing structure
 * @param member Name of the member within the structure
 * @return Pointer to the containing structure
 *
 * @warning ptr must actually point to member within a type structure
 * @warning Undefined behavior if ptr is not from specified member
 *
 * @code
 * struct Person
 * {
 *     char name[50];
 *     int age;
 *     struct Node node;  // For linked list
 * };
 *
 * void process_node(struct Node *n)
 * {
 *     struct Person *person = CONTAINER_OF(n, struct Person, node);
 *     printf("Person: %s, age %d\n", person->name, person->age);
 * }
 * @endcode
 */
#define CONTAINER_OF(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))
```

### Module-Level Documentation

```c
/**
 * @file memory_pool.h
 * @brief Fast memory pool allocator for fixed-lifetime allocations
 *
 * This module provides a memory pool allocator optimized for scenarios where
 * many allocations share the same lifetime. All allocations from a pool can
 * be freed simultaneously by destroying the pool.
 *
 * @note Thread-safety: Pools are not thread-safe. Use one pool per thread
 *       or add external synchronization.
 *
 * @warning Never mix memory_pool_free() with standard free() calls
 *
 * Example usage:
 * @code
 * MemoryPool *pool = memory_pool_new(64 * 1024);  // 64KB pool
 *
 * // Allocate from pool - no individual free needed
 * User *user = memory_pool_alloc(pool, sizeof(User));
 * user->name = memory_pool_strdup(pool, "Alice");
 * user->data = memory_pool_alloc(pool, user->data_size);
 *
 * // Process user...
 *
 * // Free everything at once
 * memory_pool_delete(pool);
 * @endcode
 *
 * @see memory_pool_new() to create a pool
 * @see memory_pool_alloc() for basic allocation
 * @see memory_pool_alloc_aligned() for aligned allocation
 */
```

### Error Handling Documentation

```c
/**
 * @brief Open configuration file
 *
 * Opens and parses a configuration file, returning a configuration object.
 * The file must be in valid INI format with UTF-8 encoding.
 *
 * @param path Path to configuration file
 * @param[out] config Newly created configuration object
 * @param[out] error Detailed error information (optional, may be NULL)
 *
 * @return Status code indicating success or failure
 * @retval ConfigOk Successfully loaded configuration
 * @retval ConfigFileNotFound Specified file does not exist
 * @retval ConfigAccessDenied Insufficient permissions to read file
 * @retval ConfigInvalidFormat File is not valid INI format
 * @retval ConfigOutOfMemory Memory allocation failed
 *
 * @post On success, *config is valid and must be freed with config_delete()
 * @post On failure, *config is NULL and *error contains details (if provided)
 *
 * @code
 * Config *config;
 * ConfigError error;
 *
 * config_result_t result = config_open("app.ini", &config, &error);
 * if (result != ConfigOk) {
 *     fprintf(stderr, "Failed to load config: %s (line %d: %s)\n",
 *             config_result_string(result),
 *             error.line_number,
 *             error.details);
 *     return EXIT_FAILURE;
 * }
 *
 * // Use configuration
 * const char *db_path = config_get(config, "database", "path");
 *
 * config_delete(config);
 * @endcode
 *
 * @note Error details are only populated on parsing errors
 * @see config_get() to retrieve configuration values
 * @see config_set() to modify configuration
 * @see config_save() to write configuration back to file
 */
config_result_t config_open(const char *path, Config **config,
                            ConfigError *error);
```

## Rule Index

This section provides a complete index of all rules defined in this style guide, organized by category.

| ID | Rule | Section |
|----|------|---------|
| **Language Constructs** | | |
| AVMCCS-L001 | Declare variables where needed, not just at function start | Modern C Practices |
| AVMCCS-L002 | Use `(void)` for empty parameter lists | Modern C Practices |
| AVMCCS-L003 | Use modern headers (`<stdint.h>`, `<stdbool.h>`, etc.) | Modern C Practices |
| AVMCCS-L004 | Follow consistent order for declaration specifiers | Declaration Order |
| AVMCCS-L005 | Use `restrict` for pointer parameters that don't alias | Declaration Order |
| AVMCCS-L006 | Use `_Atomic` for atomic operations (C11) | Declaration Order |
| AVMCCS-L007 | Use `volatile` for hardware registers or signal handlers | Declaration Order |
| AVMCCS-L008 | Prefer post-increment (`i++`) over pre-increment (`++i`) | Increment/Decrement Usage |
| AVMCCS-L009 | Use guard clauses (early exit) to reduce indentation | Early Exit Over Nesting |
| AVMCCS-L010 | Use switch when testing one variable for 3+ values | Switch Over If-Else Chains |
| AVMCCS-L011 | Use `goto` for clean error handling and resource cleanup | Goto for Cleanup |
| AVMCCS-L012 | Use `_Static_assert` after type definitions to verify layout assumptions | Static Assertions |
| AVMCCS-L013 | Use `_Static_assert` in implementation files to verify platform assumptions | Static Assertions |
| AVMCCS-L014 | Use `_Static_assert` to ensure configuration constants are consistent | Static Assertions |
| AVMCCS-L015 | Always parenthesize macro parameters | Macro Safety |
| AVMCCS-L016 | Parenthesize the entire macro expression | Macro Safety |
| AVMCCS-L017 | Use `do { ... } while (0)` for multi-statement macros | Macro Safety |
| AVMCCS-L018 | Avoid expressions with side effects as macro arguments | Macro Safety |
| AVMCCS-L019 | Protect against variable shadowing in complex macros | Macro Safety |
| **Formatting** | | |
| AVMCCS-F001 | K&R style variant for brace placement | Style Rules |
| AVMCCS-F002 | Always use braces, even for single-statement blocks | Style Rules |
| AVMCCS-F003 | Use 4 space indentation (no tabs) | Style Rules |
| AVMCCS-F004 | The `*` belongs with the variable name, not the type | General Rules |
| AVMCCS-F005 | Generally declare one variable per line | General Rules |
| AVMCCS-F006 | Keep lines below 100 columns | General Rules |
| AVMCCS-F007 | Use intermediate variables with meaningful names to break up complex expressions | General Rules |
| AVMCCS-F008 | Separate function definitions with exactly one empty line | General Rules |
| AVMCCS-F009 | Lines must not end with spaces or tabs | Whitespace Rules |
| AVMCCS-F010 | All source files must end with a newline character | Whitespace Rules |
| AVMCCS-F011 | Separate return statements with empty line when function body > 3 lines | Empty Lines and Spacing |
| AVMCCS-F012 | Use empty lines to separate logical blocks within functions | Empty Lines and Spacing |
| AVMCCS-F013 | Compact style for short blocks, spaced for longer ones | Empty Lines and Spacing |
| AVMCCS-F014 | Separate switch cases with empty lines when > 3 lines per case | Empty Lines and Spacing |
| AVMCCS-F015 | Space after keywords, before opening brace | Spacing |
| AVMCCS-F016 | No space between function name and parentheses | Spacing |
| AVMCCS-F017 | Space after comment markers | Spacing |
| AVMCCS-F018 | Space after closing parenthesis in casts | Spacing |
| AVMCCS-F019 | Use uppercase letters for hexadecimal numbers | Spacing |
| AVMCCS-F020 | Use spaces around binary operators | Spacing |
| AVMCCS-F021 | Always place initializer braces on the same line as declaration | Spacing |
| **Naming** | | |
| AVMCCS-N001 | Preserve word boundaries when converting between casing styles | Word Boundary Preservation |
| AVMCCS-N002 | Keep acronyms capitalized in PascalCase; treat as single word when converting | Word Boundary Preservation |
| AVMCCS-N003 | Use PascalCase for composite types | PascalCase |
| AVMCCS-N004 | Use PascalCase for enum constants representing exclusive alternatives | PascalCase |
| AVMCCS-N005 | Use SCREAMING_SNAKE_CASE for manifest constants | SCREAMING_SNAKE_CASE |
| AVMCCS-N006 | Use SCREAMING_SNAKE_CASE for bit flags | SCREAMING_SNAKE_CASE |
| AVMCCS-N007 | Use lower_snake_case for all function names | lower_snake_case |
| AVMCCS-N008 | Use lower_snake_case for local and global variables | lower_snake_case |
| AVMCCS-N009 | Use lower_snake_case for all parameter names | lower_snake_case |
| AVMCCS-N010 | Use lower_snake_case for scalar type aliases | lower_snake_case |
| AVMCCS-N011 | Arrays get plural names | Arrays and Collections |
| AVMCCS-N012 | Array lengths match the array name | Arrays and Collections |
| AVMCCS-N013 | Non-array collections get descriptive names with type suffix | Arrays and Collections |
| AVMCCS-N014 | Use `_to`/`_from` for infallible, straightforward type conversions | Conversion Functions |
| AVMCCS-N015 | Use `_is`/`_has`/`_can` for predicates returning `bool` | Predicate Functions |
| AVMCCS-N016 | Use `_buf` suffix for non-NULL-terminated buffers | String and Buffer Naming |
| AVMCCS-N017 | Functions must be prefixed with their logical module name | Module Prefixing |
| AVMCCS-N018 | Static functions should not use module prefixes | Static Functions |
| AVMCCS-N019 | All macros use SCREAMING_SNAKE_CASE | Macros |
| AVMCCS-N020 | Use `_t` suffix for scalar type aliases | Type Suffix Convention |
| AVMCCS-N021 | Use common prefix for enum values to avoid namespace collisions | Enumeration Namespace Management |
| AVMCCS-N022 | Use singular names for exclusive state enums | Enum and Flag Naming |
| AVMCCS-N023 | Use singular + `_flags_t` suffix for combined flags types | Enum and Flag Naming |
| AVMCCS-N024 | Include units in names to prevent errors | Measurement Units |
| AVMCCS-N025 | Use consistent suffixes for counts and sizes | Size Terminology |
| **API Design** | | |
| AVMCCS-A001 | Follow consistent parameter order (target, inputs, outputs, options, environment) | Function Parameter Ordering |
| AVMCCS-A002 | Use enums for function options to create self-documenting APIs | Prefer Enums Over Bools |
| AVMCCS-A003 | Use return values for status/errors, output parameters for results | Return Values for Error Handling |
| AVMCCS-A004 | Use array syntax `[]` for multiple elements, pointer syntax `*` for single objects | Array vs Pointer Syntax |
| AVMCCS-A005 | Use context structure instead of global variables | Global State Management |
| AVMCCS-A006 | Define separate types for flag combinations to avoid semantic issues | Flag Type Design |
| AVMCCS-A007 | Always use pointers for structs (efficiency and API consistency) | Struct Parameter Passing |
| **Type Selection and Usage** | | |
| AVMCCS-T001 | Use `size_t` for sizes, lengths, and array indices | Size and Index Types |
| AVMCCS-T002 | Use proper types for pointer operations (`uintptr_t`, `ptrdiff_t`) | Pointer Arithmetic |
| AVMCCS-T003 | Use explicit-width types from `<stdint.h>` when size matters | Fixed-Width Types |
| AVMCCS-T004 | Choose appropriate pointer type based on semantic intent | Pointer Type Usage |
| AVMCCS-T005 | Always use `const` for read-only parameters | Const Correctness |
| AVMCCS-T006 | Use `const` for arrays of strings to ensure they're stored in read-only memory | Const Correctness |
| AVMCCS-T007 | Place `const` before the type for readability | Const Correctness |
| AVMCCS-T008 | Use typedef for types exposed in headers | Typedef Usage |
| AVMCCS-T009 | Always use typedef for function pointers | Typedef Usage |
| AVMCCS-T010 | Use opaque types in public headers to hide implementation | Forward Declarations |
| AVMCCS-T011 | Forward declare to minimize header dependencies | Forward Declarations |
| AVMCCS-T012 | Use forward declarations to break circular dependencies | Forward Declarations |
| **Memory** | | |
| AVMCCS-M001 | Order structure members from largest to smallest | Structure Alignment |
| AVMCCS-M002 | Use bit fields sparingly with specific rules | Bit Field Conventions |
| AVMCCS-M003 | Convert network/file data to host byte order | External Data Considerations |
| AVMCCS-M004 | Use memcpy for potentially unaligned external data | External Data Considerations |
| AVMCCS-M005 | Always check allocation failures | Heap Allocation |
| AVMCCS-M006 | Every allocation must have a corresponding deallocation | Heap Allocation |
| AVMCCS-M007 | Limit stack arrays to ~256 bytes | Stack Allocation Limits |
| AVMCCS-M008 | Only use big stack arrays in leaf functions | Stack Allocation Limits |
| AVMCCS-M009 | Never use VLAs or `alloca()` - they prevent static stack analysis | Stack Allocation Limits |
| AVMCCS-M010 | Prefer fixed-size arrays over VLAs - predictable is better than minimal | Stack Allocation Limits |
| AVMCCS-M011 | Use `_init` for simple initialization | Simple Initialization |
| AVMCCS-M012 | Use `_create`/`_destroy` for stack-friendly resource management | Stack-Friendly Resource Management |
| AVMCCS-M013 | Use `_new`/`_delete` for heap-allocated objects | Heap-Allocated Objects |
| **Documentation and Comments** | | |
| AVMCCS-D001 | All public APIs require minimum brief description | What to Document |
| AVMCCS-D002 | Document parameter relationships | What to Document |
| AVMCCS-D003 | Document memory ownership and lifetime requirements | What to Document |
| AVMCCS-D004 | Document units of measurement even when in parameter names | What to Document |
| AVMCCS-D005 | Document preconditions and postconditions | What to Document |
| AVMCCS-D006 | Document side effects and thread safety | What to Document |
| AVMCCS-D007 | Document error handling behavior | What to Document |
| AVMCCS-D008 | Document non-obvious algorithmic complexity | What to Document |
| AVMCCS-D009 | Use formal documentation system for public APIs | Public vs Private APIs |
| AVMCCS-D010 | Use Javadoc-style comments for Doxygen | Public vs Private APIs |
| AVMCCS-D011 | Complete parameter documentation with directions | Public vs Private APIs |
| AVMCCS-D012 | Include examples for non-trivial usage | Public vs Private APIs |
| AVMCCS-D013 | Document all error conditions | Public vs Private APIs |
| AVMCCS-D014 | Standard level: Add parameters, return values, and specific error codes | Documentation Levels |
| AVMCCS-D015 | Safety-critical level: Add preconditions, warnings, and detailed behavior | Documentation Levels |
| AVMCCS-D016 | Only annotate output and bidirectional parameters | Key Documentation Patterns |
| AVMCCS-D017 | Always document array/length parameter relationships | Key Documentation Patterns |
| AVMCCS-D018 | Clearly explain dual-purpose parameters | Key Documentation Patterns |
| AVMCCS-D019 | Be explicit about memory allocation and lifetime | Key Documentation Patterns |
| AVMCCS-D020 | Document units even when parameter names include units | Key Documentation Patterns |
| AVMCCS-D021 | All public macros require documentation | Key Documentation Patterns |
| AVMCCS-D022 | Default to comments above code blocks | Comment Style |
| AVMCCS-D023 | Use end-of-line comments sparingly, only for emphasis | Comment Style |
| AVMCCS-D024 | Use standardized tags (TODO, FIXME, HACK, WORKAROUND) | Comment Tags |
| AVMCCS-D025 | Never commit commented-out code without explanation | What to Avoid |
| AVMCCS-D026 | Avoid decorative comment blocks | What to Avoid |
| **Structure** | | |
| AVMCCS-S001 | Document style exceptions at file beginning | Style Exception Documentation |
| AVMCCS-S002 | Headers follow specific organization pattern | Header Files (.h) |
| AVMCCS-S003 | Source files follow specific organization pattern | Source Files (.c) |
| AVMCCS-S004 | Use traditional include guards for maximum compatibility | Include Guards |
| AVMCCS-S005 | Group includes by origin with alphabetical ordering | Include Ordering |
| AVMCCS-S006 | Header files match module names using underscores | Header File Naming |

---

*Document Version*: 1.0
*Style Guide Name*: AtomVM C Coding Style Guide (AVMCCS Guide)
*Last Updated*: July 2025
