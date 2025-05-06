# lmachine
**A lightweight, single-header lambda calculus evaluator for C projects**  

Need lambda calculus in your project for some unholy reason? (Seriously, why?) `lmachine` provides a minimal virtual machine for evaluating lambda terms with some handy extras:  

### Features  
- **Single-header design** (inspired by [stb libraries](https://github.com/nothings/stb))  
- **Reference-counted memory management** Not that bad, I promise
- **Lazy evaluation** call-by-need strategy
- **Primitive operations**: Arithmetic (`+`, `-`, `*`, `/`) and equality comparisons  
- **De Bruijn indices** for variable binding  
- **"Standard library"** (opt-in via `LMACHINE_BASIC_UTILS`):  
  - Combinators: `S`, `K`, `I`, `Y`, `B`, `C`, `W`  
  - Boolean logic with Church encodings  
- **Cached booleans** (opt-in via `LMACHINE_CACHED_BOOLEANS`, trades performance for 6 leaked nodes)  

### Why?  
For when you need:  
- To embed a functional language subset in your C code  
- A teaching tool for lambda calculus implementations  

### Example  
```c
#define LMACHINE_IMPLEMENTATION
#define LMACHINE_BASIC_UTILS
#include "lmachine.h"

// Evaluate (λx.x) 42
int main() {
    lm_node_t* id = lm_mk_i_combinator();  // Identity function
    lm_node_t* answer = lm_evaluate(lm_mk_app(id, lm_mk_value(69)));
    printf("%lld", answer->as.value);  // Prints 69
    lm_destroy_node(answer);
}
```

### Documentation
Read a source code

### License  
[MIT](LICENSE)