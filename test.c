#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#define LMACHINE_IMPLEMENTATION
#define LMACHINE_BASIC_UTILS
#define LMACHINE_STRICT
#define LMACHINE_CACHED_BOOLEANS
#define LMACHINE_STDIO

long t = 0;
void fr(void* ptr) {
    --t;
    free(ptr);
}
void* al(size_t sz) {
    ++t;
    return malloc(sz);
}
#define LMACHINE_ALLOC al
#define LMACHINE_FREE fr
#include "lmachine.h"

lm_node_t* lm_mk_fibonacci() {
    // Y (λfib.λn. 
    //   == n 0 then 0
    //   else == n 1 then 1
    //   else + fib (- n 1)  fib (- n 2))
    
    lm_node_t* fib = lm_mk_var(0);  // fib
    lm_node_t* n = lm_mk_var(1);    // n
    
    // Base cases
    lm_node_t* zero = lm_mk_value(0);
    lm_node_t* one = lm_mk_value(1);
    
    // n == 0
    lm_node_t* cond1 = lm_mk_primitive(LM_NODE_PRIMITIVE_EQ, lm_copy_node(n), lm_copy_node(zero));
    
    // n == 1
    lm_node_t* cond2 = lm_mk_primitive(LM_NODE_PRIMITIVE_EQ, lm_copy_node(n), lm_copy_node(one));
    
    // fib (n-1)
    lm_node_t* n_minus_1 = lm_mk_primitive(LM_NODE_PRIMITIVE_SUB, lm_copy_node(n), lm_copy_node(one));
    lm_node_t* fib_n_minus_1 = lm_mk_app(lm_copy_node(fib), n_minus_1);
    
    // fib (n-2)
    lm_node_t* n_minus_2 = lm_mk_primitive(LM_NODE_PRIMITIVE_SUB, n, lm_mk_value(2));
    lm_node_t* fib_n_minus_2 = lm_mk_app(fib, n_minus_2);
    
    // fib(n-1) + fib(n-2)
    lm_node_t* addition = lm_mk_primitive(LM_NODE_PRIMITIVE_ADD, fib_n_minus_1, fib_n_minus_2);
    
    // if-then-else
    lm_node_t* inner_if = lm_mk_app(
        lm_mk_app(cond2, one),  // if n == 1 then 1
        addition             // else fib(n-1) + fib(n-2)
    );

    // strict application only for testing
    lm_node_t* outer_if = lm_mk_app(
        lm_mk_strict_app(cond1, zero), // if n == 0 then 0
        inner_if            // else (if n == 1 then 1 else ...)
    );
    
    // λfib.λn. if-then-else
    lm_node_t* inner = lm_mk_abs(1, outer_if);
    lm_node_t* fib_lambda = lm_mk_abs(0, inner);
    
    // Y fib_lambda
    lm_node_t* fibonacci = lm_mk_app(
        lm_mk_y_combinator(),
        fib_lambda
    );
    
    return fibonacci;
}
int main(int argc, char** argv) {
    bool cache_enabled = false;
    size_t bytes, depth;
    char* cache_data;

    if (argc == 3) {
        if (sscanf(argv[1], "%zu", &bytes) != 1) return 1;
        if (sscanf(argv[2], "%zu", &depth) != 1) return 1;
        cache_enabled = true;
    }
    
    cache_data = malloc(bytes);
    if (!cache_data) return 1;

    lm_node_t* fib = lm_mk_fibonacci();
    lm_node_cache_t cache, *cache_handle = NULL;
    if (cache_enabled && lm_init_node_cache(&cache, (void*)cache_data, bytes, depth)) {
        cache_handle = &cache;
    } else {
        printf("Failed to initialize cache\n");
    }


    if (cache_handle) {
        printf("With cache\n");
    } else {
        printf("Without cache\n");
    }
    
    for (int i = 0; i <= 25; ++i) {
        lm_node_t* result = lm_evaluate_cache(
            lm_mk_app(
                lm_copy_node(fib),
                lm_mk_value(i)
            ),
            cache_handle
        );
        
        printf("Fib(%d) = %lld\n", i, result->as.value);
        lm_destroy_node(result);
    }
    lm_destroy_node(fib);
    lm_destroy_node_cache(cache_handle);
    printf("not freed: %ld\n", t); // 6

    return 0;
}
