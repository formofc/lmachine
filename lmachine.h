#ifndef __LMACHINE_H__
#define __LMACHINE_H__ 1
/*
 * lambda calculus machine (lmachine) - public domain stb-like lambda calculus evaluator
 * no warranty implied; use at your own risk
 * 
 * blah-blah-blah
 *
 * LICENSE
 *    MIT License
 *
 *  Copyright (c) 2025 Aidar Shigapov
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 *
 *
 * USAGE
 *    This is a single-header library. Define `LMACHINE_IMPLEMENTATION` before
 *    including to create the implementation.
 *
 * FEATURES
 *    - Reference-counted lambda terms
 *    - Call-by-need(lazy) evaluation strategy
 *    - Basic arithmetic primitives (+ - * / == ...)
 *    - De Bruijn indices for variables
 * 
 * CONFIGURATION
 * #define these before including to override defaults
 *
 *    `LMACHINE_ALLOC`              - custom memory allocation function (default: malloc)
 *    `LMACHINE_STRICT`             - allow to use strict application via lm_mk_stric_app
 *    `LMACHINE_FREE`               - custom memory free function (default: free)
 *    `LMACHINE_BASIC_UTILS`        - enables combinators (S,K,I,Y etc.) and boolean logic
 *    `LMACHINE_CACHED_BOOLEANS`    - caches true/false nodes globally. Safes a lot of perfomance, but adds 6 leaked nodes
 *    `LMACHINE_STDIO`              - includes <stdio.h> and adds 2 new primitives: `LM_NODE_PRIMITIVE_PRINT_CHAR`, `LM_NODE_PRIMITIVE_GET_CHAR`
 *
 * Example:
 * ```c
 *    #define LMACHINE_IMPLEMENTATION
 *    #define LMACHINE_ALLOC my_malloc
 *    #define LMACHINE_FREE my_free
 *    #define LMACHINE_BASIC_UTILS
 *    #include "lmachine.h"
 * ```
 *
 * LIMITATIONS
 *    - Poorly tested
 *    - Really small amount of primitives
 *
 * CONVENTIONS(`ABI`)
 *    - All functions consume their arguments (DCO rules apply, down here)
 *    - Returned nodes must be destroyed by caller
 *    - Variables use De Bruijn indices (https://en.wikipedia.org/wiki/De_Bruijn_index)
 *
 * EXAMPLE
 *  ```c
 *    // Create and evaluate (λx.x) 42
 *    lm_node_t* id = lm_mk_abs(0, lm_mk_var(0));
 *    lm_node_t* answer = lm_evaluate(lm_mk_app(id, lm_mk_value(42)));
 *    printf("%lld", answer->as.value); // prints 42
 *  ```
 *
 * TODO:
 *    - Add more primitives?
 * 
 * MORE
 *  Rules of Argument Passing
 *
 *  *   DCO (Destroy Copy Optimization): When a value should be destroyed and copied in the same context, pass it by value.
 *  *   Every provided `lm_node_t*` value should be destroyed within the function.
 *  ```c
 *      int do_some(lm_node_t* arg) {
 *          lm_node_t* useless = lm_mk_value(10);
 *          // Just before `return`
 *          lm_destroy_node(useless);
 *          lm_destroy_node(arg);
 *          return 4;
 *      }
 *  ```
 *      P.S. This rule guarantees that the value provided in the argument list is destroyed.
 *
 *  *   Every taken `lm_node_t*` value should be copied.
 *      *   Exceptions:
 *          *   DCO
 *      *   Arguments of type `lm_node_t*` must be provided as copies.
 *      *   Exceptions:
 *          *   DCO
 *          *   `lm_copy_node`
 *          *   `lm_destroy_node`
 *  ```c
 *      lm_node_t* do_some2(lm_node_t* arg) {
 *          if (true) {
 *              return arg; // `arg` should be destroyed here, but also copied. No need to destroy or copy.
 *          } else {
 *              lm_node_t* new_node = lm_mk_value(10); // `new_node` should be destroyed here, but also copied. No need to destroy or copy.
 *              lm_destroy_node(arg);
 *              return new_node;
 *          }
 *      }
 *      int do_some3(lm_node_t* arg) {
 *          do_some(lm_copy_node(arg));
 *          do_some(do_some2(lm_copy_node(arg))); // `do_some2(lm_copy_node(arg))` - no need to destroy due to DCO. This value will be destroyed by the called function.
 *          do_some(arg); // This is the last usage of `arg`. `do_some` will destroy it. No need to destroy it manually.
 *          return 4;
 *      }
 *  ```
*/

#if !defined LMACHINE_ALLOC || !defined LMACHINE_FREE
#include <stdlib.h>
#define LMACHINE_ALLOC malloc
#define LMACHINE_FREE free
#endif

#if !defined LMACHINE_ALLOC || !defined LMACHINE_FREE
#error "define LMACHINE_ALLOC and LMACHINE_FREE"
#endif
#define _LMACHINE_PRIMITIVE_ARGS_COUNT 2

typedef long long lm_int;
enum lm_node_tag_t {
    LM_NODE_ABSTRACTION,        // λx.M
    LM_NODE_APPLICATION,        // (f M)
#ifdef LMACHINE_STRICT
    LM_NODE_STRICT_APPLICATION, // (f M)
#endif
    LM_NODE_VARIABLE,           // x
    LM_NODE_VALUE,              // 69, 420, true, false
    LM_NODE_PRIMITIVE,          // +, -, *, /, >, <, ==
    LM_NODE_THUNK,              // lazy evaluation
};
enum lm_node_primitive_t {
    LM_NODE_PRIMITIVE_ADD,
    LM_NODE_PRIMITIVE_SUB,
    LM_NODE_PRIMITIVE_MUL,
    LM_NODE_PRIMITIVE_DIV,
    LM_NODE_PRIMITIVE_EQ,
    LM_NODE_PRIMITIVE_MORE,
    LM_NODE_PRIMITIVE_LESS,
#ifdef LMACHINE_STDIO // Not rlly in functional programming fashion, but built-in monads would be MASSIVE bloat
    LM_NODE_PRIMITIVE_PRINT_CHAR,
    LM_NODE_PRIMITIVE_GET_CHAR,
#endif
};

typedef struct lm_node_t {
    enum lm_node_tag_t tag;
    int ref_count;
    union {
        // LM_NODE_ABSTRACTION
        struct {
            long var_index;         // 1, 2, 3 ...
            struct lm_node_t* body; // body
        } abstraction;

        // LM_NODE_APPLICATION/LM_NODE_STRICT_APPLICATION
        struct {
            struct lm_node_t* func;  // f
            struct lm_node_t* arg;   // N
        } application;

        // LM_NODE_VARIABLE (x)
        long variable;

        // LM_NODE_VALUE
        lm_int value;

        // LM_NODE_PRIMITIVE (+, -, /, *, ...)
        struct {
            enum lm_node_primitive_t opcode; 
            struct lm_node_t* args[_LMACHINE_PRIMITIVE_ARGS_COUNT];
        } primitive;

        // LM_NODE_THUNK (lazy eval) .{<-{@(__!!~`! MUTABLE !`~!!__)@}->}.
        struct lm_node_t* thunk;

    } as;
} lm_node_t;

// API
/**
 * @brief Evaluates a lambda calculus expression to normal form
 * @param node The expression to evaluate
 * @return New node containing the evaluated result (must be destroyed by caller)
 */
lm_node_t* lm_evaluate(lm_node_t* node);

/**
 * @brief Creates a new lambda abstraction node
 * @param var_index De Bruijn index of the bound variable
 * @param body Body of the abstraction
 * @return New abstraction node (must be destroyed by caller)
 */
lm_node_t* lm_mk_abs(long var_index, lm_node_t* body);

/**
 * @brief Creates a new variable node
 * @param index De Bruijn index of the variable
 * @return New variable node (must be destroyed by caller)
 */
lm_node_t* lm_mk_var(long index);

/**
 * @brief Creates a new application node
 * @param func Function expression
 * @param arg Argument expression
 * @return New application node (must be destroyed by caller)
 */
lm_node_t* lm_mk_app(lm_node_t* func, lm_node_t* arg);

/**
 * @brief Creates a new application node
 * @param func Function expression
 * @param arg Argument expression
 * @return New application node (must be destroyed by caller)
 */
lm_node_t* lm_mk_strict_app(lm_node_t* func, lm_node_t* arg);

/**
 * @brief Creates a new value node (integer)
 * @param val Integer value
 * @return New value node (must be destroyed by caller)
 */
lm_node_t* lm_mk_value(lm_int val);

/**
 * @brief Creates a new boolean node. Define `LMACHINE_CACHED_BOOLEANS` to cache this(+6 leaked nodes) 
 * @param val Boolean value (0 = false, nonzero = true)
 * @return New boolean node (must be destroyed by caller)
 */
lm_node_t* lm_mk_boolean(int val);

/**
 * @brief Creates a new boolean node. Define `LMACHINE_CACHED_BOOLEANS` to cache this(+6 leaked nodes) 
 * @param op Operand (+. -, *, /, <, >, ==, ...). See lm_node_primitive_t
 * @param arg1 First argument
 * @param arg2 Second argument
 * @return New value node or copy of it self when `arg1` or `arg2` evaluate to non-value node (must be destroyed by caller)
 */
lm_node_t* lm_mk_primitive(enum lm_node_primitive_t op, lm_node_t* arg1, lm_node_t* arg2);
/**
 * @brief Creates a copy of a node with reference counting
 * @param node Node to copy
 * @return New reference to the same node
 */
lm_node_t* lm_copy_node(lm_node_t* node);

/**
 * @brief Destroys a node, decrementing reference count or freeing memory
 * @param node Node to destroy
 */
void lm_destroy_node(lm_node_t* node);

#ifdef LMACHINE_BASIC_UTILS
/**
 * @brief Creates Y combinator (λf.(λx.f (x x)) (λx.f (x x)))
 * @return New node containing Y combinator
 */
lm_node_t* lm_mk_y_combinator();

/**
 * @brief Creates B combinator (λf.λg.λx.f (g x))
 * @return New node containing B combinator
 */
lm_node_t* lm_mk_b_combinator();

/**
 * @brief Creates C combinator (λf.λx.λy.f y x)
 * @return New node containing C combinator
 */
lm_node_t* lm_mk_c_combinator();

/**
 * @brief Creates W combinator (λf.λx.f x x)
 * @return New node containing W combinator
 */
lm_node_t* lm_mk_w_combinator();

/**
 * @brief Creates I combinator (λx.x)
 * @return New node containing I combinator
 */
lm_node_t* lm_mk_i_combinator();

/**
 * @brief Creates K combinator (λx.λy.x)
 * @return New node containing K combinator
 */
lm_node_t* lm_mk_k_combinator();

/**
 * @brief Creates S combinator (λx.λy.λz.x z (y z))
 * @return New node containing S combinator
 */
lm_node_t* lm_mk_s_combinator();

/**
 * @brief Creates logical AND combinator (λx.λy.x y false)
 * @return New node containing AND combinator
 */
lm_node_t* lm_mk_and();

/**
 * @brief Creates logical OR combinator (λx.λy.x true y)
 * @return New node containing OR combinator
 */
lm_node_t* lm_mk_or();

/**
 * @brief Creates logical NOT combinator (λx.x false true)
 * @return New node containing NOT combinator
 */
lm_node_t* lm_mk_not();

#endif

#ifdef LMACHINE_IMPLEMENTATION
#ifdef LMACHINE_STDIO
#include <stdio.h>
#endif
// Not part of an API
lm_node_t* _lm_allocate_node() {
    return (lm_node_t*)LMACHINE_ALLOC(sizeof(lm_node_t));
}
// lm_mk_... functions always have `DCO`
lm_node_t* lm_mk_var(long index) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_VARIABLE;
    node->as.variable = index;
    node->ref_count = 0;
    return node;
}
lm_node_t* lm_mk_abs(long var_index, lm_node_t* body) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_ABSTRACTION;
    node->as.abstraction.var_index = var_index;
    node->as.abstraction.body = body;
    node->ref_count = 0;
    return node;
}
lm_node_t* lm_mk_app(lm_node_t* func, lm_node_t* arg) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_APPLICATION;
    node->as.application.func = func;
    node->as.application.arg = arg;
    node->ref_count = 0;
    return node;
}
#ifdef LMACHINE_STRICT
lm_node_t* lm_mk_strict_app(lm_node_t* func, lm_node_t* arg) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_STRICT_APPLICATION;
    node->as.application.func = func;
    node->as.application.arg = arg;
    node->ref_count = 0;
    return node;
}
#endif
lm_node_t* lm_mk_value(lm_int val) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_VALUE;
    node->as.value = val;
    node->ref_count = 0;
    return node;
}

#ifdef LMACHINE_CACHED_BOOLEANS
static lm_node_t* __lm_true_node = NULL;
static lm_node_t* __lm_false_node = NULL;
lm_node_t* lm_mk_boolean(int val) {
    if (val) {
        if (!__lm_true_node)
            __lm_true_node = lm_mk_abs(0, lm_mk_abs(1, lm_mk_var(0))); // True
        return lm_copy_node(__lm_true_node); 
    } else {
        if (!__lm_false_node)
            __lm_false_node = lm_mk_abs(0, lm_mk_abs(1, lm_mk_var(1))); // True
        return lm_copy_node(__lm_false_node); 
    }
}
#else
lm_node_t* lm_mk_boolean(int val) {
    if (val) {
        return lm_mk_abs(0, lm_mk_abs(1, lm_mk_var(0))); // True
    } else {
        return lm_mk_abs(0, lm_mk_abs(1, lm_mk_var(1))); // False
    }
}
#endif
lm_node_t* lm_mk_primitive(enum lm_node_primitive_t op, lm_node_t* arg1, lm_node_t* arg2) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_PRIMITIVE;
    node->as.primitive.opcode = op;
    node->as.primitive.args[0] = arg1;
    node->as.primitive.args[1] = arg2;
    node->ref_count = 0;
    return node;
}
// Not part of an API
lm_node_t* _lm_mk_thunk(lm_node_t* func) {
    lm_node_t* node = _lm_allocate_node();
    node->tag = LM_NODE_THUNK;
    node->as.thunk = func;
    node->ref_count = 0;
    return node;
}
lm_node_t* lm_copy_node(lm_node_t* node) {
    if (!node) return NULL;
    
    ++node->ref_count;
    return node;
}
void lm_destroy_node(lm_node_t* node) {
    if (!node) return;

    if (node->ref_count == 0) {
        switch (node->tag) {
            case LM_NODE_ABSTRACTION:
                lm_destroy_node(node->as.abstraction.body);
                break;
#ifdef LMACHINE_STRICT 
            case LM_NODE_STRICT_APPLICATION:
#endif
            case LM_NODE_APPLICATION:
                lm_destroy_node(node->as.application.func);
                lm_destroy_node(node->as.application.arg);
                break;
                
            case LM_NODE_PRIMITIVE:
                for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) {
                    lm_destroy_node(node->as.primitive.args[i]);
                }
                break;
            case LM_NODE_THUNK:
                lm_destroy_node(node->as.thunk);
                break;
            default:
                break;
        }
        
        LMACHINE_FREE(node);

    } else {
        --node->ref_count;
    }
}
lm_node_t* _lm_eval_primitive(lm_node_t* node) {
    lm_node_t* args[_LMACHINE_PRIMITIVE_ARGS_COUNT] = {NULL};
    
    for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) { // Compute all arguments
        if (node->as.primitive.args[i]) {
            args[i] = lm_evaluate(lm_copy_node(node->as.primitive.args[i]));
        }
    }

    switch (node->as.primitive.opcode) {
#ifdef LMACHINE_STDIO
        case LM_NODE_PRIMITIVE_PRINT_CHAR: {
            if (!args[0] ||
                args[0]->tag != LM_NODE_VALUE) {

                for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);

                return node; // DCO
            }
            lm_int ch = args[0]->as.value;

            lm_node_t* result = lm_copy_node(args[1]);
            
            for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);

            lm_destroy_node(node);

            putchar(ch);

            return result; // DCO
        }
        case LM_NODE_PRIMITIVE_GET_CHAR: {
             for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);
            lm_destroy_node(node);

            return lm_mk_value((lm_int)getchar()); // DCO
        }
#endif
        case LM_NODE_PRIMITIVE_EQ:
        case LM_NODE_PRIMITIVE_MORE:
        case LM_NODE_PRIMITIVE_LESS: {
            if (!args[0] || !args[1] || 
                args[0]->tag != LM_NODE_VALUE ||
                args[1]->tag != LM_NODE_VALUE) {

                for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);

                return node; // DCO
            }

            lm_int a = args[0]->as.value;
            lm_int b = args[1]->as.value;
            
            for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);

            int result;
            switch (node->as.primitive.opcode) {
                case LM_NODE_PRIMITIVE_EQ: result = a == b; break;
                case LM_NODE_PRIMITIVE_MORE: result = a > b; break;
                case LM_NODE_PRIMITIVE_LESS: result = a < b; break;
                default: return node; // DCO
            }
            
            lm_destroy_node(node);
            return lm_mk_boolean(result);
        }

        case LM_NODE_PRIMITIVE_ADD:
        case LM_NODE_PRIMITIVE_SUB:
        case LM_NODE_PRIMITIVE_MUL:
        case LM_NODE_PRIMITIVE_DIV: {
            if (!args[0] || !args[1] || 
                args[0]->tag != LM_NODE_VALUE ||
                args[1]->tag != LM_NODE_VALUE) {

                for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);

                return node; // DCO
            }
            lm_int a = args[0]->as.value;
            lm_int b = args[1]->as.value;

            for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);
            
            lm_int result;
            switch (node->as.primitive.opcode) {
                case LM_NODE_PRIMITIVE_ADD: result = a + b; break;
                case LM_NODE_PRIMITIVE_SUB: result = a - b; break;
                case LM_NODE_PRIMITIVE_MUL: result = a * b; break;
                case LM_NODE_PRIMITIVE_DIV: result = (b != 0) ? a / b : 0; break;
                default: return node; // DCO
            }
            
            lm_destroy_node(node);

            return lm_mk_value(result);
        }

        default:
            return node; // DCO
    }
}

lm_node_t* _lm_substitute(lm_node_t* expr, long var_index, lm_node_t* value) {
    if (!expr) return NULL;

    switch (expr->tag) {
        case LM_NODE_VARIABLE:
            if (expr->as.variable == var_index) {
                lm_destroy_node(expr);
                return value; // DCO
            }
            lm_destroy_node(value);
            // DCO
            return expr;
            
        case LM_NODE_ABSTRACTION: {
            // rebinding defender
            if (expr->as.abstraction.var_index == var_index) {
                lm_destroy_node(value);
                return expr; // DCO
            }

            lm_node_t* new_body = _lm_substitute(lm_copy_node(expr->as.abstraction.body), var_index, value); // DCO
            if (expr->as.abstraction.body == new_body) { // Not changed
                lm_destroy_node(new_body);
                return expr; // DCO
            }
            lm_node_t* new_node = lm_mk_abs(expr->as.abstraction.var_index, new_body); // DCO

            lm_destroy_node(expr);

            return new_node; // DCO
        }
#ifdef LMACHINE_STRICT 
        case LM_NODE_STRICT_APPLICATION:
#endif
        case LM_NODE_APPLICATION: {
            lm_node_t* new_func = _lm_substitute(lm_copy_node(expr->as.application.func), var_index, lm_copy_node(value));
            lm_node_t* new_arg = _lm_substitute(lm_copy_node(expr->as.application.arg), var_index, value); // DCO. last usage
            if (expr->as.application.func == new_func && expr->as.application.arg == new_arg) { // Not changed
                lm_destroy_node(new_func);
                lm_destroy_node(new_arg);
                return expr; // DCO
            }

            lm_node_t* new_node = lm_mk_app(new_func, new_arg); // DCO. last usage
            lm_destroy_node(expr);

            return new_node; // DCO
        }
            
        case LM_NODE_PRIMITIVE: {
            lm_node_t* args[_LMACHINE_PRIMITIVE_ARGS_COUNT] = {NULL};
            int not_changed = 1;
            for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) {
                if (expr->as.primitive.args[i]) {
                    args[i] = _lm_substitute(lm_copy_node(expr->as.primitive.args[i]), var_index, lm_copy_node(value));
                    not_changed = not_changed && (args[i] == expr->as.primitive.args[i]);
                }
            }
            lm_destroy_node(value);

            if (not_changed) {
                for (int i = 0; i < _LMACHINE_PRIMITIVE_ARGS_COUNT; ++i) lm_destroy_node(args[i]);
                return expr; // DCO. Not changed
            }

            enum lm_node_primitive_t opcode = expr->as.primitive.opcode;
            lm_destroy_node(expr);

            return lm_mk_primitive(opcode, args[0], args[1]); // DCO. last usage
        }

        case LM_NODE_THUNK: {
            // return expr;
            lm_node_t* new_thunk = _lm_substitute(lm_copy_node(expr->as.thunk), var_index, value); // DCO

            if (expr->as.thunk == new_thunk) {
                lm_destroy_node(new_thunk);
                return expr; // DCO. Not changed
            }

            // Rarely reacheble

            lm_destroy_node(expr);

            return _lm_mk_thunk(new_thunk); // DCO.
        }
            
        default:
            lm_destroy_node(value);
            return expr; // DCO
    }
}
lm_node_t* lm_evaluate(lm_node_t* node) {
    if (!node) return NULL;
    
    switch (node->tag) {
        case LM_NODE_ABSTRACTION: // Normal form
        case LM_NODE_VALUE:
        case LM_NODE_VARIABLE:
        default:
            return node; // DCO
#ifdef LMACHIME_STRICT
        case LM_NODE_STRICT_APPLICATION:
#endif
        case LM_NODE_APPLICATION: {
            lm_node_t* func = lm_evaluate(lm_copy_node(node->as.application.func));
            
            // if func its abstraction(new lambda) -> replace all variables in body with arg
            lm_node_t* arg;
            if (func->tag == LM_NODE_ABSTRACTION) {
#ifdef LMACHIME_STRICT
                if (node->tag == LM_NODE_STRICT_APPLICATION) {
                    arg = lm_evaluate(lm_copy_node(node->as.application.arg));
                } else
#endif
                if (node->as.application.arg->tag == LM_NODE_APPLICATION || node->as.application.arg->tag == LM_NODE_PRIMITIVE) { 
                    arg = _lm_mk_thunk(lm_copy_node(node->as.application.arg));
                } else { // No need to create a thunk for node in normal form
                    arg = lm_copy_node(node->as.application.arg);
                }

                lm_node_t* result = _lm_substitute(
                    lm_copy_node(func->as.abstraction.body),
                    func->as.abstraction.var_index,
                    arg // DCO. last usage
                );
                lm_destroy_node(func);
                lm_destroy_node(node);

                return lm_evaluate(result); // DCO
            }
            arg = lm_copy_node(node->as.application.arg);
            lm_node_t* new_node = lm_mk_app(func, arg); // DCO. new_node just take "ownership" over `func` and `arg`
            lm_destroy_node(node);
            return new_node;
        }
            
        case LM_NODE_PRIMITIVE: {
            return _lm_eval_primitive(node); // DCO
        }

        case LM_NODE_THUNK: {
            lm_node_t* result;
            if (node->ref_count == 0) { // DONT TOUCH. `node->as.thunk.value` can NOT refer to `node`
                result = lm_evaluate(lm_copy_node(node->as.thunk));
                lm_destroy_node(node);
                *node = *result;

            } else { // thunks have access to context depended bindings
                result = lm_evaluate(lm_copy_node(node->as.thunk));
                lm_destroy_node(node);
            }

            return result;
        }
            
    }
}
#ifdef LMACHINE_BASIC_UTILS
// ------------ Basic combinators ------------ //
// λf. (λx. f (x x))(λx. f (x x))
lm_node_t* lm_mk_y_combinator() {
    lm_node_t* f = lm_mk_var(0);
    lm_node_t* x = lm_mk_var(1);
    
    lm_node_t* xx = lm_mk_app(lm_copy_node(x), x); // x x
    lm_node_t* fxx = lm_mk_app(f, xx); // f (x x)
    lm_node_t* inner = lm_mk_abs(1, fxx); // λx.f (x x)
    
    lm_node_t* outer = lm_mk_app(lm_copy_node(inner), inner); // (λx.f (x x)) (λx.f (x x))
    
    return lm_mk_abs(0, outer);
}
// λf.λg.λx.f (g x)
lm_node_t* lm_mk_b_combinator() {
    lm_node_t* f = lm_mk_var(0);
    lm_node_t* g = lm_mk_var(1);
    lm_node_t* x = lm_mk_var(2);
    
    lm_node_t* gx = lm_mk_app(g, x); // DCO
    lm_node_t* fgx = lm_mk_app(f, gx); // DCO
    
    return lm_mk_abs(0, lm_mk_abs(1, lm_mk_abs(2, fgx))); // DCO
}
// λf.λx.λy.f y x
lm_node_t* lm_mk_c_combinator() {
    lm_node_t* f = lm_mk_var(0);
    lm_node_t* x = lm_mk_var(1);
    lm_node_t* y = lm_mk_var(2);
    
    lm_node_t* fy = lm_mk_app(f, y); // DCO
    lm_node_t* fyx = lm_mk_app(fy, x); // DCO
    
    return lm_mk_abs(0, lm_mk_abs(1, lm_mk_abs(2, fyx))); // DCO
}
// λf.λx.f x x
lm_node_t* lm_mk_w_combinator() {
    lm_node_t* f = lm_mk_var(0);
    lm_node_t* x = lm_mk_var(1);
    
    lm_node_t* fxx = lm_mk_app(lm_mk_app(f, lm_copy_node(x)), x); // DCO
    
    return lm_mk_abs(0, lm_mk_abs(1, fxx));
}

// λx.x
lm_node_t* lm_mk_i_combinator() {
    return lm_mk_abs(0, lm_mk_var(0));
}

// λx.λy.x
lm_node_t* lm_mk_k_combinator() {
    return lm_mk_abs(0, lm_mk_abs(1, lm_mk_var(0)));
}

// λx.λy.λz.x z (y z)
lm_node_t* lm_mk_s_combinator() {
    lm_node_t* x = lm_mk_var(0);
    lm_node_t* y = lm_mk_var(1);
    lm_node_t* z = lm_mk_var(2);
    
    lm_node_t* xz = lm_mk_app(x, lm_copy_node(z));
    lm_node_t* yz = lm_mk_app(y, z);
    lm_node_t* xz_yz = lm_mk_app(xz, yz);
    
    return lm_mk_abs(0, lm_mk_abs(1, lm_mk_abs(2, xz_yz)));
}


// ------------ Boolean operators ------------ //
// λx.λy.x y false
lm_node_t* lm_mk_and() {
    lm_node_t* x = lm_mk_var(0);
    lm_node_t* y = lm_mk_var(1);
    lm_node_t* false_val = lm_mk_boolean(0);
    
    return lm_mk_abs(0, lm_mk_abs(1, 
        lm_mk_app(lm_mk_app(x, y), false_val))); // DCO
}
// λx.λy.x true y
lm_node_t* lm_mk_or() {
    lm_node_t* x = lm_mk_var(0);
    lm_node_t* y = lm_mk_var(1);
    lm_node_t* true_val = lm_mk_boolean(1);
    
    return lm_mk_abs(0, lm_mk_abs(1,
        lm_mk_app(lm_mk_app(x, true_val), y))); // DCO
}
// λx.x false true
lm_node_t* lm_mk_not() {
    lm_node_t* x = lm_mk_var(0);
    lm_node_t* false_val = lm_mk_boolean(0);
    lm_node_t* true_val = lm_mk_boolean(1);
    
    return lm_mk_abs(0,
        lm_mk_app(lm_mk_app(x, false_val), true_val)); // DCO
}
#endif // LMACHINE_BASIC_UTILS
#endif // LMACHINE_IMPLEMENTATION
#endif // __LMACHINE_H__
