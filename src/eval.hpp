/*
 * Copyright (c) 2016 Andrew Kelley
 *
 * This file is part of zig, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#ifndef ZIG_EVAL_HPP
#define ZIG_EVAL_HPP

#include "all_types.hpp"


bool const_values_equal(ConstValue *a, ConstValue *b, TypeTableEntry *type_entry);
int eval_const_expr_bin_op(ConstValue *op1_val, TypeTableEntry *op1_type,
        BinOpType bin_op, ConstValue *op2_val, TypeTableEntry *op2_type, ConstValue *out_val);

void eval_min_max_value(CodeGen *g, TypeTableEntry *type_entry, ConstValue *const_val, bool is_max);

#endif
