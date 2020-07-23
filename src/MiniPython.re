/* type point = {x:int,y: int}

   let my_point: point = {x: 5, y: 6};

   type state =
   | Red
   | Green
   | Yellow(int);

   let my_state: state = Red;
   let my_other_state: state = Yellow(5);

   type point_tuple = (int, int);

   let my_point_tuple: point_tuple = (5, 6); */

type vid = string;
type loc = int;

type env = list((vid, loc));

type value =
  | VNone
  | VBool(bool)
  | VInt(int)
  | VString(int, string);

type store = list((loc, value));
type glob = env;

/*
 input: 1 + 1

 {
   "kind" : "Program",
   "location" : [ 1, 1, 1, 6 ],
   "declarations" : [ ],
   "statements" : [ {
     "kind" : "ExprStmt",
     "location" : [ 1, 1, 1, 5 ],
     "expr" : {
       "kind" : "BinaryExpr",
       "location" : [ 1, 1, 1, 5 ],
       "left" : {
         "kind" : "VInt",
         "location" : [ 1, 1, 1, 1 ],
         "value" : 1
       },
       "operator" : "+",
       "right" : {
         "kind" : "VInt",
         "location" : [ 1, 5, 1, 5 ],
         "value" : 1
       }
     }
   } ],
   "errors" : {
     "errors" : [ ],
     "kind" : "Errors",
     "location" : [ 0, 0, 0, 0 ]
   }
 }

  */

type unary_op =
  | Neg;

type binary_op =
  | Add;

type stmt_op =
  | ExprStmt;

/* TODO: feels like this should be a GADT */
type op =
  | Unary(unary_op)
  | Binary(binary_op)
  | Stmt(stmt_op);

type exp =
  | NoneLiteral
  | BooleanLiteral(bool)
  | IntegerLiteral(int)
  | StringLiteral(string)
  | UnaryExpr(unary_expr)
  | BinaryExpr(binary_expr)

and op_expr = {
  op,
  args: list(exp),
}

and unary_expr = op_expr

and binary_expr = op_expr;

type stmt = op_expr;

type stmts = list(stmt);

/* TODO: write semantics for this */
type program = {stmts};

/* ctxts */
type op_ctxt = {
  op,
  args: list(exp),
  values: list(value),
};

type unary_ctxt = op_ctxt;

type binary_ctxt = op_ctxt;

type program_ctxt = unit;
type stmt_ctxt = op_ctxt;

type ctxt =
  | OpCtxt(op_ctxt)
  | Program(program_ctxt)
  | Stmt(stmt_ctxt);

/* preval */
type op_preval = {
  op,
  values: list(value),
};

/* type preval =
   | OpPreval(op_preval); */

type preval = op_preval;

/* exp -> val */
/* None -> VNone*/
/* False -> VBool(false) */

type focus =
  | Empty
  | Program(program)
  | Stmt(stmt)
  | Exp(exp)
  | PreVal(preval)
  | Value(value);

type op_ctxts = list(op_ctxt);

type ctxts = list(ctxt);

/* for now only used for expressions, but maybe more in the future, so calling it workspace */
type workspaceZipper = {
  focus,
  op_ctxts,
};

type programZipper = {
  focus,
  ctxts,
};

type config = {
  workspaceZipper,
  programZipper,
  env,
  store,
  glob,
};

let step = (c: config): option(config) =>
  switch (c) {
  /* zipper rules */
  /* zipper begin - UnaryExpr */
  | {
      programZipper,
      workspaceZipper: {focus: Exp(UnaryExpr({op, args: [a]})), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Exp(a),
        op_ctxts: [{op, args: [], values: []}, ...op_ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper begin - BinaryExpr */
  | {
      programZipper,
      workspaceZipper: {focus: Exp(BinaryExpr({op, args: [a1, a2]})), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Exp(a1),
        op_ctxts: [{op, args: [a2], values: []}, ...op_ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper continue - op_expr */
  | {
      programZipper,
      workspaceZipper: {
        focus: Value(v),
        op_ctxts: [{op, args: [a, ...args], values}, ...op_ctxts],
      },
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Exp(a),
        op_ctxts: [{op, args, values: [v, ...values]}, ...op_ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper end - op_expr */
  | {
      programZipper,
      workspaceZipper: {focus: Value(v), op_ctxts: [{op, args: [], values}, ...op_ctxts]},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: PreVal({op, values: List.rev([v, ...values])}), /* we reverse the values, since they were pushed in reverse order */
        op_ctxts,
      },
      env,
      store,
      glob,
    })

  // /* zipper begin - Stmt */
  // | {
  //     programZipper,
  //     workspaceZipper: {focus: Stmt({op, args: [a, ...args]}), op_ctxts},
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       focus: Exp(a),
  //       op_ctxts: [Stmt({op, args, values: []}), ...op_ctxts],
  //     },
  //     env,
  //     store,
  //     glob,
  //   })
  // /* zipper continue - Stmt */
  // | {
  //     programZipper,
  //     workspaceZipper: {
  //       focus: Value(v),
  //       op_ctxts: [Stmt({op, args: [a, ...args], values}), ...op_ctxts],
  //     },
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       focus: Exp(a),
  //       op_ctxts: [Stmt({op, args, values: [v, ...values]}), ...op_ctxts],
  //     },
  //     env,
  //     store,
  //     glob,
  //   })
  // /* zipper end - Stmt */
  // | {
  //     programZipper,
  //     workspaceZipper: {focus: Value(v), op_ctxts: [Stmt({op, args: [], values}), ...op_ctxts]},
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       focus: PreVal({op, values: List.rev([v, ...values])}), /* we reverse the values, since they were pushed in reverse order */
  //       op_ctxts,
  //     },
  //     env,
  //     store,
  //     glob,
  //   })

  /* ChocoPy rules */
  /* NONE */
  | {programZipper, workspaceZipper: {focus: Exp(NoneLiteral), op_ctxts}, env, store, glob} =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VNone),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* BOOL-TRUE and BOOL-FALSE */
  | {
      programZipper,
      workspaceZipper: {focus: Exp(BooleanLiteral(bool)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VBool(bool)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* INT */
  | {
      programZipper,
      workspaceZipper: {focus: Exp(IntegerLiteral(int)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VInt(int)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* STR */
  | {
      programZipper,
      workspaceZipper: {focus: Exp(StringLiteral(string)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VString(String.length(string), string)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* EXPR-STMT */
  | {
      programZipper,
      workspaceZipper: {focus: PreVal({op: Stmt(ExprStmt), values: [_v]}), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Empty,
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* NEGATE */
  | {
      programZipper,
      workspaceZipper: {focus: PreVal({op: Unary(Neg), values: [VInt(i1)]}), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VInt(- i1)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* ARITH: + */
  | {
      programZipper,
      workspaceZipper: {
        focus: PreVal({op: Binary(Add), values: [VInt(i1), VInt(i2)]}),
        op_ctxts,
      },
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        focus: Value(VInt(i1 + i2)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  | _ => None
  };

let inject = (e: exp): config => {
  programZipper: {
    focus: Empty,
    ctxts: [],
  },
  workspaceZipper: {
    focus: Exp(e),
    op_ctxts: [],
  },
  env: [],
  store: [],
  glob: [],
};

/* def is_final : state -> Prop
   | ⟨sum.inr v, ⟨option.none, env⟩, []⟩ := true
   | _ := false */

/* let isFinal = (c: config): bool =>
   switch (c) {
   | {zipper: {focus: Value(_), op_ctxts: []}, env: _, stack: []} => true
   | _ => false
   }; */
let isFinal = (c: config): bool => false;

let rec iterateMaybeAux = (f: 'a => option('a), x: option('a)): list('a) =>
  switch (x) {
  | None => []
  | Some(x) =>
    let fx = f(x);
    [x, ...iterateMaybeAux(f, fx)];
  };

let advance = step;

let rec takeWhileInclusive = (p, l: list('a)): list('a) =>
  switch (l) {
  | [] => []
  | [x, ...xs] => [
      x,
      ...if (p(x)) {
           takeWhileInclusive(p, xs);
         } else {
           [];
         },
    ]
  };

let iterateMaybe = (f: 'a => option('a), x: 'a): list('a) => iterateMaybeAux(f, Some(x));

let interpretTrace = (p: exp): list(config) => {
  let states = iterateMaybe(step, inject(p));
  /* takeWhileInclusive(c => !isFinal(c), states) */
  states;
};
