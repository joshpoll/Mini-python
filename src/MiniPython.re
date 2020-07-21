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

type op =
  | Unary(unary_op)
  | Binary(binary_op);

type exp =
  | NoneLiteral
  | BooleanLiteral(bool)
  | IntegerLiteral(int)
  | StringLiteral(string)
  | OpExpr(op_expr)

and op_expr = {
  op,
  args: list(exp),
};

/* ctxts */
type op_ctxt = {
  op,
  args: list(exp),
  values: list(value),
};

type ctxt =
  | OpCtxt(op_ctxt);

/* preval */
type op_preval = {
  op,
  values: list(value),
};

type preval =
  | OpPreval(op_preval);

/* exp -> val */
/* None -> VNone*/
/* False -> VBool(false) */

type focus =
  | Exp(exp)
  | PreVal(preval)
  | Value(value);

type ctxts = list(ctxt);

type zipper = {
  focus,
  ctxts,
};

type config = {
  zipper,
  env,
  store,
  glob,
};

let step = (c: config): option(config) =>
  switch (c) {
  /* zipper rules */
  /* zipper skip - op_expr */
  | {zipper: {focus: Exp(OpExpr({op, args: []})), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: PreVal(OpPreval({op, values: []})),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* zipper begin - op_expr */
  | {zipper: {focus: Exp(OpExpr({op, args: [a, ...args]})), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: Exp(a),
        ctxts: [OpCtxt({op, args, values: []}), ...ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper continue - op_expr */
  | {
      zipper: {focus: Value(v), ctxts: [OpCtxt({op, args: [a, ...args], values}), ...ctxts]},
      env,
      store,
      glob,
    } =>
    Some({
      zipper: {
        focus: Exp(a),
        ctxts: [OpCtxt({op, args, values: [v, ...values]}), ...ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper end - op_expr */
  | {
      zipper: {focus: Value(v), ctxts: [OpCtxt({op, args: [], values}), ...ctxts]},
      env,
      store,
      glob,
    } =>
    Some({
      zipper: {
        focus: PreVal(OpPreval({op, values: List.rev([v, ...values])})), /* we reverse the values, since they were pushed in reverse order */
        ctxts,
      },
      env,
      store,
      glob,
    })

  /* ChocoPy rules */
  /* NONE */
  | {zipper: {focus: Exp(NoneLiteral), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: Value(VNone),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* BOOL-TRUE and BOOL-FALSE */
  | {zipper: {focus: Exp(BooleanLiteral(bool)), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: Value(VBool(bool)),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* INT */
  | {zipper: {focus: Exp(IntegerLiteral(int)), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: Value(VInt(int)),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* STR */
  | {zipper: {focus: Exp(StringLiteral(string)), ctxts}, env, store, glob} =>
    Some({
      zipper: {
        focus: Value(VString(String.length(string), string)),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* NEGATE */
  | {
      zipper: {focus: PreVal(OpPreval({op: Unary(Neg), values: [VInt(i1)]})), ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      zipper: {
        focus: Value(VInt(- i1)),
        ctxts,
      },
      env,
      store,
      glob,
    })
  /* ARITH: + */
  | {
      zipper: {focus: PreVal(OpPreval({op: Binary(Add), values: [VInt(i1), VInt(i2)]})), ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      zipper: {
        focus: Value(VInt(i1 + i2)),
        ctxts,
      },
      env,
      store,
      glob,
    })
  | _ => None
  };

let inject = (e: exp): config => {
  zipper: {
    focus: Exp(e),
    ctxts: [],
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
   | {zipper: {focus: Value(_), ctxts: []}, env: _, stack: []} => true
   | _ => false
   }; */
let isFinal = (c: config): bool => false;

let rec iterateMaybeAux = (f, x) =>
  switch (x) {
  | None => []
  | Some(x) =>
    let fx = f(x);
    [x, ...iterateMaybeAux(f, fx)];
  };

let advance = step;

let rec takeWhileInclusive = (p, l) =>
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
  takeWhileInclusive(c => !isFinal(c), states);
};
