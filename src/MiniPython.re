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

type id = string;

type loc = int;

let counter = ref(0);

let freshLoc = () => {
  counter := counter^ + 1;
  counter^ - 1;
};

type env = list((id, loc));

type value =
  | VNone
  | VBool(bool)
  | VInt(int)
  | VString(int, string);

type store = list((loc, value));
type glob = env;

type unary_op =
  | Neg;

type binary_op =
  | Add;

type stmt_op =
  | Pass
  | ExprStmt
  | AssignStmt(id);

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
  | Identifier(id)

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
type stmt_ctxt = {
  stmt_op,
  pre: list(exp),
  post: list(exp),
};

type stmts_ctxt = {
  pre: list(stmt),
  post: list(stmt),
};

type op_ctxt = {
  op,
  args: list(exp),
  values: list(value),
};

type unary_ctxt = op_ctxt;

type binary_ctxt = op_ctxt;

type prog_ctxt =
  | StmtCtxt(op_ctxt)
  | StmtsCtxt(stmts_ctxt);

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

type workspaceFocus =
  | Empty
  | Exp(exp)
  | PreVal(preval)
  | Value(value);

type programFocus =
  | Program(program)
  | Stmt(stmt)
  /* state after stmt has executed, before control moves on */
  | PostStmt(stmt)
  | Exp(exp);

type op_ctxts = list(op_ctxt);
type prog_ctxts = list(prog_ctxt);

// type ctxts = list(ctxt);

/* for now only used for expressions, but maybe more in the future, so calling it workspace */
type workspaceZipper = {
  workspaceFocus,
  op_ctxts,
};

type programZipper = {
  programFocus,
  prog_ctxts,
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
      workspaceZipper: {workspaceFocus: Exp(UnaryExpr({op, args: [a]})), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Exp(a),
        op_ctxts: [{op, args: [], values: []}, ...op_ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper begin - BinaryExpr */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(BinaryExpr({op, args: [a1, a2]})), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Exp(a1),
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
        workspaceFocus: Value(v),
        op_ctxts: [{op, args: [a, ...args], values}, ...op_ctxts],
      },
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Exp(a),
        op_ctxts: [{op, args, values: [v, ...values]}, ...op_ctxts],
      },
      env,
      store,
      glob,
    })
  /* zipper end - op_expr */
  | {
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(v),
        op_ctxts: [{op, args: [], values}, ...op_ctxts],
      },
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: PreVal({op, values: List.rev([v, ...values])}), /* we reverse the values, since they were pushed in reverse order */
        op_ctxts,
      },
      env,
      store,
      glob,
    })

  // /* zipper begin - Stmt */
  // | {
  //     programZipper,
  //     workspaceZipper: {workspaceFocus: Stmt({op, args: [a, ...args]}), op_ctxts},
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       workspaceFocus: Exp(a),
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
  //       workspaceFocus: Value(v),
  //       op_ctxts: [Stmt({op, args: [a, ...args], values}), ...op_ctxts],
  //     },
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       workspaceFocus: Exp(a),
  //       op_ctxts: [Stmt({op, args, values: [v, ...values]}), ...op_ctxts],
  //     },
  //     env,
  //     store,
  //     glob,
  //   })
  // /* zipper end - Stmt */
  // | {
  //     programZipper,
  //     workspaceZipper: {workspaceFocus: Value(v), op_ctxts: [Stmt({op, args: [], values}), ...op_ctxts]},
  //     env,
  //     store,
  //     glob,
  //   } =>
  //   Some({
  //     programZipper,
  //     workspaceZipper: {
  //       workspaceFocus: PreVal({op, values: List.rev([v, ...values])}), /* we reverse the values, since they were pushed in reverse order */
  //       op_ctxts,
  //     },
  //     env,
  //     store,
  //     glob,
  //   })

  /* ChocoPy rules */
  /* NONE */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(NoneLiteral), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VNone),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* BOOL-TRUE and BOOL-FALSE */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(BooleanLiteral(bool)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VBool(bool)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* INT */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(IntegerLiteral(int)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VInt(int)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* STR */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(StringLiteral(string)), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VString(String.length(string), string)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* PASS */
  | {
      programZipper: {programFocus: Stmt({op: Stmt(Pass), args: []}), prog_ctxts},
      workspaceZipper: {workspaceFocus: Value(_v), op_ctxts: []},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: PostStmt({op: Stmt(Pass), args: []}),
        prog_ctxts,
      },
      workspaceZipper: {
        workspaceFocus: Empty,
        op_ctxts: [],
      },
      env,
      store,
      glob,
    })
  /* EXPR-STMT */
  /* zipper enter. boilerplate for zipper */
  | {
      programZipper: {programFocus: Stmt({op: Stmt(ExprStmt), args: [e]}), prog_ctxts},
      workspaceZipper,
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: Exp(e),
        prog_ctxts: [StmtCtxt({op: Stmt(ExprStmt), args: [], values: []}), ...prog_ctxts],
      },
      workspaceZipper,
      env,
      store,
      glob,
    })
  /* workspace enter. not specific to ExprStmt!!! */
  | {
      programZipper: {programFocus: Exp(e), prog_ctxts},
      workspaceZipper: {workspaceFocus: Empty, op_ctxts: []},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: Exp(e),
        prog_ctxts,
      },
      workspaceZipper: {
        workspaceFocus: Exp(e),
        op_ctxts: [],
      },
      env,
      store,
      glob,
    })
  /* workspace exit. the actual rule */
  | {
      programZipper: {
        programFocus: Exp(e),
        prog_ctxts: [StmtCtxt({op: Stmt(ExprStmt), args: [], values: []}), ...prog_ctxts],
      },
      workspaceZipper: {workspaceFocus: Value(_v), op_ctxts: []},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: PostStmt({op: Stmt(ExprStmt), args: [e]}),
        prog_ctxts,
      },
      workspaceZipper: {
        workspaceFocus: Empty,
        op_ctxts: [],
      },
      env,
      store,
      glob,
    })
  /* VAR-READ */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: Exp(Identifier(id)), op_ctxts},
      env,
      store,
      glob,
    } =>
    switch (Belt.List.getAssoc(env, id, (==))) {
    | None => None
    | Some(l_id) =>
      switch (Belt.List.getAssoc(store, l_id, (==))) {
      | None => None
      | Some(v) =>
        Some({
          programZipper,
          workspaceZipper: {
            workspaceFocus: Value(v),
            op_ctxts,
          },
          env,
          store,
          glob,
        })
      }
    }
  /* VAR-INIT */
  /* VAR-ASSIGN-STMT */
  /* zipper enter. boilerplate for zipper */
  /* todo: generalize the version above by intersecting with this one! */
  | {
      programZipper: {programFocus: Stmt({op: Stmt(AssignStmt(id)), args: [e]}), prog_ctxts},
      workspaceZipper,
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: Exp(e),
        prog_ctxts: [
          StmtCtxt({op: Stmt(AssignStmt(id)), args: [], values: []}),
          ...prog_ctxts,
        ],
      },
      workspaceZipper,
      env,
      store,
      glob,
    })
  | {
      programZipper: {
        programFocus: Exp(e),
        prog_ctxts: [StmtCtxt({op: Stmt(AssignStmt(id)), args: [], values: []}), ...prog_ctxts],
      },
      workspaceZipper: {workspaceFocus: Value(v), op_ctxts: []},
      env,
      store,
      glob,
    } =>
    let (env, l_id) =
      switch (Belt.List.getAssoc(env, id, (==))) {
      | None =>
        /* VAR-INIT */
        let l_id = freshLoc();
        (Belt.List.setAssoc(env, id, l_id, (==)), l_id);
      | Some(l_id) =>
        /* VAR-ASSIGN-STMT */
        (env, l_id)
      };
    Some({
      programZipper: {
        programFocus: PostStmt({op: Stmt(AssignStmt(id)), args: [e]}),
        prog_ctxts,
      },
      workspaceZipper: {
        workspaceFocus: Empty,
        op_ctxts: [],
      },
      env,
      store: Belt.List.setAssoc(store, l_id, v, (==)),
      glob,
    });
  /* NEGATE */
  | {
      programZipper,
      workspaceZipper: {workspaceFocus: PreVal({op: Unary(Neg), values: [VInt(i1)]}), op_ctxts},
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VInt(- i1)),
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
        workspaceFocus: PreVal({op: Binary(Add), values: [VInt(i1), VInt(i2)]}),
        op_ctxts,
      },
      env,
      store,
      glob,
    } =>
    Some({
      programZipper,
      workspaceZipper: {
        workspaceFocus: Value(VInt(i1 + i2)),
        op_ctxts,
      },
      env,
      store,
      glob,
    })
  /* PROGRAM */
  /* enter. zipper boilerplate */
  | {
      programZipper: {programFocus: Program({stmts: [s, ...stmts]}), prog_ctxts: []},
      workspaceZipper,
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: Stmt(s),
        prog_ctxts: [StmtsCtxt({pre: stmts, post: []})],
      },
      workspaceZipper,
      env,
      store,
      glob,
    })
  /* continue. zipper boilerplate */
  | {
      programZipper: {
        programFocus: PostStmt(s),
        prog_ctxts: [StmtsCtxt({pre: [s', ...pre], post})],
      },
      workspaceZipper,
      env,
      store,
      glob,
    } =>
    Some({
      programZipper: {
        programFocus: Stmt(s'),
        prog_ctxts: [StmtsCtxt({pre, post: [s, ...post]})],
      },
      workspaceZipper,
      env,
      store,
      glob,
    })
  /* TODO: end? */
  | _ => None
  };

let liftExpr = (e: exp): program => {stmts: [{op: Stmt(ExprStmt), args: [e]}]};

let inject = (p: program): config => {
  programZipper: {
    programFocus: Program(p),
    prog_ctxts: [],
  },
  workspaceZipper: {
    workspaceFocus: Empty,
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
   | {zipper: {workspaceFocus: Value(_), op_ctxts: []}, env: _, stack: []} => true
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

let interpretTrace = (p: program): list(config) => {
  let states = iterateMaybe(step, inject(p));
  /* takeWhileInclusive(c => !isFinal(c), states) */
  states;
};
