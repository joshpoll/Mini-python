open Sidewinder;
open Bobcat;
open MiniPython;

let vizId = (id: id) =>
  Some(ConfigIR.mk(~name="id", ~nodes=[], ~render=_ => Theia.str(id), ()));

let vizLoc = (loc: loc) =>
  Some(
    ConfigIR.mk(
      ~name="loc_" ++ string_of_int(loc),
      ~nodes=[],
      ~render=_ => Theia.str(string_of_int(loc)),
      (),
    ),
  );

let vizUnaryOp = (uo: unary_op) =>
  switch (uo) {
  | Neg =>
    Some(
      ConfigIR.mk(
        ~name="Neg",
        ~nodes=[None],
        ~render=
          ([x]) => Theia.hSeq(~gap=2., [Theia.str("-"), Theia.str("("), x, Theia.str(")")]),
        (),
      ),
    )
  };

let vizBinaryOp = (bo: binary_op) =>
  switch (bo) {
  | Add =>
    Some(
      ConfigIR.mk(
        ~name="Add",
        ~nodes=[None, None],
        ~render=
          ([x, y]) =>
            Theia.hSeq(
              ~gap=2.,
              [
                Theia.str("("),
                x,
                Theia.str(")"),
                Theia.str("+"),
                Theia.str("("),
                y,
                Theia.str(")"),
              ],
            ),
        (),
      ),
    )
  };

let vizStmtOp = (so: stmt_op) =>
  switch (so) {
  | Pass => Some(ConfigIR.mk(~name="Pass", ~nodes=[], ~render=_ => Theia.str("pass"), ()))
  | ExprStmt =>
    Some(
      ConfigIR.mk(~name="ExprStmt", ~nodes=[None], ~render=([e]) => Theia.noOp(e, []), ()),
    )
  | AssignStmt(id) =>
    Some(
      ConfigIR.mk(
        ~name="AssignStmt",
        ~nodes=[vizId(id), None],
        ~render=([id, e]) => Theia.hSeq([id, Theia.str("="), e]),
        (),
      ),
    )
  };

let vizOp = (o: op) =>
  switch (o) {
  | Unary(unary_op) =>
    Some(
      ConfigIR.mk(
        ~name="Unary",
        ~nodes=[vizUnaryOp(unary_op)],
        ~render=([uo]) => Theia.noOp(uo, []),
        (),
      ),
    )
  | Binary(binary_op) =>
    Some(
      ConfigIR.mk(
        ~name="Binary",
        ~nodes=[vizBinaryOp(binary_op)],
        ~render=([bo]) => Theia.noOp(bo, []),
        (),
      ),
    )
  | Stmt(stmt_op) =>
    Some(
      ConfigIR.mk(
        ~name="Binary",
        ~nodes=[vizStmtOp(stmt_op)],
        ~render=([bo]) => Theia.noOp(bo, []),
        (),
      ),
    )
  };

let rec vizExp = (e: exp) =>
  switch (e) {
  | NoneLiteral =>
    Some(ConfigIR.mk(~name="NoneLiteral", ~nodes=[], ~render=_ => Theia.str("NoneLiteral"), ()))
  | BooleanLiteral(false) =>
    Some(ConfigIR.mk(~name="False", ~nodes=[], ~render=_ => Theia.str("False"), ()))
  | BooleanLiteral(true) =>
    Some(ConfigIR.mk(~name="True", ~nodes=[], ~render=_ => Theia.str("True"), ()))
  | IntegerLiteral(int) =>
    Some(
      ConfigIR.mk(
        ~name="IntegerLiteral",
        ~nodes=[],
        ~render=_ => Theia.str(string_of_int(int)),
        (),
      ),
    )
  | StringLiteral(string) =>
    Some(ConfigIR.mk(~name="StringLiteral", ~nodes=[], ~render=_ => Theia.str(string), ()))
  | UnaryExpr(unary_expr) =>
    Some(
      ConfigIR.mk(
        ~name="UnaryExpr",
        ~nodes=[vizOpExpr(unary_expr)],
        ~render=([oe]) => Theia.noOp(oe, []),
        (),
      ),
    )
  | BinaryExpr(binary_expr) =>
    Some(
      ConfigIR.mk(
        ~name="BinaryExpr",
        ~nodes=[vizOpExpr(binary_expr)],
        ~render=([oe]) => Theia.noOp(oe, []),
        (),
      ),
    )
  | Identifier(id) =>
    Some(
      ConfigIR.mk(
        ~name="Identifier",
        ~nodes=[vizId(id)],
        ~render=([id]) => Theia.noOp(id, []),
        (),
      ),
    )
  }

and vizExps = (exps: list(exp)) =>
  switch (exps) {
  | [] => Some(ConfigIR.mk(~name="exps_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [exp, ...exps] =>
    Some(
      ConfigIR.mk(
        ~name="exps_cons",
        ~nodes=[vizExp(exp), vizExps(exps)],
        ~render=Theia.hSeq,
        (),
      ),
    )
  }

and vizOpExpr = ({op, args}: unary_expr) =>
  Some(
    ConfigIR.mk(~name="op_expr", ~nodes=[vizOp(op), vizExps(args)], ~render=Theia.hSeq, ()),
  );

let vizEnvBinding = ((id, loc): (id, loc)) =>
  Some(
    ConfigIR.mk(
      ~name="env_binding",
      ~nodes=[vizId(id), vizLoc(loc)],
      ~render=
        ([i, l]) =>
          Theia.hSeq([Theia.box(~dx=2., ~dy=2., i, []), Theia.box(~dx=2., ~dy=2., l, [])]),
      (),
    ),
  );

let rec vizEnv = (env: env) =>
  switch (env) {
  | [] => Some(ConfigIR.mk(~name="env_empty", ~nodes=[], ~render=_ => Theia.str("env"), ()))
  | [b, ...env] =>
    Some(
      ConfigIR.mk(
        ~name="env_bind",
        ~nodes=[vizEnvBinding(b), vizEnv(env)],
        ~render=([b, env]) => Theia.vSeq([env, b]),
        (),
      ),
    )
  };

let vizValue = (v: value) =>
  switch (v) {
  | VNone => Some(ConfigIR.mk(~name="VNone", ~nodes=[], ~render=_ => Theia.str("VNone"), ()))
  | VBool(false) =>
    Some(ConfigIR.mk(~name="false", ~nodes=[], ~render=_ => Theia.str("false"), ()))
  | VBool(true) =>
    Some(ConfigIR.mk(~name="true", ~nodes=[], ~render=_ => Theia.str("true"), ()))
  | VInt(int) =>
    Some(
      ConfigIR.mk(~name="VInt(int)", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()),
    )
  | VString(int, string) =>
    Some(
      ConfigIR.mk(
        ~name="VString(String.length(string), string)",
        ~nodes=[],
        ~render=
          _ =>
            Theia.box(
              Theia.hSeq([Theia.str(string_of_int(int)), Theia.str(", "), Theia.str(string)]),
              [],
            ),
        (),
      ),
    )
  };

let rec vizValues = (values: list(value)) =>
  switch (values) {
  | [] => Some(ConfigIR.mk(~name="values_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [value, ...values] =>
    Some(
      ConfigIR.mk(
        ~name="values_cons",
        ~nodes=[vizValue(value), vizValues(values)],
        ~render=Theia.hSeq,
        (),
      ),
    )
  };

let vizStoreBinding = ((loc, value): (loc, value)) =>
  Some(
    ConfigIR.mk(
      ~name="store_binding",
      ~nodes=[vizLoc(loc), vizValue(value)],
      ~render=
        ([l, v]) =>
          Theia.hSeq([Theia.box(~dx=2., ~dy=2., l, []), Theia.box(~dx=2., ~dy=2., v, [])]),
      (),
    ),
  );

let rec vizStore = (store: store) =>
  switch (store) {
  | [] => Some(ConfigIR.mk(~name="store_empty", ~nodes=[], ~render=_ => Theia.str("store"), ()))
  | [b, ...store] =>
    Some(
      ConfigIR.mk(
        ~name="store_bind",
        ~nodes=[vizStoreBinding(b), vizStore(store)],
        ~render=([b, store]) => Theia.vSeq([store, b]),
        (),
      ),
    )
  };

let vizStmt = s =>
  Some(
    ConfigIR.mk(
      ~name="Stmt",
      ~nodes=[vizOpExpr(s)],
      ~render=([oe]) => Theia.noOp(oe, []),
      (),
    ),
  );

let rec vizStmts = (stmts: list(stmt)) =>
  switch (stmts) {
  | [] => Some(ConfigIR.mk(~name="stmts_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [stmt, ...stmts] =>
    Some(
      ConfigIR.mk(
        ~name="stmts_cons",
        ~nodes=[vizStmt(stmt), vizStmts(stmts)],
        ~render=Theia.vSeq,
        (),
      ),
    )
  };

let vizProgram = ({stmts}: program) =>
  Some(
    ConfigIR.mk(
      ~name="program",
      ~nodes=[vizStmts(stmts)],
      ~render=([s]) => Theia.noOp(s, []),
      (),
    ),
  );

let vizOpCtxt = ({op, args, values}: op_ctxt) =>
  Some(
    ConfigIR.mk(
      ~name="op_ctxt",
      ~nodes=[vizOp(op), vizExps(args), None, vizValues(values)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let rec vizOpCtxts = (op_ctxts: op_ctxts) =>
  switch (op_ctxts) {
  | [] => Some(ConfigIR.mk(~name="op_ctxts_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [oc, ...op_ctxts] =>
    Some(
      ConfigIR.mk(
        ~name="op_ctxts_cons",
        ~nodes=[vizOpCtxt(oc), vizOpCtxts(op_ctxts)],
        ~render=Theia.vSeq,
        (),
      ),
    )
  };

let vizStmtsCtxt = ({pre, post}: stmts_ctxt) =>
  Some(
    ConfigIR.mk(
      ~name="stmts_ctxt",
      ~nodes=[vizStmts(post |> List.rev), None, vizStmts(pre)],
      ~render=Theia.vSeq,
      (),
    ),
  );

let vizProgCtxt = (pc: prog_ctxt) =>
  switch (pc) {
  | StmtCtxt(oc) =>
    Some(
      ConfigIR.mk(
        ~name="prog_ctxt.StmtCtxt",
        ~nodes=[vizOpCtxt(oc)],
        ~render=([oc]) => Theia.noOp(oc, []),
        (),
      ),
    )
  | StmtsCtxt(sc) =>
    Some(
      ConfigIR.mk(
        ~name="prog_ctxt.StmtsCtxt",
        ~nodes=[vizStmtsCtxt(sc)],
        ~render=([sc]) => Theia.noOp(sc, []),
        (),
      ),
    )
  };

let rec vizProgCtxts = (prog_ctxts: prog_ctxts) =>
  switch (prog_ctxts) {
  | [] => Some(ConfigIR.mk(~name="prog_ctxts_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [pc, ...prog_ctxts] =>
    Some(
      ConfigIR.mk(
        ~name="prog_ctxts_cons",
        ~nodes=[vizProgCtxt(pc), vizProgCtxts(prog_ctxts)],
        ~render=Theia.vSeq,
        (),
      ),
    )
  };

let vizOpPreval = ({op, values}: op_preval) =>
  Some(
    ConfigIR.mk(
      ~name="op_preval",
      ~nodes=[vizOp(op), vizValues(values)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizPreVal = (pv: preval) => vizOpPreval(pv);

let vizWorkspaceFocus = (wf: workspaceFocus) =>
  switch (wf) {
  | Empty =>
    Some(ConfigIR.mk(~name="workspaceFocus_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | Exp(e) =>
    Some(
      ConfigIR.mk(
        ~name="workspaceFocus_exp",
        ~nodes=[vizExp(e)],
        ~render=([e]) => Theia.noOp(e, []),
        (),
      ),
    )
  | PreVal(pv) =>
    Some(
      ConfigIR.mk(
        ~name="workspaceFocus_preval",
        ~nodes=[vizPreVal(pv)],
        ~render=([pv]) => Theia.noOp(pv, []),
        (),
      ),
    )
  | Value(v) =>
    Some(
      ConfigIR.mk(
        ~name="workspaceFocus_value",
        ~nodes=[vizValue(v)],
        ~render=([v]) => Theia.noOp(v, []),
        (),
      ),
    )
  };

let vizProgramFocus = (pf: programFocus) =>
  switch (pf) {
  | Program(p) =>
    Some(
      ConfigIR.mk(
        ~name="programFocus_program",
        ~nodes=[vizProgram(p)],
        ~render=([p]) => Theia.noOp(p, []),
        (),
      ),
    )
  | Stmt(s) =>
    Some(
      ConfigIR.mk(
        ~name="programFocus_stmt",
        ~nodes=[vizStmt(s)],
        ~render=([s]) => Theia.noOp(s, []),
        (),
      ),
    )
  | PostStmt(s) =>
    Some(
      ConfigIR.mk(
        ~name="programFocus_postStmt",
        ~nodes=[vizStmt(s)],
        ~render=([s]) => Theia.noOp(s, []),
        (),
      ),
    )
  | Exp(e) =>
    Some(
      ConfigIR.mk(
        ~name="programFocus_exp",
        ~nodes=[vizExp(e)],
        ~render=([e]) => Theia.noOp(e, []),
        (),
      ),
    )
  };

let vizWorkspaceZipper = ({workspaceFocus, op_ctxts}: workspaceZipper) =>
  Some(
    ConfigIR.mk(
      ~name="workspaceZipper",
      ~nodes=[vizWorkspaceFocus(workspaceFocus), vizOpCtxts(op_ctxts)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizProgramZipper = ({programFocus, prog_ctxts}: programZipper) =>
  Some(
    ConfigIR.mk(
      ~name="programZipper",
      ~nodes=[vizProgramFocus(programFocus), vizProgCtxts(prog_ctxts)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizConfig = ({programZipper, workspaceZipper, env, store, glob}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[
      vizProgramZipper(programZipper),
      vizWorkspaceZipper(workspaceZipper),
      vizEnv(env),
      vizStore(store),
    ],
    ~render=
      ([pz, wz, e, s]) =>
        Theia.hSeq(
          ~gap=20.,
          [Theia.box(~dx=10., ~dy=10., pz, []), Theia.box(~dx=10., ~dy=10., wz, []), e, s],
        ),
    (),
  );
