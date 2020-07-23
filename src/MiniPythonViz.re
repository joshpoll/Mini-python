open Sidewinder;
open Bobcat;
open MiniPython;

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
  | ExprStmt =>
    Some(
      ConfigIR.mk(~name="ExprStmt", ~nodes=[None], ~render=([e]) => Theia.noOp(e, []), ()),
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

let vizOpCtxt = ({op, args, values}: unary_ctxt) =>
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

let vizCtxt = (c: ctxt) =>
  switch (c) {
  | OpCtxt(oc) =>
    Some(
      ConfigIR.mk(
        ~name="OpCtxt",
        ~nodes=[vizOpCtxt(oc)],
        ~render=([oc]) => Theia.noOp(oc, []),
        (),
      ),
    )
  | Program(pc) => failwith("TODO")
  | Stmt(sc) =>
    Some(
      ConfigIR.mk(
        ~name="StmtCtxt",
        ~nodes=[vizOpCtxt(sc)],
        ~render=([sc]) => Theia.noOp(sc, []),
        (),
      ),
    )
  };

let rec vizCtxts = (ctxts: ctxts) =>
  switch (ctxts) {
  | [] => Some(ConfigIR.mk(~name="ctxts_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [c, ...ctxts] =>
    Some(
      ConfigIR.mk(
        ~name="ctxts_cons",
        ~nodes=[vizCtxt(c), vizCtxts(ctxts)],
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

let vizFocus = (f: focus) =>
  switch (f) {
  | Empty => Some(ConfigIR.mk(~name="focus_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | Program(p) => failwith("TODO")
  | Stmt(s) =>
    Some(
      ConfigIR.mk(
        ~name="focus_stmt",
        ~nodes=[vizStmt(s)],
        ~render=([s]) => Theia.noOp(s, []),
        (),
      ),
    )
  | Exp(e) =>
    Some(
      ConfigIR.mk(
        ~name="focus_exp",
        ~nodes=[vizExp(e)],
        ~render=([e]) => Theia.noOp(e, []),
        (),
      ),
    )
  | PreVal(pv) =>
    Some(
      ConfigIR.mk(
        ~name="focus_preval",
        ~nodes=[vizPreVal(pv)],
        ~render=([pv]) => Theia.noOp(pv, []),
        (),
      ),
    )
  | Value(v) =>
    Some(
      ConfigIR.mk(
        ~name="focus_value",
        ~nodes=[vizValue(v)],
        ~render=([v]) => Theia.noOp(v, []),
        (),
      ),
    )
  };

let vizWorkspaceZipper = ({focus, op_ctxts}: workspaceZipper) =>
  Some(
    ConfigIR.mk(
      ~name="workspaceZipper",
      ~nodes=[vizFocus(focus), vizOpCtxts(op_ctxts)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizConfig = ({workspaceZipper, env, store, glob}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[vizWorkspaceZipper(workspaceZipper)],
    ~render=([z]) => Theia.noOp(z, []),
    (),
  );
