open Sidewinder;
open Bobcat;
open MiniPython;

let vizUnaryOp = (uo: unary_op) =>
  switch (uo) {
  | Neg => Some(ConfigIR.mk(~name="Neg", ~nodes=[], ~render=_ => Theia.str("-"), ()))
  };

let vizBinaryOp = (bo: binary_op) =>
  switch (bo) {
  | Add => Some(ConfigIR.mk(~name="Add", ~nodes=[], ~render=_ => Theia.str("+"), ()))
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
        ~nodes=[vizUnaryExpr(unary_expr)],
        ~render=([ue]) => Theia.noOp(ue, []),
        (),
      ),
    )
  | BinaryExpr(binary_expr) =>
    Some(
      ConfigIR.mk(
        ~name="BinaryExpr",
        ~nodes=[vizBinaryExpr(binary_expr)],
        ~render=([be]) => Theia.noOp(be, []),
        (),
      ),
    )
  }

and vizUnaryExpr = ({unary_op, operand}: unary_expr) =>
  Some(
    ConfigIR.mk(
      ~name="unary_expr",
      ~nodes=[vizUnaryOp(unary_op), vizExp(operand)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizBinaryExpr = ({left, binary_op, right}: binary_expr) =>
  Some(
    ConfigIR.mk(
      ~name="unary_expr",
      ~nodes=[vizExp(left), vizBinaryOp(binary_op), vizExp(right)],
      ~render=Theia.hSeq,
      (),
    ),
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

let vizUnaryPreval = ({unary_op, operand}: unary_preval) =>
  Some(
    ConfigIR.mk(
      ~name="unary_preval",
      ~nodes=[vizUnaryOp(unary_op), vizValue(operand)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizBinaryPreval = ({left, binary_op, right}: binary_preval) =>
  Some(
    ConfigIR.mk(
      ~name="binary_preval",
      ~nodes=[vizValue(left), vizBinaryOp(binary_op), vizValue(right)],
      ~render=Theia.hSeq,
      (),
    ),
  );

let vizPreVal = (pv: preval) =>
  switch (pv) {
  | PVUnary(up) =>
    Some(
      ConfigIR.mk(
        ~name="PVUnary",
        ~nodes=[vizUnaryPreval(up)],
        ~render=([up]) => Theia.noOp(up, []),
        (),
      ),
    )
  | PVBinary(bp) =>
    Some(
      ConfigIR.mk(
        ~name="PVBinary",
        ~nodes=[vizBinaryPreval(bp)],
        ~render=([bp]) => Theia.noOp(bp, []),
        (),
      ),
    )
  };

let vizFocus = (f: focus) =>
  switch (f) {
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

let vizZipper = ({focus, ctxts}: zipper) =>
  Some(
    ConfigIR.mk(
      ~name="zipper",
      ~nodes=[vizFocus(focus)],
      ~render=([focus]) => Theia.noOp(focus, []),
      (),
    ),
  );

let vizConfig = ({zipper, env, store, glob}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[vizZipper(zipper)],
    ~render=([zipper]) => Theia.noOp(zipper, []),
    (),
  );
