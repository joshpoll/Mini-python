// Entry point

[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let style = document##createElement("style");
document##head##appendChild(style);
style##innerHTML #= ExampleStyles.style;

let makeContainer = text => {
  let container = document##createElement("div");
  container##className #= "container";

  let title = document##createElement("div");
  title##className #= "containerTitle";
  title##innerText #= text;

  let content = document##createElement("div");
  content##className #= "containerContent";

  let () = container##appendChild(title);
  let () = container##appendChild(content);
  let () = document##body##appendChild(container);

  content;
};

// let id = x => ZEDLang.Lam({vid: x, exp: Lift(Var(x))});

// ReactDOMRe.render(
//   <VizTrace
//     program={ZEDLang.Let("x", Num(5), Lift(Add(App(id("y"), Var("x")), Num(1))))}
//   />,
//   makeContainer("Demo. See state 17."),
// );

// ReactDOMRe.render(
//   <VizTrace
//     program={
//       ZEDLang.Let("x", Num(5), Lift(Add(Add(App(id("y"), Var("x")), Num(1)), Num(2))))
//     }
//   />,
//   makeContainer("Nested continuations on stack"),
// );

// ReactDOMRe.render(
//   <VizTrace program={ZEDLang.Let("x", Num(5), Lift(Var("x")))} />,
//   makeContainer("Variable Lookup"),
// );

// ReactDOMRe.render(
//   <VizTrace program={ZEDLang.Lift(Add(Num(1), Num(2)))} />,
//   makeContainer("Add"),
// );

// ReactDOMRe.render(<VizTrace program={ZEDLang.Lift(Num(1))} />, makeContainer("Lift(ae)"));

// ReactDOMRe.render(
//   <VizTrace program={ZEDLang.Lift(Bracket(Lift(Num(1))))} />,
//   makeContainer("Lift(Bracket(Lift(ae)))"),
// );

/* ENONE */
ReactDOMRe.render(
  <VizTrace program={MiniPython.liftExpr(MiniPython.NoneLiteral)} />,
  makeContainer("NoneLiteral"),
);

/* BOOL-FALSE */
ReactDOMRe.render(
  <VizTrace program={MiniPython.liftExpr(MiniPython.BooleanLiteral(false))} />,
  makeContainer("False"),
);

/* BOOL-TRUE */
ReactDOMRe.render(
  <VizTrace program={MiniPython.liftExpr(MiniPython.BooleanLiteral(true))} />,
  makeContainer("True"),
);

/* INT */
ReactDOMRe.render(
  <VizTrace program={MiniPython.liftExpr(MiniPython.IntegerLiteral(5))} />,
  makeContainer("IntegerLiteral"),
);

/* STR */
ReactDOMRe.render(
  <VizTrace program={MiniPython.liftExpr(MiniPython.StringLiteral("Hello"))} />,
  makeContainer("StringLiteral"),
);

/* NEG */
ReactDOMRe.render(
  <VizTrace
    program={MiniPython.liftExpr(
      MiniPython.UnaryExpr({op: Unary(Neg), args: [IntegerLiteral(1)]}),
    )}
  />,
  makeContainer("NEG"),
);

/* ARITH: + */
ReactDOMRe.render(
  <VizTrace
    program={MiniPython.liftExpr(
      MiniPython.BinaryExpr({op: Binary(Add), args: [IntegerLiteral(1), IntegerLiteral(2)]}),
    )}
  />,
  makeContainer("ARITH: +"),
);

/* multiple statements */
ReactDOMRe.render(
  <VizTrace
    program=MiniPython.{
      stmts: [
        {
          op: Stmt(ExprStmt),
          args: [UnaryExpr({op: Unary(Neg), args: [IntegerLiteral(1)]})],
        },
        {
          op: Stmt(ExprStmt),
          args: [
            BinaryExpr({op: Binary(Add), args: [IntegerLiteral(1), IntegerLiteral(2)]}),
          ],
        },
      ],
    }
  />,
  makeContainer("multiple statements"),
);

/* PASS */
ReactDOMRe.render(
  <VizTrace program=MiniPython.{stmts: [{op: Stmt(Pass), args: []}]} />,
  makeContainer("PASS"),
);

/* VAR-INIT, VAR-READ, VAR-ASSIGN-STMT */
ReactDOMRe.render(
  <VizTrace
    program=MiniPython.{
      stmts: [
        {op: Stmt(AssignStmt("x")), args: [IntegerLiteral(5)]},
        {op: Stmt(ExprStmt), args: [Identifier("x")]},
        {op: Stmt(AssignStmt("x")), args: [IntegerLiteral(6)]},
        {op: Stmt(ExprStmt), args: [Identifier("x")]},
      ],
    }
  />,
  makeContainer("var"),
);

/* mini python tutor example */
ReactDOMRe.render(
  <VizTrace
    program=MiniPython.{
      stmts: [
        {op: Stmt(AssignStmt("x")), args: [IntegerLiteral(1)]},
        {op: Stmt(AssignStmt("y")), args: [IntegerLiteral(4)]},
        {op: Stmt(AssignStmt("z")), args: [Identifier("y")]},
        {op: Stmt(AssignStmt("y")), args: [Identifier("x")]},
        {op: Stmt(AssignStmt("x")), args: [Identifier("z")]},
      ],
    }
  />,
  makeContainer("var"),
);
