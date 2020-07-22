open Sidewinder.ConfigGraphIR;

let rec mergeNone =
        (
          l1: list(option(Sidewinder.ConfigGraphIR.node)),
          l2: list(option(Sidewinder.ConfigGraphIR.node)),
        ) =>
  switch (l1, l2) {
  | (l1, []) => l1
  | ([None, ...l1], [x, ...l2]) => [x, ...mergeNone(l1, l2)]
  | ([Some(x), ...l1], l2) => [Some(x), ...mergeNone(l1, l2)]
  | ([], l2) =>
    Js.log(l2 |> Array.of_list);
    failwith("Some nodes remaining!");
  };

let rec expsToList = ({name, nodes} as exps) =>
  if (name == "exps_empty") {
    [];
  } else if (name == "exps_cons") {
    let [Some(exp), Some(exps)] = nodes;
    [Some(exp), ...expsToList(exps)];
  } else {
    Js.log2("expected exps_empty or exps_cons. found", name);
    assert(false);
  };

let rec valuesToList = ({name, nodes} as values) =>
  if (name == "values_empty") {
    [];
  } else if (name == "values_cons") {
    let [Some(value), Some(values)] = nodes;
    [Some(value), ...valuesToList(values)];
  } else {
    Js.log2("expected values_empty or values_cons. found", name);
    assert(false);
  };

let rec transformOpExprOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformOpExprOption, n.nodes)};
    if (name == "op_expr") {
      let [Some({nodes}), Some(args)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let args = expsToList(args);
      Some({...op, nodes: nodes->mergeNone(args)});
    } else {
      Some(n);
    };
  };

let transformOpExpr = n => transformOpExprOption(Some(n))->Belt.Option.getExn;

let rec transformOpCtxtOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformOpCtxtOption, n.nodes)};
    if (name == "op_ctxt") {
      let [Some({nodes}), Some(args), None, Some(values)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let args = expsToList(args);
      let values = valuesToList(values);
      Some({...op, nodes: nodes->mergeNone(values)->mergeNone([None, ...args])});
    } else {
      Some(n);
    };
  };

let transformOpCtxt = n => transformOpCtxtOption(Some(n))->Belt.Option.getExn;

let rec transformOpPrevalOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformOpPrevalOption, n.nodes)};
    if (name == "op_preval") {
      let [Some({nodes}), Some(values)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let values = valuesToList(values) /*  |> List.rev */;
      Some({...op, nodes: nodes->mergeNone(values)});
    } else {
      Some(n);
    };
  };

let transformOpPreval = n => transformOpPrevalOption(Some(n))->Belt.Option.getExn;

let rec ctxtsToList = ({name, nodes} as ctxts) =>
  if (name == "ctxts_empty") {
    [];
  } else if (name == "ctxts_cons") {
    let [Some(ctxt), Some(ctxts)] = nodes;
    [Some(ctxt), ...ctxtsToList(ctxts)];
  } else {
    Js.log2("expected ctxts_empty or ctxts_cons. found", name);
    assert(false);
  };

let rec zipUp =
        (
          f: option(Sidewinder.ConfigGraphIR.node),
          cs: list(option(Sidewinder.ConfigGraphIR.node)),
        ) =>
  switch (cs) {
  | [] => f
  | [c, ...cs] =>
    switch (c) {
    | None => None
    | Some(c) =>
      let place =
        switch (f) {
        | None => {pat: None, extFns: []}
        | Some(f) =>
          switch (f.place.pat) {
          | None => {pat: None, extFns: f.place.extFns}
          /* TODO: need to add into flow */
          | Some(place) => {pat: Some(place ++ ".highlight"), extFns: f.place.extFns}
          }
        };
      let f =
        Some(
          Sidewinder.ConfigGraphIR.mk(
            /* TODO: add to flow!!! */
            /* ~place?, */
            ~name="highlight",
            ~nodes=[f],
            ~render=([f]) => Bobcat.Theia.highlight(~fill="hsla(240, 100%, 80%, 33%)", f, []),
            (),
          ),
        );
      zipUp(Some({...c, nodes: mergeNone(c.nodes, [f])}), cs);
    }
  };

let rec transformZipperOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformZipperOption, n.nodes)};
    if (name == "zipper") {
      let [f, Some(ctxts)] = nodes;
      let ctxts = ctxtsToList(ctxts);
      zipUp(f, ctxts);
    } else {
      Some(n);
    };
  };

let transformZipper = n => transformZipperOption(Some(n))->Belt.Option.getExn;

// let rec transformContinuationOption = on =>
//   switch (on) {
//   | None => None
//   | Some(n) =>
//     let {name, nodes} as n = {...n, nodes: List.map(transformContinuationOption, n.nodes)};
//     if (name == "frame") {
//       let [env, Some(ctxts)] = nodes;
//       let ctxts = ctxtsToList(ctxts);
//       Some({...n, nodes: [env, zipUp(None, ctxts)]});
//     } else {
//       Some(n);
//     };
//   };

// let transformContinuation = n => transformContinuationOption(Some(n))->Belt.Option.getExn;
