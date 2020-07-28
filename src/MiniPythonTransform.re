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

let rec op_ctxtsToList = ({name, nodes} as op_ctxts) =>
  if (name == "op_ctxts_empty") {
    [];
  } else if (name == "op_ctxts_cons") {
    let [Some(op_ctxt), Some(op_ctxts)] = nodes;
    [Some(op_ctxt), ...op_ctxtsToList(op_ctxts)];
  } else {
    Js.log2("expected op_ctxts_empty or op_ctxts_cons. found", name);
    assert(false);
  };

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
            ~links=[],
            ~render=([f]) => Bobcat.Theia.highlight(~fill="hsla(240, 100%, 80%, 33%)", f, []),
            (),
          ),
        );
      zipUp(Some({...c, nodes: mergeNone(c.nodes, [f])}), cs);
    }
  };

let rec transformWorkspaceZipperOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformWorkspaceZipperOption, n.nodes)};
    if (name == "workspaceZipper") {
      let [f, Some(op_ctxts)] = nodes;
      let op_ctxts = op_ctxtsToList(op_ctxts);
      zipUp(f, op_ctxts);
    } else {
      Some(n);
    };
  };

let transformWorkspaceZipper = n => transformWorkspaceZipperOption(Some(n))->Belt.Option.getExn;

let rec prog_ctxtsToList = ({name, nodes} as prog_ctxts) =>
  if (name == "prog_ctxts_empty") {
    [];
  } else if (name == "prog_ctxts_cons") {
    let [Some(prog_ctxt), Some(prog_ctxts)] = nodes;
    [Some(prog_ctxt), ...prog_ctxtsToList(prog_ctxts)];
  } else {
    Js.log2("expected prog_ctxts_empty or prog_ctxts_cons. found", name);
    assert(false);
  };

let rec transformProgCtxtOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformProgCtxtOption, n.nodes)};
    let n =
      if (Js.String.startsWith("prog_ctxt.", name)) {
        List.hd(nodes);
      } else {
        Some(n);
      };
    n;
  };

let transformProgCtxt = n => transformProgCtxtOption(Some(n))->Belt.Option.getExn;

let rec transformProgramZipperOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformProgramZipperOption, n.nodes)};
    if (name == "programZipper") {
      // Js.log2("programZipper nodes", nodes |> Array.of_list);
      let [f, Some(prog_ctxts)] = nodes;
      /* TODO: list converter probably wrong b/c there is a level of indirection */
      let prog_ctxts = prog_ctxtsToList(prog_ctxts);
      Js.log2("f", f);
      Js.log2("prog_ctxts", prog_ctxts |> Array.of_list);
      zipUp(f, prog_ctxts);
    } else {
      Some(n);
    };
  };

let transformProgramZipper = n => transformProgramZipperOption(Some(n))->Belt.Option.getExn;

let rec storeToList = ({name, nodes} as store) =>
  if (name == "store_empty") {
    [];
  } else if (name == "store_bind") {
    let [Some(binding), Some(store)] = nodes;
    [Some(binding), ...storeToList(store)];
  } else {
    Js.log2("expected store_empty or store_bind. found", name);
    assert(false);
  };

let rec envToList = ({name, nodes} as env) =>
  if (name == "env_empty") {
    [];
  } else if (name == "env_bind") {
    let [Some(binding), Some(env)] = nodes;
    [Some(binding), ...envToList(env)];
  } else {
    Js.log2("expected env_empty or env_bind. found", name);
    assert(false);
  };

let rec envFromList =
        (env: list(option(Sidewinder.ConfigGraphIR.node))): Sidewinder.ConfigGraphIR.node =>
  switch (env) {
  | [] =>
    Sidewinder.ConfigGraphIR.mk(
      ~name="env_empty",
      ~nodes=[],
      ~links=[],
      ~render=_ => Bobcat.Theia.str("env"),
      (),
    )
  | [binding, ...env] =>
    Sidewinder.ConfigGraphIR.mk(
      ~name="env_bind",
      ~nodes=[binding, Some(envFromList(env))],
      ~links=[],
      ~render=([b, env]) => Bobcat.Theia.vSeq([env, b]),
      (),
    )
  };

let getDstRefUIDs = n => {
  let Some(store) = List.nth(n.nodes, 3);
  let store = storeToList(store);
  let locUIDMap =
    List.map(
      (Some({nodes})) => {
        let Some(loc) = List.hd(nodes);
        (loc.name |> Js.String.sliceToEnd(~from=4), loc.uid);
      },
      store,
    );
  locUIDMap;
};

let rec addEnvLinks = (locUIDMap, {name, nodes} as env) =>
  if (name == "env_empty") {
    env;
  } else if (name == "env_bind") {
    let [Some(binding), Some(env')] = nodes;

    let Some(loc) = List.nth(binding.nodes, 1);
    let lookup =
      locUIDMap
      ->Belt.List.getAssoc(loc.name |> Js.String.sliceToEnd(~from=4), (==))
      ->Belt.Option.getExn;
    /* compute the link */
    let link =
      Bobcat.Link.{
        source: loc.uid,
        target: lookup,
        linkRender:
          Some(
            (~source, ~target) => {
              <line
                x1={Js.Float.toString(source->Bobcat.Rectangle.cx)}
                y1={Js.Float.toString(source->Bobcat.Rectangle.cy)}
                x2={Js.Float.toString(target->Bobcat.Rectangle.x1 -. 12.)}
                y2={Js.Float.toString(target->Bobcat.Rectangle.cy)}
                stroke="#85C1E9"
                markerEnd="url(#arrowhead)"
              />
            },
          ),
      };
    /* add the link to the binding */
    let nodes = [
      Some({
        ...binding,
        nodes: [
          List.hd(binding.nodes),
          Some({
            ...loc,
            render: _ =>
              Bobcat.Theia.atom(
                <circle cx="0" cy="0" r="2" stroke="#85C1E9" fill="#85C1E9" />,
                Bobcat.Rectangle.fromCenterPointSize(~cx=0., ~cy=0., ~width=5., ~height=5.),
              ),
          }),
        ],
        links: [link],
      }),
      Some(addEnvLinks(locUIDMap, env')),
    ];
    {...env, nodes};
  } else {
    Js.log2("expected env_empty or env_bind. found", name);
    assert(false);
  };

let transformRefs = n => {
  let locUIDMap = getDstRefUIDs(n);
  Js.log2("locUIDMap", locUIDMap |> Array.of_list);
  let nodes =
    List.mapi(
      (i, n) =>
        if (i == 2) {
          /* env */
          let Some(env) = n;
          let env = addEnvLinks(locUIDMap, env);
          Some(env);
        } else {
          n;
        },
      n.nodes,
    );
  // first getDstRefUIDs. This should be a map from a loc to a UID.
  // then go through source UIDs and add links to them by looking up their loc in the map.
  //
  // after that part works, we should see links superimposed on the existing visualization, so next
  // we want to rewrite it so that things we no longer care about are hidden.
  {...n, nodes};
};

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
