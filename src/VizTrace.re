let leftButtonStyle = ReactDOMRe.Style.make(~borderRadius="4px 0px 0px 4px", ~width="48px", ());
let rightButtonStyle = ReactDOMRe.Style.make(~borderRadius="0px 4px 4px 0px", ~width="48px", ());

type state = {
  renderedNodes: list(React.element),
  pos: int,
  length: int,
  pointers: bool,
};

type action =
  | Increment
  | Decrement
  | Length(int)
  | SetNodes(list(React.element))
  | TogglePointers
  | Error;

let initialState = {
  renderedNodes: [<g> {React.string("Loading...")} </g>],
  pos: 0,
  length: 1,
  pointers: false,
};

let reducer = (state, action) => {
  switch (action) {
  | Increment => {...state, pos: min(state.length - 1, state.pos + 1)}
  | Decrement => {...state, pos: max(0, state.pos - 1)}
  | Length(length) => {...state, length}
  | SetNodes(renderedNodes) => {...state, renderedNodes}
  | TogglePointers => {...state, pointers: !state.pointers}
  | Error => state
  };
};

module Animation = Sidewinder.AnimationComponentHelper;
module AnimationProvider = Sidewinder.AnimationComponentProvider;

let toggle = (Animation.{curr: _, next}) =>
  switch (next) {
  | Before => Animation.{curr: next, next: After}
  | After => {curr: next, next: Before}
  };

let transform = (~pointers=false, n) =>
  n
  |> MiniPythonTransform.transformOpExpr
  |> MiniPythonTransform.transformOpCtxt
  |> MiniPythonTransform.transformOpPreval
  |> MiniPythonTransform.transformWorkspaceZipper
  |> MiniPythonTransform.transformProgCtxt
  |> MiniPythonTransform.transformProgramZipper
  |> (
    if (pointers) {
      MiniPythonTransform.transformRefs;
    } else {
      x => x;
    }
  );

// |> ZEDTransform.transformContinuation;

[@react.component]
let make = (~continuity=true, ~padding=10., ~program) => {
  let nodes = MiniPython.interpretTrace(program);

  let (state, dispatch) = React.useReducer(reducer, initialState);
  let (Animation.{curr, next}, setAnimationState) = React.useState(() => Animation.initValue);

  // Notice that instead of `useEffect`, we have `useEffect0`. See
  // reasonml.github.io/reason-react/docs/en/components#hooks for more info
  React.useEffect1(
    () => {
      dispatch(Length(List.length(nodes)));

      let nodes: list(Sidewinder.ConfigGraphIR.node) =
        nodes
        |> List.map(MiniPythonViz.vizConfig)
        |> List.map(Sidewinder.ToConfigGraph.lower)
        |> List.map(Sidewinder.PropagatePlace.convert(Sidewinder.Flow.none))
        |> List.map(((_, n)) => n)
        |> List.map(transform(~pointers=state.pointers));

      let finalNode = nodes |> List.rev |> List.hd;
      let finalState =
        Sidewinder.Config.compileTransition(finalNode, Sidewinder.Flow.none, finalNode);

      dispatch(
        SetNodes(
          Bobcat.Fn.mapPairs(
            (n1, n2) => Sidewinder.Config.compileTransition(n1, Sidewinder.Flow.none, n2),
            nodes,
          )
          @ [finalState],
        ),
      );

      // Returning None, instead of Some(() => ...), means we don't have any
      // cleanup to do before unmounting. That's not 100% true. We should
      // technically cancel the promise. Unofortunately, there's currently no
      // way to cancel a promise. Promises in general should be way less used
      // for React components; but since folks do use them, we provide such an
      // example here. In reality, this fetch should just be a plain callback,
      // with a cancellation API
      None;
    },
    [|state.pointers|],
  );

  let renderedConfig = List.nth(state.renderedNodes, state.pos);
  let width = 1000.;
  let height = 300.;
  let xOffset = 0.;
  let yOffset = 100.;
  /* let width = renderedConfig.bbox->Sidewinder.Rectangle.width;
     let height = renderedConfig.bbox->Sidewinder.Rectangle.height; */

  /* /* transform is unnecessary b/c top-level always has identity transform b/c parent controls transform */
     let xOffset =
       /* renderedConfig.transform.translate.x +.  */ renderedConfig.bbox->Sidewinder.Rectangle.x1;
     let yOffset = /* renderedConfig.transform.translate.y +. */
     renderedConfig.bbox->Sidewinder.Rectangle.y1; */
  <div>
    <div> {React.string("state: ")} {React.string(string_of_int(state.pos))} </div>
    <button
      style=leftButtonStyle
      onClick={_event => {
        dispatch(Decrement);
        setAnimationState(_ => Animation.initValue);
      }}>
      {React.string("<-")}
    </button>
    <button
      style=rightButtonStyle
      onClick={_event => {
        dispatch(Increment);
        setAnimationState(_ => Animation.initValue);
      }}>
      {React.string("->")}
    </button>
    <button onClick={_event => {dispatch(TogglePointers)}}>
      {if (state.pointers) {
         React.string("use pointer ids");
       } else {
         React.string("use reference arrows");
       }}
    </button>
    <br />
    <br />
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={Js.Float.toString(width +. padding *. 2.)}
      height={Js.Float.toString(height +. padding *. 2.)}>
      /* arrowhead styling */

        <defs>
          <marker
            id="arrowhead" markerWidth="10" markerHeight="7" refX="0" refY="3.5" orient="auto">
            <polygon
              points="0 0, 10 3.5, 0 7"
              style={ReactDOMRe.Style.make(~fill="#85C1E9", ())}
            />
          </marker>
        </defs>
        <g
          transform={
            "translate("
            ++ Js.Float.toString(xOffset +. padding)
            ++ ", "
            ++ Js.Float.toString(yOffset +. padding)
            ++ ")"
          }>
          <AnimationProvider value=Animation.{curr, next}> renderedConfig </AnimationProvider>
        </g>
      </svg>
  </div>;
  // };
  /* ->Belt.Array.mapWithIndex((i, dog) => {
              let imageStyle =
                ReactDOMRe.Style.make(
                  ~height="120px",
                  ~width="100%",
                  ~marginRight=i === Js.Array.length(dogs) - 1 ? "0px" : "8px",
                  ~borderRadius="8px",
                  ~boxShadow="0px 4px 16px rgb(200, 200, 200)",
                  ~backgroundSize="cover",
                  ~backgroundImage={j|url($dog)|j},
                  ~backgroundPosition="center",
                  (),
                );
              <div key=dog style=imageStyle />;
            })
          ->React.array
        }}
     */
};
