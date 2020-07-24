# ReasonReact Template & Examples

This is:
- A template for your new ReasonReact project.
- A collection of thin examples illustrating ReasonReact usage.
- Extra helper documentation for ReasonReact (full ReasonReact docs [here](https://reasonml.github.io/reason-react/)).

`src` contains 4 sub-folders, each an independent, self-contained ReasonReact example. Feel free to delete any of them and shape this into your project! This template's more malleable than you might be used to =).

The point of this template and examples is to let you understand and personally tweak the entirely of it. We **don't** give you an opaque, elaborate mega build setup just to put some boxes on the screen. It strikes to stay transparent, learnable, and simple. You're encouraged to read every file; it's a great feeling, having the full picture of what you're using and being able to touch any part.

## Run

```sh
npm install
npm run server
# in a new tab
npm start
```

Open a new web page to `http://localhost:8000/`. Change any `.re` file in `src` to see the page auto-reload. **You don't need any bundler when you're developing**!

**How come we don't need any bundler during development**? We highly encourage you to open up `index.html` to check for yourself!

# Features Used

|                           | Blinking Greeting | Reducer from ReactJS Docs | Fetch Dog Pictures | Reason Using JS Using Reason |
|---------------------------|-------------------|---------------------------|--------------------|------------------------------|
| No props                  |                   | ✓                         |                    |                              |
| Has props                 |                   |                           |                    | ✓                            |
| Children props            | ✓                 |                           |                    |                              |
| No state                  |                   |                           |                    | ✓                            |
| Has state                 | ✓                 |                           |  ✓                 |                              |
| Has state with useReducer |                   | ✓                         |                    |                              |
| ReasonReact using ReactJS |                   |                           |                    | ✓                            |
| ReactJS using ReasonReact |                   |                           |                    | ✓                            |
| useEffect                 | ✓                 |                           |  ✓                 |                              |
| Dom attribute             | ✓                 | ✓                         |                    | ✓                            |
| Styling                   | ✓                 | ✓                         |  ✓                 | ✓                            |
| React.array               |                   |                           |  ✓                 |                              |

# Bundle for Production

We've included a convenience `UNUSED_webpack.config.js`, in case you want to ship your project to production. You can rename and/or remove that in favor of other bundlers, e.g. Rollup.

We've also provided a barebone `indexProduction.html`, to serve your bundle.

```sh
npm install webpack webpack-cli
# rename file
mv UNUSED_webpack.config.js webpack.config.js
# call webpack to bundle for production
./node_modules/.bin/webpack
open indexProduction.html
```

# Handle Routing Yourself

To serve the files, this template uses a minimal dependency called `moduleserve`. A URL such as `localhost:8000/scores/john` resolves to the file `scores/john.html`. If you'd like to override this and handle URL resolution yourself, change the `server` command in `package.json` from `moduleserve ./ --port 8000` to `moduleserve ./ --port 8000 --spa` (for "single page application"). This will make `moduleserve` serve the default `index.html` for any URL. Since `index.html` loads `Index.bs.js`, you can grab hold of the URL in the corresponding `Index.re` and do whatever you want.

By the way, ReasonReact comes with a small [router](https://reasonml.github.io/reason-react/docs/en/router) you might be interested in.

# ChocoPy Rules

**Goal: Implement through Stage 5 by August 11.**

## Stage 1
- [x] NONE (Grace)
- [x] BOOL-FALSE (Grace)
- [x] BOOL-TRUE (Grace)
- [x] INT (Grace)
- [x] STR (Grace)

## Stage 2
- [ ] PASS (Josh)
- [x] EXPR-STMT (Josh)
- [ ] VAR-READ (Josh)
- [ ] VAR-ASSIGN-STMT (Josh)
- [x] NEGATE (Josh)
- [ ] ARITH (Grace)
- [ ] INT-COMPARE (Grace)
- [ ] BOOL-COMPARE (Grace)
- [ ] STR-COMPARE (Grace)
- [ ] STR-CONCAT (Grace)
- [ ] STR-SELECT (Grace)
- [ ] IS (??)
- [ ] NOT (Grace)

## Stage 3: Boolean Expressions
- [ ] AND-1 (Josh)
- [ ] AND-2 (Josh)
- [ ] OR-1 (Grace)
- [ ] OR-2 (Grace)
- [ ] IF-ELSE-EXPR-TRUE (??)
- [ ] IF-ELSE-EXPR-FALSE (??)

## Stage 4: If Statements, While Loops, and I/O
- [ ] IF-ELSE-TRUE
- [ ] IF-ELSE-FALSE
- [ ] IF-ELIF-TRUE
- [ ] IF-ELIF-FALSE
- [ ] IF-NO-ELSE
- [ ] WHILE-FALSE
- [ ] WHILE-TRUE-LOOP
- [ ] PRINT
- [ ] INPUT

## Stage 5: Functions
- [ ] FUNC-METHOD-DEF
- [ ] INVOKE
- [ ] WHILE-TRUE-RETURN
- [ ] RETURN-E
- [ ] RETURN
- [ ] STMT-SEQ
- [ ] STMT-SEQ-RETURN
- [ ] PROGRAM

## Stage 6: Objects
- [ ] class pre-processing? Chocopy creates classes at compile time I think...
- [ ] DISPATCH
- [ ] ATTR-READ
- [ ] ATTR-ASSIGN-STMT
- [ ] NEW

## Stage 7: Lists and For Loops
- [ ] LIST-DISPLAY
- [ ] LIST-SELECT
- [ ] LIST-CONCAT
- [ ] LIST-ASSIGN-STMT
- [ ] MULTI-ASSIGN-STMT
- [ ] LEN-LIST
- [ ] LEN-STR
- [ ] FOR-LIST
- [ ] FOR-LIST-RETURN
- [ ] FOR-STR
- [ ] FOR-STR-RETURN