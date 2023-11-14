> 🐋 The API is quite big, so **expect lots of changes**! 🔨

# elm-review-codegen

There are many use-cases for generating elm code:

- `{|}` record field helpers, lenses: [hayleigh's elm-record-helpers](https://github.com/hayleigh-dot-dev/elm-record-helpers), ...
- `[n]` data structures of different sizes: [bburdette's toop](https://package.elm-lang.org/packages/bburdette/toop/latest/), [Chadtech's elm-vector](https://package.elm-lang.org/packages/Chadtech/elm-vector/latest/), [lue-bird's elm-bounded-nat Nats](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/Nats), [typesafe-array ArraySized.to-/l2-16](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/ArraySized#l1), ...
- `{,}` json (, bytes?, xml?, yaml?, ...) decoders, encoders, codecs: [dkodaj's decgen](https://github.com/dkodaj/decgen) with a CLI: [FranzSkuffka's elm-coder-generator](https://github.com/FranzSkuffka/elm-coder-generator), [alexkorban's json2elm](https://github.com/alexkorban/json2elm) & [its review rule](https://package.elm-lang.org/packages/alexkorban/elm-review-json-to-elm/latest/), [wscherphof's json-to-elm](https://github.com/wscherphof/json-to-elm), [spkerkela's elm-decoder-generator](https://git.spkerkela.com/spkerkela/elm-decoder-generator), [miniBill's elm-codec-generator](https://github.com/miniBill/elm-codec-generator), ...
- 🧪 tests from elm documentation examples: [stoeffel's elm-verify-examples](https://github.com/stoeffel/elm-verify-examples), ...
- `</>` html (, other html libs?): [dillonkearns's elm-review-html-to-elm](https://package.elm-lang.org/packages/dillonkearns/elm-review-html-to-elm/latest/), [korban's elmstatic](https://korban.net/elm/elmstatic/), [CodeTownOfficial's html-to-elm](https://github.com/CodeTownOfficial/html-to-elm) & [rubymaniac's vscode-html-to-elm](https://github.com/rubymaniac/vscode-html-to-elm), [pzavolinsky's elmx](https://github.com/pzavolinsky/elmx), [necinc's elmmet](https://github.com/necinc/elmmet)...
- 🖼️ svg (, typed-svg?, ...): [jackfranklin's svg-to-elm](https://github.com/jackfranklin/svg-to-elm), [ChristophP's parcel-transformer-elm-svg-modules](https://github.com/ChristophP/parcel-transformer-elm-svg-modules), [massimo-zaniboni's svg-to-elm](https://github.com/massimo-zaniboni/svg-to-elm), [pinata-llc's svg2elm](https://github.com/pinata-llc/svg2elm), [janwirth's svg-folder-to-elm-module](https://github.com/janwirth/svg-folder-to-elm-module) ...
- 💨 tailwind utilities: [matheus23's elm-tailwind-modules](https://github.com/matheus23/elm-tailwind-modules)
- ☊ graphql: [dillonkearns's elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/), [harmboschloo's graphql-to-elm](https://package.elm-lang.org/packages/harmboschloo/graphql-to-elm/latest/), ...
- ☁️ aws services: [the-sett's elm-aws-codegen](https://github.com/the-sett/elm-aws-codegen)
- 📃 [protobuf](https://developers.google.com/protocol-buffers/docs/overview): [feral-dot-io's protoc-gen-elmer](https://package.elm-lang.org/packages/feral-dot-io/protoc-gen-elmer/1.0.0/)
- ...

Elm code generation can be simplified! `elm-review-codegen` 🤜🤛 [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) 🤜🤛 [`elm-codegen`](https://github.com/mdgriffith/elm-codegen):

- run without separate command line tools or IDE plugins
- one simple way to configure, customize and create all code generators
- adding necessary imports, reporting failed/successful code generation etc. is handled automatically by [`elm-codegen`](https://github.com/mdgriffith/elm-codegen)

## configuration

```elm
module ReviewConfig exposing (config)

import Review.Generate
import Elm.Generator.RecordFieldHelper as RecordFieldHelper
import Elm.Generator.Svg as Svg
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Review.Generate.inModule
        ( "Accessors", [ "Library", "Fields" ] )
        RecordFieldHelper.accessors
        |> Review.Generate.rule
    , Review.Generate.replaceStub "svgToGenerate"
        Svg.generator
        |> Review.Generate.rule
    ]
```
[↑ more details](Review-Generate#Config)

## limits of [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) 😢

- can't make new modules or directories (yet)
- can't access online resources

To overcome these limitations,
you can build a tool that generates an elm module with the necessary information
and run `elm-review` with `generate-elm` as a second step.

## suggestions?
→ [contributing](https://github.com/lue-bird/generate-elm/blob/master/contributing.md).


## Nice resources

- 📻 [elm-radio podcast 031: Elm Code Generation](https://elm-radio.com/episode/code-generation/)
