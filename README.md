# elm-review-generate

Many tools exist to generate elm code for different things:

- record field helpers, lenses: [pd-andy/elm-record-helpers](https://github.com/pd-andy/elm-record-helpers), ...
- json (, bytes?, xml?, yaml?, ...) encoders, decoders (, codecs?): [alexkorban/json2elm](https://github.com/alexkorban/json2elm), [FranzSkuffka/elm-coder-generator](https://github.com/FranzSkuffka/elm-coder-generator), [wscherphof/json-to-elm](https://github.com/wscherphof/json-to-elm), [spkerkela/elm-decoder-generator](https://git.spkerkela.com/spkerkela/elm-decoder-generator), ...
- tests from elm documentation examples: [stoeffel/elm-verify-examples](https://github.com/stoeffel/elm-verify-examples), ...
- html (, other html libs?): [dillonkearns/elm-review-html-to-elm](https://package.elm-lang.org/packages/dillonkearns/elm-review-html-to-elm/latest/), [CodeTownOfficial's html-to-elm](https://github.com/CodeTownOfficial/html-to-elm) & [rubymaniac's vscode-html-to-elm](https://github.com/rubymaniac/vscode-html-to-elm)...
- svg (, typed-svg?, ...): [jackfranklin/svg-to-elm](https://github.com/jackfranklin/svg-to-elm), [massimo-zaniboni/svg-to-elm](https://github.com/massimo-zaniboni/svg-to-elm), [pinata-llc's svg2elm](https://github.com/pinata-llc/svg2elm) ...
- ...

The process can be simplified using [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) for those listed ↑:

- no need to install separate command line tools or IDE plugins
- one simple way to configure, customize and create all code generators
- adding necessary imports, reporting failed/successful code generation etc. is handled automatically

## configuration

```elm
module ReviewConfig exposing (config)

import Review.Generate
import  Elm.Generator.RecordFieldHelper as RecordFieldHelper
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Generate.inModule
        [ "Accessors", "Library", "Fields" ]
        RecordFieldHelper.accessors
        |> Review.Generate.rule
    ]
```
[↑ more details](Review-Generate#rule)

## limits

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) can't make new modules or directories 😢.

## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-generate/blob/master/contributing.md).
