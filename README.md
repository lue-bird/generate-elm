# elm-review-generate

Many tools exist to generate elm code for different things:

- html: [dillonkearns/elm-review-html-to-elm](https://dark.elm.dmy.fr/packages/dillonkearns/elm-review-html-to-elm/latest/), ...
- svg: [jackfranklin/svg-to-elm](https://github.com/jackfranklin/svg-to-elm), [massimo-zaniboni/svg-to-elm](https://github.com/massimo-zaniboni/svg-to-elm), ...
- graphql: [harmboschloo/graphql-to-elm](https://dark.elm.dmy.fr/packages/harmboschloo/graphql-to-elm/latest/), ...
- record field helpers, lenses etc.: [pd-andy/elm-record-helpers](https://github.com/pd-andy/elm-record-helpers), ...
- json encoders, decoders (, codecs?): [alexkorban/json2elm](https://github.com/alexkorban/json2elm), [FranzSkuffka/elm-coder-generator](https://github.com/FranzSkuffka/elm-coder-generator), [wscherphof/json-to-elm](https://github.com/wscherphof/json-to-elm), [spkerkela/elm-decoder-generator](https://git.spkerkela.com/spkerkela/elm-decoder-generator), ...
- examples from elm documentation: [stoeffel/elm-verify-examples](https://github.com/stoeffel/elm-verify-examples)

We can make that process easier using [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/):
1. figure out when elm code should be generated
2. figure out what information we have to generate elm code
3. generate elm code using this information

## configuration

```elm
module ReviewConfig exposing (config)

import NoMissingRecordFieldHelper
import RecordFieldHelper
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Generate.inModule
        ( "Accessors", [ "Library", "Fields" ] )
        RecordFieldHelper.accessors
        |> Review.Generation.rule
    ]
```
See [`Config`](NoMissingRecordFieldHelper#Config)

## limits

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) can't make new modules or directories 😢.

## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-generate/blob/master/contributing.md).
