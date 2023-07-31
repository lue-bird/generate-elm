module Elm.Code.Type exposing
    ( TypeNotUnconstrained(..), TypeAny
    , inferred
    , unit, tuple2, tuple3, to, named, record, recordExtendedBy
    , char, bool, int, float, string
    , maybe, list, array, set, dict
    )

{-| Type

@docs TypeNotUnconstrained, TypeAny
@docs inferred
@docs unit, tuple2, tuple3, to, named, record, recordExtendedBy


### `elm/core` type

for safety & convenience

@docs char, bool, int, float, string
@docs maybe, list, array, set, dict

-}

import Elm.Code.Common exposing (Field, Origin, Tuply(..), from)
import Elm.Code.Module exposing (Imports)
import Elm.Code.Util exposing (Both2, SpecificOrGeneral(..))
import Elm.Code.Util.Generalizable exposing (generalWith)
import Elm.Syntax.TypeAnnotation
import Misc exposing (mapAll3)
import Possibly exposing (Possibly(..))
import Stack exposing (StackFilled, StackWithTop)


{-| Any type.
-}
type alias TypeAny =
    TypeNotUnconstrained Possibly


{-|

  - `UnconstrainedType`: type argument (but not `compappend`, `appendable`, `comparable`, `number`)

        a : A <a> -> <a>

        type alias A a =
            <a>

        type A a
            = A <a>

  - `TypeNotUnconstrained Possibly`: `Name with type arguments`

        i : <I>

        type alias I =
            <O ()>

        type O o
            = O <(O o)>

  - `SpecificType`: type that can't adapt any other possible type

-}
type TypeNotUnconstrained possiblyOrNever
    = Unconstrained String
    | Constrained possiblyOrNever Constraint String
    | Specific possiblyOrNever TypeSpecificAny
    | Named possiblyOrNever TypeNamedLiteral


adaptNotUnconstrained :
    (possiblyOrNever -> possiblyOrNeverAdapted)
    -> TypeNotUnconstrained possiblyOrNever
    -> TypeNotUnconstrained possiblyOrNeverAdapted
adaptNotUnconstrained adaptPossiblyOrNever =
    \typeNotConstrained ->
        case typeNotConstrained of
            Unconstrained name ->
                Unconstrained name

            Constrained possiblyOrNever constraint name ->
                Constrained
                    (possiblyOrNever |> adaptPossiblyOrNever)
                    constraint
                    name

            Specific possiblyOrNever specific ->
                Specific
                    (possiblyOrNever |> adaptPossiblyOrNever)
                    specific

            Named possiblyOrNever namedLiteral ->
                Named
                    (possiblyOrNever |> adaptPossiblyOrNever)
                    namedLiteral


{-| "constrained type variables" are

  - `compappend`...
  - `appendable`...
  - `comparable`...
  - `number`...

-}
type Constraint
    = Compappend
    | Appendable
    | Comparable
    | Number


type alias TypeNamedLiteral =
    ( Origin, List TypeAny )


type TypeSpecificAny
    = Unit ()
    | Function TypeFunctionLiteral
    | Tuple
        (Tuply
            Tuple2LiteralType
            Tuple3LiteralType
        )
    | Record TypeRecordLiteral


type alias TypeFunctionLiteral =
    ( ( TypeAny, To TypeAny ), ParenthesizedOrNot )


type To result
    = To result


type ParenthesizedOrNot
    = NotParenthesized
    | Parenthesized


type alias Tuple2LiteralType =
    Both2 TypeAny


type alias Tuple3LiteralType =
    Both3 TypeAny


type TypeRecordLiteral
    = RecordFields (List (Field TypeAny))
    | RecordExtended String (StackFilled (Field TypeAny))



--


{-| A type variable.
-}
inferred : String -> TypeNotUnconstrained Never
inferred typeVariableName =
    varType typeVariableName


{-| Should not be exposed.
-}
varType : String -> TypeNotUnconstrained Never
varType typeVariableName =
    typeVariableName |> Unconstrained


{-| `()` type.

    import Elm.Code.Type as Type exposing (unit, fn, type_, from)

    fn (unit ()) (type_ (from "Test" "Test") [])

would generate

    () -> Test.Test

See [`Origin`](#Origin).

-}
unit : () -> TypeNotUnconstrained Possibly
unit unitValue =
    unitValue
        |> Unit
        |> Specific Possible


{-| `Bool` type.

    import Elm.Code.Type as Type exposing (record, bool, field)

    record [ field "usesContext" bool ]

would generate

    : { usesContext : Bool }

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    type_ (from "Basics" "Bool") []

See [`Origin`](#Origin).

-}
bool : TypeNotUnconstrained Possibly
bool =
    named (from "Basics" "Bool") []


{-| `Char` type.

    import Elm.Code.Type as Type exposing (funType, list, string, char)

    funType [ string ] (list char)

would generate

    String -> List Char

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    type_ (from "Char" "Char") []

See [`Origin`](#Origin).

-}
char : TypeNotUnconstrained Possibly
char =
    named (from "Char" "Char") []


{-| `Int` type.

    import Elm.Code.Type as Type exposing (tuple2, int)

    tuple2 ( int, int )

would generate

    ( Int, Int )

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    type_ (from "Basics" "Int") []

See [`Origin`](#Origin).

-}
int : TypeNotUnconstrained Possibly
int =
    named (from "Basics" "Int") []


{-| `Float` type.

    import Elm.Code.Type as Type exposing (tuple2, float)

    tuple2 ( float, float )

would generate

    ( Float, Float )

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    type_ (from "Basics" "Float") []

See [`Origin`](#Origin).

-}
float : TypeNotUnconstrained Possibly
float =
    named (from "Basics" "Float") []


{-| `String` type.

    import Elm.Code.Type as Type exposing (record, string)

    record
        [ field "name" string
        , field "status" string
        ]

would generate

    : { name : String, status : String }

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    type_ (from "String" "String") []

See [`Origin`](#Origin).

-}
string : TypeNotUnconstrained Possibly
string =
    named (from "String" "String") []


{-| `Maybe a` type.

    import Elm.Code.Type as Type exposing (fn, maybe, string, int)

    fn (string) (maybe int)

would generate

    String -> Maybe Int

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    \a -> type_ (from "Maybe" "Maybe") [ a ]

See [`Origin`](#Origin).

-}
maybe :
    TypeNotUnconstrained possiblyOrNever_
    -> TypeNotUnconstrained Possibly
maybe valueType =
    named (from "Maybe" "Maybe") [ valueType ]


{-| `List a` type.

    import Elm.Code.Type as Type exposing (funType, list, string, char)

    funType [ string ] (list char)

would generate

    String -> List Char

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    \a -> type_ (from "List" "List") [ a ]

See [`Origin`](#Origin).

-}
list :
    TypeNotUnconstrained possiblyOrNever_
    -> TypeNotUnconstrained Possibly
list valueType =
    named (from "List" "List") [ valueType ]


{-| `Array a` type.

    import Elm.Code.Type as Type exposing (fn)

    \a -> fn (Type.list a) (Type.array a)

would generate

    List a -> Array.Array a

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    \a -> type_ (from "Array" "Array") [ a ]

See [`Origin`](#Origin).

-}
array :
    TypeNotUnconstrained possiblyOrNever_
    -> TypeNotUnconstrained Possibly
array valueType =
    named (from "Array" "Array") [ valueType ]


{-| `Set a` type.

    import Elm.Code.Type as Type exposing (fn)

    \comparable ->
        fn (Type.list comparable)
            (Type.set comparable)

would generate

    List comparable -> Set.Set comparable

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, from)

    \a -> type_ (from "Set" "Set") [ a ]

See [`Origin`](#Origin).

-}
set :
    TypeNotUnconstrained possiblyOrNever_
    -> TypeNotUnconstrained Possibly
set valueType =
    named (from "Set" "Set") [ valueType ]


{-| `Dict k v` type.

    import Elm.Code.Type as Type exposing (funType, dict, string, type_, local)

    dict string (type_ (local "Country"))

would generate

    Dict.Dict String Country

Note: This is equivalent to

    import Elm.Code.Type as Type exposing (type_, use, from)

    \k v ->
        type_ (from "Dict" "Dict") [ use k, use v ]

See [`Origin`](#Origin) and [`use`](#use).

-}
dict :
    TypeNotUnconstrained possiblyOrNeverKey_
    -> TypeNotUnconstrained possiblyOrNeverValue_
    -> TypeNotUnconstrained Possibly
dict keyType valueType =
    named (from "Dict" "Dict")
        [ keyType |> adaptNotUnconstrained (\_ -> Possible)
        , valueType |> adaptNotUnconstrained (\_ -> Possible)
        ]


{-| A 2-tuple type.

    import Elm.Code.Type as Type exposing (tuple2, float)

    tuple2 ( float, float )

would generate

    ( Float, Float )

-}
tuple2 :
    ( TypeNotUnconstrained possiblyOrNeverFirst_
    , TypeNotUnconstrained possiblyOrNeverSecond_
    )
    -> TypeNotUnconstrained Possibly
tuple2 ( firstType, secondType ) =
    ( firstType, secondType )
        |> Tuple.mapBoth
            (adaptNotUnconstrained (\_ -> Possible))
            (adaptNotUnconstrained (\_ -> Possible))
        |> Tuple2y
        |> Tuple
        |> Specific Possible


{-| A 3-tuple type.

    import Elm.Code.Type as Type exposing (tuple3, int)

    tuple3 ( int, int, int )

would generate

    ( Int, Int, Int )

-}
tuple3 :
    ( TypeNotUnconstrained possiblyOrNeverFirst_
    , TypeNotUnconstrained possiblyOrNeverSecond_
    , TypeNotUnconstrained possiblyOrNeverThird_
    )
    -> TypeNotUnconstrained Possibly
tuple3 ( firstType, secondType, thirdType ) =
    ( firstType, secondType, thirdType )
        |> mapAll3
            (adaptNotUnconstrained (\_ -> Possible))
            (adaptNotUnconstrained (\_ -> Possible))
            (adaptNotUnconstrained (\_ -> Possible))
        |> Tuple3y
        |> Tuple
        |> Specific Possible


{-| A type with type arguments.

    import Elm.Code.Type as Type exposing (type_)

    \a ->
        type_ (from "Tree" "Tree") [ a ]

would generate

    Tree.Tree a

See [`Origin`](#Origin).
Note: If the type arguments aren't of the same specific (e.g. both type variables), you need [`use`](#use).

    import Elm.Code.Type as Type exposing (type_, string, use, from)

    \a ->
        type_ (from "Result" "Result") [ string, a ]

-}
named :
    Origin
    -> List (TypeNotUnconstrained possiblyOrNever_)
    -> TypeNotUnconstrained Possibly
named origin typeArguments =
    ( origin
    , typeArguments
        |> List.map (adaptNotUnconstrained (\_ -> Possible))
    )
        |> Named Possible


{-| A function type.

    import Elm.Code.Type as Type exposing (to, list)

    \a b c ->
        ( ( a, [ b ] ) |> to c)
        , [ list a
          , list b
          ]
        )
            |> to (list c)

would generate

    (a -> b -> c)
    -> List a
    -> List b
    -> List c

-}
to :
    TypeNotUnconstrained possiblyOrNever_
    ->
        StackFilled
            (TypeNotUnconstrained possiblyOrNever_)
    -> TypeNotUnconstrained Possibly
to resultType argumentTypes =
    ( ( Debug.todo ""
      , To (resultType |> adaptNotUnconstrained (\_ -> Possible))
      )
    , NotParenthesized
    )
        |> Function
        |> Specific Possible


fun argumentType resultType =
    ( ( argumentType
      , To (resultType |> adaptNotUnconstrained (\_ -> Possible))
      )
    , NotParenthesized
    )
        |> Function
        |> Specific Possible


{-| A record type.

    import Elm.Code.Type as Type exposing (record, string)

    record
        [ field "name" string
        , field "status" string
        ]

would generate

    { name = String, status = String }

-}
record :
    List (Field (TypeNotUnconstrained possiblyOrNever_))
    -> TypeNotUnconstrained Possibly
record fields =
    fields
        |> List.map
            (Tuple.mapSecond
                (adaptNotUnconstrained (\_ -> Possible))
            )
        |> RecordFields
        |> Record
        |> Specific Possible


{-| An extended record type.

    import Elm.Code.Type as Type exposing (recordExtendedBy, field, string)

    \named ->
        named
            |> recordExtendedBy
                (field "name" string)
                []

would generate

    : { named | name : String }

-}
recordExtendedBy :
    Field TypeAny
    -> List (Field TypeAny)
    -> TypeNotUnconstrained Never
    -> TypeNotUnconstrained Possibly
recordExtendedBy firstAddedField followingAddedFields recordTypeVariableToExtend =
    RecordExtended
        (recordTypeVariableToExtend |> Generalizable.value)
        ( firstAddedField, followingAddedFields )
        |> generalWith (Record >> Specific)



--


{-| Convert an [`Code.Type`](#Type) into an `Elm.Syntax.TypeAnnotation.TypeAnnotation`.
-}
typeToSyntax :
    { imports : Imports }
    -> TypeNotUnconstrained possiblyOrNever_
    -> Elm.Syntax.TypeAnnotation.TypeAnnotation
typeToSyntax { imports } typeStructure =
    let
        findOrigin_ =
            findOrigin imports

        step =
            typeToSyntax { imports = imports }

        stepFields =
            List.map (Tuple.mapSecond step)
    in
    case typeStructure of
        Specific specific ->
            case specific of
                Unit () ->
                    CodeGen.unitAnn

                Function ( input, In to, parenthesizedOrNot ) ->
                    CodeGen.funAnn (step input) (step to)

                Tuply tuply ->
                    case tuply of
                        Tuple2y ( a0, a1 ) ->
                            CodeGen.tupleAnn ([ a0, a1 ] |> List.map step)

                        Tuple3y ( a0, a1, a2 ) ->
                            CodeGen.tupleAnn ([ a0, a1, a2 ] |> List.map step)

                Record recordSpecificType ->
                    case recordSpecificType of
                        RecordFields fields ->
                            CodeGen.recordAnn
                                (fields |> stepFields)

                        RecordExtended recordVariable addedFields ->
                            CodeGen.extRecordAnn recordVariable
                                (addedFields |> List.NonEmpty.toList |> stepFields)

        Unconstrained variableName ->
            CodeGen.typeVar variableName

        TypeNotUnconstrained Possibly ( origin, With typeParameters ) ->
            fq CodeGen.fqTyped
                findOrigin_
                origin
                (typeParameters |> List.map step)
