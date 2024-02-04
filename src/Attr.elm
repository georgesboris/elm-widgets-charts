module Attr exposing
    ( Attr
    , apply
    , attr
    , cond
    , maybe
    , none
    , withAttrs
    )


type Attr attrs
    = Attr (attrs -> attrs)
    | AttrBatch (List (Attr attrs))


attr : (attrs -> attrs) -> Attr attrs
attr =
    Attr


{-|

    Attr.cond
        [ ( isOpen, primary )
        , ( isClosed, disabled )
        ]

-}
cond : List ( Attr attrs, Bool ) -> Attr attrs
cond attrList =
    attrList
        |> List.filterMap
            (\( attr_, predicate ) ->
                if predicate then
                    Just attr_

                else
                    Nothing
            )
        |> AttrBatch


maybe : (a -> Attr attrs) -> Maybe a -> Attr attrs
maybe toAttr maybeA =
    maybeA
        |> Maybe.map toAttr
        |> Maybe.withDefault none


none : Attr attrs
none =
    Attr identity


apply : attrs -> List (Attr attrs) -> attrs
apply =
    List.foldl
        (\attr_ acc ->
            case attr_ of
                Attr fn ->
                    fn acc

                AttrBatch fns ->
                    apply acc fns
        )


withAttrs : attrs -> (attrs -> a) -> List (Attr attrs) -> a
withAttrs attrs fn attrList =
    fn (apply attrs attrList)
