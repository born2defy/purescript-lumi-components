module Lumi.Components.Form.Table
  ( TableFormBuilder
  , editableTable
  , nonEmptyEditableTable
  , column
  , column_
  , withProps
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldMap, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Lumi.Components.Column as Column
import Lumi.Components.EditableTable as EditableTable
import Lumi.Components.Form.Internal (FormBuilder, FormBuilder'(..), Tree(..), Forest, formBuilder)
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Orientation (Orientation(..))
import Lumi.Components.Row as Row
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text as T
import React.Basic (JSX, keyed)
import React.Basic.DOM as R
import Unsafe.Reference (unsafeRefEq)

-- | An applicative functor used to build editable tables out of `FormBuilder`s.
-- | `FormBuilder`s can be turned into `TableFormBuilder`s with the `column`
-- | function. `TableFormBuilder`s can, then, be used inside forms with the
-- | `editableTable` function.
newtype TableFormBuilder props row result =
  TableFormBuilder
    ( props
      -> { columns :: Array
             { label :: String
             , render :: row -> ((row -> row) -> Effect Unit) -> JSX
             }
         , validate :: row -> Maybe result
         }
    )

derive instance newtypeTableFormBuilder :: Newtype (TableFormBuilder props row a) _
derive instance functorTableFormBuilder :: Functor (TableFormBuilder props row)
instance applyTableFormBuilder :: Apply (TableFormBuilder props row) where
  apply (TableFormBuilder ff) (TableFormBuilder fa) =
    TableFormBuilder \props ->
      let
        { columns: columnsF, validate: validateF } = ff props
        { columns: columnsA, validate: validateA } = fa props
      in
        { columns: columnsF <> columnsA
        , validate: \row -> validateF row <*> validateA row
        }
instance applicativeTableFormBuilder :: Applicative (TableFormBuilder props row) where
  pure a =
    TableFormBuilder \_ ->
      { columns: []
      , validate: \_ -> pure a
      }

-- | A `TableFormBuilder` makes a `FormBuilder` for an array where each row has
-- | columns defined by it.
editableTable
  :: forall props row result
   . { addLabel :: String
     , defaultValue :: Maybe row
     , formBuilder :: TableFormBuilder { readonly :: Boolean | props } row result
     , maxRows :: Int
     , summary :: JSX
     }
  -> FormBuilder
      { readonly :: Boolean | props }
      (Array row)
      (Array result)
editableTable { addLabel, defaultValue, formBuilder: builder, maxRows, summary } =
  formBuilder \props rows ->
    let
      { columns, validate } = (un TableFormBuilder builder) props
    in
      { edit: \onChange ->
          EditableTable.editableTable
            { addLabel
            , maxRows
            , readonly: isNothing defaultValue || props.readonly
            , rowEq: unsafeRefEq
            , summary:
                Row.row
                  { style: R.css
                      { flex: "1"
                      , flexWrap: "wrap"
                      , justifyContent: "flex-end"
                      }
                  , children: [ summary ]
                  }
            , rows: Left $ mapWithIndex Tuple rows
            , onRowAdd: foldMap (onChange <<< flip Array.snoc) defaultValue
            , onRowRemove: \(Tuple index _) ->
                onChange \rows' -> fromMaybe rows' (Array.deleteAt index rows')
            , removeCell: EditableTable.defaultRemoveCell
            , columns:
                columns <#> \{ label, render } ->
                  { label
                  , renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  }
            }
      , validate: traverse validate rows
      }

-- | A `TableFormBuilder` makes a `FormBuilder` for a non-empty array where each
-- | row has columns defined by it.
nonEmptyEditableTable
  :: forall props row result
   . { addLabel :: String
     , defaultValue :: Maybe row
     , formBuilder :: TableFormBuilder { readonly :: Boolean | props } row result
     , maxRows :: Int
     , summary :: JSX
     }
  -> FormBuilder
      { readonly :: Boolean | props }
      (NEA.NonEmptyArray row)
      (NEA.NonEmptyArray result)
nonEmptyEditableTable { addLabel, defaultValue, formBuilder: builder, maxRows, summary } =
  formBuilder \props rows ->
    let
      { columns, validate } = (un TableFormBuilder builder) props
    in
      { edit: \onChange ->
          EditableTable.editableTable
            { addLabel
            , maxRows
            , readonly: isNothing defaultValue || props.readonly
            , rowEq: unsafeRefEq
            , summary:
                Row.row
                  { style: R.css
                      { flex: "1"
                      , flexWrap: "wrap"
                      , justifyContent: "flex-end"
                      }
                  , children: [ summary ]
                  }
            , rows: Right $ mapWithIndex Tuple rows
            , onRowAdd: foldMap (onChange <<< flip NEA.snoc) defaultValue
            , onRowRemove: \(Tuple index _) ->
                onChange \rows' -> fromMaybe rows' (NEA.fromArray =<< NEA.deleteAt index rows')
            , removeCell: EditableTable.defaultRemoveCell
            , columns:
                columns <#> \{ label, render } ->
                  { label
                  , renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  }
            }
      , validate: traverse validate rows
      }

-- | Convert a `FormBuilder` into a column of a table form with the specified
-- | label where all fields are laid out horizontally.
column_
  :: forall props row
   . String
  -> FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
column_ label = column label Horizontal

-- | Convert a `FormBuilder` into a column of a table form with the specified
-- | label and orientation.
column
  :: forall props row
   . String
  -> Orientation
  -> FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
column label orientation (FormBuilder f) =
  let
    renderer =
      case orientation of
        Horizontal ->
          horizontalRenderer
        Vertical ->
          verticalRenderer
  in
    TableFormBuilder \props ->
      { columns:
          [ { label
            , render: \row onChange ->
                renderer props.readonly ((f props row).edit onChange)
            }
          ]
      , validate: \row ->
          (f props row).validate
      }
  where
    innerColumn_ children =
      Column.column
        { style: R.css { maxWidth: "100%" }
        , children
        }

    innerRow_ children =
      Row.row
        { style: R.css { maxWidth: "100%" }
        , children
        }

    horizontalRenderer :: Boolean -> Forest -> JSX
    horizontalRenderer readonly forest =
      innerRow_
        [ intercalate (hspace S12) (map (toColumn readonly) forest)
        ]

    toColumn :: Boolean -> Tree -> JSX
    toColumn readonly =
      case _ of
        Child { key, child } ->
          maybe identity keyed key $ child
        Wrapper { key, wrap, children } ->
          maybe identity keyed key $ wrap
            [ innerRow_
                [ intercalate (hspace S12) (map (toColumn readonly) children)
                ]
            ]
        Node { key, validationError, children } ->
          maybe identity keyed key $
            innerColumn_
              [ innerRow_
                  [ intercalate (hspace S12) (map (toColumn readonly) children)
                  ]
              , guard (not readonly) $
                  foldMap errLine validationError
              ]

    verticalRenderer :: Boolean -> Forest -> JSX
    verticalRenderer readonly forest =
      innerColumn_
        [ intercalate (vspace S8) (map (toRow readonly) forest)
        ]

    toRow :: Boolean -> Tree -> JSX
    toRow readonly =
      case _ of
        Child { key, child } ->
          maybe identity keyed key $ child
        Wrapper { key, wrap, children } ->
          maybe identity keyed key $ wrap
            [ innerColumn_
                [ intercalate (vspace S8) (map (toColumn readonly) children)
                ]
            ]
        Node { key, validationError, children } ->
          maybe identity keyed key $
            innerColumn_
              [ intercalate (vspace S8) (map (toColumn readonly) children)
              , guard (not readonly) $
                  foldMap errLine validationError
              ]

    errLine :: ValidationMessage -> JSX
    errLine =
      case _ of
        Error e ->
          T.text T.subtext
            { className = Nullable.notNull "labeled-field--validation-error"
            , children = [ R.text e ]
            }
        Warning w ->
          T.text T.subtext
            { className = Nullable.notNull "labeled-field--validation-warning"
            , children = [ R.text w ]
            }

-- | Make the props available. This allows for changing the structure of a table
-- | form builder based on the current props.
withProps
  :: forall props row result
   . (props -> TableFormBuilder props row result)
  -> TableFormBuilder props row result
withProps f = TableFormBuilder \props -> un TableFormBuilder (f props) props
