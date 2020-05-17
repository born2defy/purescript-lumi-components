module Lumi.Components.Form.Validation2
  ( Validator
  , nonEmpty, nonEmptyArray, nonNull
  , mustEqual, mustBe
  , validNumber, validInt, validDate
  , optional
  , Validated(..)
  , _Validated, _Fresh, _Modified
  , setFresh, setModified
  , ModifyValidated(..)
  , class CanValidate, fresh, modified, fromValidated
  , validated
  , warn
  , FormField(..)
  , FieldState(..)
  , ValidationState(..)
  , fieldState
  , formValue
  , InputEvent(..)
  , updateFormField
  , validated2
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as NEA
import Data.Date as Date
import Data.Either (Either(..), either, hush, note)
import Data.Enum (toEnum)
import Data.Eq (class Eq1)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens (Lens, Prism', lens, over, prism', review, view)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (class Newtype, un)
import Data.Nullable (notNull)
import Data.Number as Number
import Data.Ord (class Ord1)
import Data.String.Common (split)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (fromString) as NES
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap, mapping)
import Lumi.Components.Column (column)
import Lumi.Components.Form.Internal (Forest, FormBuilder, FormBuilder'(..), Tree(..))
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Text (subtext, text)
import Prim.RowList as RL
import React.Basic (JSX)
import React.Basic.DOM as R

-- | A `Validator` takes a possibly invalid form `result` and produces
-- | a `valid` result, or an error message.
type Validator result valid =
  result -> Either String valid

-- | A `WarningValidator` can be used to issue a message to the user on
-- | certain form data, but cannot cause the form to fail. Accordingly,
-- | it cannot modify the form data value or type.
type WarningValidator result =
  result -> Maybe String

-- | A `Validator` which verifies that an input string is non-empty.
nonEmpty :: String -> Validator String NonEmptyString
nonEmpty name = note (name <> " is required.") <<< NES.fromString

-- | A `Validator` which verifies that an input array is non-empty.
nonEmptyArray :: forall a. String -> Validator (Array a) (NonEmptyArray a)
nonEmptyArray name = note (name <> " cannot be empty.") <<< NEA.fromArray

-- | A `Validator` which verifies that an optional field is specified.
nonNull :: forall a. String -> Validator (Maybe a) a
nonNull name = note (name <> " is required.")

-- | A `Validator` which verifies that its input equals some value.
mustEqual :: forall a. Eq a => a -> String -> Validator a a
mustEqual value1 = mustBe (_ == value1)

-- | A `Validator` which verifies that its input fulfills a specified condition.
mustBe :: forall a. (a -> Boolean) -> String -> Validator a a
mustBe cond error value
  | cond value = pure value
  | otherwise  = Left error

-- | A `Validator` which verifies that its input can be parsed as a number.
validNumber :: String -> Validator String Number
validNumber name = note (name <> " must be a number.") <<< Number.fromString

-- | A `Validator` which verifies that its input can be parsed as an integer.
validInt :: String -> Validator String Int
validInt name = note (name <> " must be a whole number.") <<< Int.fromString

-- | A `Validator` which verifies that its input can be parsed as a date.
-- | Dates are of the format "YYYY-MM-DD".
validDate :: String -> Validator String Date.Date
validDate name input =
  note (name <> " must be a date.") result
  where
    result = case traverse Int.fromString $ split (Pattern "-") input of
      Just [y, m, d] -> join $ Date.exactDate <$> toEnum y <*> toEnum m <*> toEnum d
      _ -> Nothing

-- | Modify a `Validator` to accept empty strings in addition to anything it
-- | already accepts. The empty string is mapped to `Nothing`, and any other
-- | valid input is mapped to `Just` the result of the original validator.
optional :: forall a. Validator String a -> Validator String (Maybe a)
optional _ "" = pure Nothing
optional v s  = map Just (v s)

newtype FormField a = FormField {fieldState :: FieldState, value :: a}  

fieldState :: ∀ a. FormField a -> FieldState
fieldState = _.fieldState <<< un FormField 

formValue :: ∀ a. FormField a -> a
formValue = _.value <<< un FormField 

derive instance ntFormField :: Newtype (FormField a) _
derive instance eqeqFormField :: Eq a => Eq (FormField a)
derive instance eq1FormField :: Eq1 FormField
derive instance ordFormField :: Ord a => Ord (FormField a)
derive instance ord1FormField :: Ord1 FormField
derive instance functorFormField :: Functor FormField

instance applyFormField :: Apply FormField where
  apply (FormField f) (FormField x) = case f.fieldState, x.fieldState of 
    Initial,                state               -> FormField {fieldState: state, value: applyValue}
    state,                  Initial             -> FormField {fieldState: state, value: applyValue}

    BeingModifiedFirstTime, state                  -> FormField {fieldState: state, value: applyValue}
    state,                  BeingModifiedFirstTime -> FormField {fieldState: state, value: applyValue}

    BeingModifiedAgain,     state               -> FormField {fieldState: BeingModifiedAgain, value: applyValue}
    state,                  BeingModifiedAgain  -> FormField {fieldState: BeingModifiedAgain, value: applyValue}

    BeenModified v,         BeenModified v2     -> FormField {fieldState: BeenModified (v <> v2), value: applyValue}
    where
    applyValue = f.value x.value 

instance applicativeFormField :: Applicative FormField where
  pure x = FormField {fieldState: Initial, value: x } 

data InputEvent a = ChangeEvent a | BlurEvent a 
derive instance eqeqInputEvent :: Eq a => Eq (InputEvent a)
derive instance eq1InputEvent :: Eq1 InputEvent
derive instance ordInputEvent :: Ord a => Ord (InputEvent a)
derive instance ord1InputEvent :: Ord1 InputEvent

updateFormField :: ∀ a. Eq a => InputEvent a -> FormField a -> FormField a 
updateFormField x ff = case x, fieldState ff of 
  ChangeEvent v, Initial -> FormField {fieldState: BeingModifiedFirstTime, value: v}
-- ^ if the user is changing the initial value, then its being modified the first time
  BlurEvent v, st | st == Initial  && formValue ff == v   
    -> FormField {fieldState: Initial, value: v}
-- ^ on the first change we make sure the user has done something before we change the state
  BlurEvent   v, _       -> FormField {fieldState: BeenModified Valid,     value: v}
-- ^ if the user is done, then its been modified and its valid until told otherwise
  ChangeEvent v, BeenModified _ -> FormField {fieldState: BeingModifiedAgain, value: v}
-- ^ if we are modifiying a previously modified value, then its being modified again
  ChangeEvent v, prevState    -> FormField {fieldState: prevState, value: v}
-- ^ if we changing a being modified state, then this just means we are not done yet

-- | A FieldState correctly describes the state of a form field
data FieldState 
  = Initial
 -- ^ The Intital Value supplied at creation
  | BeingModifiedFirstTime
 -- ^ The first time a user tries to modfiy a value (needed for waitToWarn)
  | BeingModifiedAgain
 -- ^ The value is being modified by the user/a new value is in the process of being constructed by the user.  We may have a warning to show based on their input.
  | BeenModified ValidationState
 -- ^ The value has been modified by the user/a new value has been constructed by the user.  We may have a Valid or Invalid new value.
derive instance eqFieldState :: Eq FieldState
derive instance ordFieldState :: Ord FieldState

data ValidationState 
  = Valid
 -- ^ The updated value is Valid
  | Invalid 
 -- ^ The updated value is Invalid 
  | BeingValidated 
 -- ^ The updated value is in an the process of being validated asynchronously
derive instance eqValidationState :: Eq ValidationState
derive instance ordValidationState :: Ord ValidationState

instance sgValidationState :: Semigroup ValidationState where
  append x y = case x, y of 
    Valid,          Valid          -> Valid
    Valid,          Invalid        -> Invalid
    Invalid,        Valid          -> Invalid
    Invalid,        Invalid        -> Invalid
    _,              BeingValidated -> BeingValidated
    BeingValidated, _              -> BeingValidated

validated2
  :: ∀ props validated a b
   . Validator a b
  -> FormBuilder { readonly :: Boolean, waitToWarn :: Boolean | props } (FormField validated) a
  -> FormBuilder { readonly :: Boolean, waitToWarn :: Boolean | props } (FormField validated) b
validated2 runValidator editor = FormBuilder \props@{ readonly, waitToWarn } ff ->
  let
      innerColumn_ children =
        column
          { style: R.css { maxWidth: "100%", maxHeight: "100%" }
          , children
          }

      { edit, validate } = un FormBuilder editor props ff

      modify :: Maybe String -> Forest -> Forest
      modify message forest =
          case Array.unsnoc forest of
            Nothing -> [Child { key: Nothing, child: errLine }]
            Just { init, last: Child c } ->
              Array.snoc init (Child c { child = innerColumn_ [c.child, errLine] })
            Just { init, last: Wrapper c } ->
              Array.snoc init (Wrapper c { children = modify message c.children })
            Just { init, last: Node n } ->
              Array.snoc init (Node n { validationError = Error <$> message })
        where
        errLine =
          guard (not readonly) message # foldMap \s ->
            case Error s of
              Error e ->
                text subtext
                  { className = notNull "labeled-field--validation-error"
                  , children = [ R.text e ]
                  }
              Warning w ->
                text subtext
                  { className = notNull "labeled-field--validation-warning"
                  , children = [ R.text w ]
                  }

      -- The validation can produce either a valid result, an error message, or
      -- none in the case where the form is Fresh.
      res :: Maybe (Either String b)
      res = do
        valid <- validate
        case fieldState ff of
          Initial ->
            hushError valid
          BeingModifiedFirstTime | waitToWarn ->
            hushError valid
          BeingModifiedAgain | waitToWarn ->
            hushError valid
          _ -> 
            showError valid

      hushError valid = pure <$> hush (runValidator valid)        
      showError valid = pure $ runValidator valid
      err = either pure (const Nothing) =<< res

   in { edit: \onChange -> modify err <<< edit $ onChange 
      , validate: hush =<< res
      }

-- | The `Validated` type describes the state of a validated form field. This
-- | state may be used to modify the way this form field or its validation
-- | messages are displayed.
-- |
-- | TODO: maybe convert this type to a record? Possible extensions to this
-- | type (as a record) could be a field `valid :: Boolean` to display an
-- | indicator that the field is valid, or a field
-- | `validating :: Maybe (Canceler a)` to control form fields with asynchronous
-- | validation.
data Validated a
  = Fresh a
  | Modified a

derive instance eqValidated :: Eq a => Eq (Validated a)
derive instance eq1Validated :: Eq1 Validated
derive instance ordValidated :: Ord a => Ord (Validated a)
derive instance ord1Validated :: Ord1 Validated

derive instance functorValidated :: Functor Validated

instance applyValidated :: Apply Validated where
  apply (Fresh f) r = f <$> r
  apply (Modified f) (Fresh a) = Modified (f a)
  apply (Modified f) (Modified a) = Modified (f a)

instance applicativeValidated :: Applicative Validated where
  pure = Fresh

-- | Lens for viewing and modifying `Validated` values.
_Validated :: forall a b. Lens (Validated a) (Validated b) a b
_Validated = flip lens ($>) $
  case _ of
    Fresh a -> a
    Modified a -> a

-- | Prism for the `Fresh` constructor of `Validated`.
_Fresh :: forall a. Prism' (Validated a) a
_Fresh = prism' Fresh $
  case _ of
    Fresh a -> Just a
    _ -> Nothing

-- | Prism for the `Modified` constructor of `Validated`.
_Modified :: forall a. Prism' (Validated a) a
_Modified = prism' Modified $
  case _ of
    Modified a -> Just a
    _ -> Nothing

-- | Sets all `Validated` fields in a record to `Fresh`, hiding all validation
-- | messages.
setFresh
  :: forall value
   . Mapping ModifyValidated value value
  => value
  -> value
setFresh = mapping (ModifyValidated (Fresh <<< view _Validated))

-- | Sets all `Validated` fields in a record to `Modified`, showing all
-- | validation messages.
setModified
  :: forall value
   . Mapping ModifyValidated value value
  => value
  -> value
setModified = mapping (ModifyValidated (Modified <<< view _Validated))

-- | Internal utility type for modifying the validated state of fields in
-- | records containing `Validated` values.
newtype ModifyValidated = ModifyValidated (Validated ~> Validated)

instance modifyValidated :: Mapping ModifyValidated a a => Mapping ModifyValidated (Validated a) (Validated a) where
  mapping m@(ModifyValidated f) = over _Validated (mapping m) <<< f
else instance modifyValidatedRecord :: (RL.RowToList r xs, MapRecordWithIndex xs (ConstMapping ModifyValidated) r r) => Mapping ModifyValidated {| r} {| r} where
  mapping d = hmap d
else instance modifyValidatedArray :: Mapping ModifyValidated a a => Mapping ModifyValidated (Array a) (Array a) where
  mapping d = map (mapping d)
else instance modifyValidatedIdentity :: Mapping ModifyValidated a a where
  mapping _ = identity

-- | Internal utility type class used to flatten repeated applications of
-- | `Validated` to a type.
class CanValidate u v | u -> v where
  fresh :: Prism' (Validated v) u
  modified :: Prism' (Validated v) u
  fromValidated :: Validated v -> u

instance canValidateValidated :: CanValidate (Validated a) a where
  fresh = identity
  modified = identity
  fromValidated = identity
else instance canValidateAny :: CanValidate a a where
  fresh = _Fresh
  modified = _Modified
  fromValidated = view _Validated

-- | Attach a validation function to a `FormBuilder p u a`, producing a new
-- | `FormBuilder` that takes a `Validated u` as form state and displays an
-- | error message if its form data is invalid.
-- |
-- | This `Validated` data type describes a form field as either `Fresh` or
-- | `Modified`, so that validation messages are only displayed if the field
-- | is `Modified`.
validated
  :: forall props unvalidated validated result result_
   . CanValidate unvalidated validated
  => Validator result_ result
  -> FormBuilder { readonly :: Boolean | props } unvalidated result_
  -> FormBuilder { readonly :: Boolean | props } (Validated validated) result
validated runValidator editor = FormBuilder \props@{ readonly } v ->
  let value = fromValidated v

      innerColumn_ children =
        column
          { style: R.css { maxWidth: "100%", maxHeight: "100%" }
          , children
          }

      { edit, validate } = un FormBuilder editor props value

      modify :: Maybe String -> Forest -> Forest
      modify message forest =
          case Array.unsnoc forest of
            Nothing -> [Child { key: Nothing, child: errLine }]
            Just { init, last: Child c } ->
              Array.snoc init (Child c { child = innerColumn_ [c.child, errLine] })
            Just { init, last: Wrapper c } ->
              Array.snoc init (Wrapper c { children = modify message c.children })
            Just { init, last: Node n } ->
              Array.snoc init (Node n { validationError = Error <$> message })
        where
          errLine =
            guard (not readonly) message # foldMap \s ->
              case Error s of
                Error e ->
                  text subtext
                    { className = notNull "labeled-field--validation-error"
                    , children = [ R.text e ]
                    }
                Warning w ->
                  text subtext
                    { className = notNull "labeled-field--validation-warning"
                    , children = [ R.text w ]
                    }

      -- The validation can produce either a valid result, an error message, or
      -- none in the case where the form is Fresh.
      res :: Maybe (Either String result)
      res = do
        valid <- validate
        case v of
          Fresh _ ->
            pure <$> hush (runValidator valid)
          _ ->
            pure $ runValidator valid

      err = either pure (const Nothing) =<< res

   in { edit: \onChange -> (modify err <<< edit) (onChange <<< \f ->
          case _ of
            v'@(Fresh u) -> review modified (f (fromValidated v'))
            v'@(Modified u) -> review modified (f (fromValidated v'))
        )
      , validate: hush =<< res
      }

-- | Attach a validation function to a `FormBuilder p u a`, producing a new
-- | `FormBuilder` that takes a `Validated u` as form state and displays a
-- | warning message if its form data triggers a warning, while still allowing
-- | the form to proceed.
warn
  :: forall props unvalidated validated result
   . CanValidate unvalidated validated
  => WarningValidator result
  -> FormBuilder { readonly :: Boolean | props } unvalidated result
  -> FormBuilder { readonly :: Boolean | props } (Validated validated) result
warn warningValidator editor = FormBuilder \props@{ readonly } v ->
  let { edit, validate } = un FormBuilder editor props (fromValidated v)

      innerColumn_ children =
        column
          { style: R.css { maxWidth: "100%", maxHeight: "100%" }
          , children
          }

      modify :: Forest -> Forest
      modify forest =
          case Array.unsnoc forest of
            Nothing -> [Child { key: Nothing, child: errLine }]
            Just { init, last: Child c } ->
              Array.snoc init (Child c { child = innerColumn_ [c.child, errLine] })
            Just { init, last: Wrapper c } ->
              Array.snoc init (Wrapper c { children = modify c.children })
            Just { init, last: Node n } ->
              Array.snoc init (Node n { validationError = Warning <$> message })

      errLine :: JSX
      errLine =
        guard (not readonly) message # foldMap \s ->
          text subtext
            { className = notNull "labeled-field--validation-warning"
            , children = [ R.text s ]
            }

      message :: Maybe String
      message =
        case v of
          Fresh _ ->
            Nothing
          _ ->
            warningValidator =<< validate

   in { edit: \onChange -> (modify <<< edit) (onChange <<< \f ->
          case _ of
            v'@(Fresh u) -> review modified (f (fromValidated v'))
            v'@(Modified u) -> review modified (f (fromValidated v'))
        )
      , validate
      }
