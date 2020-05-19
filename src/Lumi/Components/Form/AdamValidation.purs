module Lumi.Components.Form.AdamValidation where

import Prelude

import Data.Array (elem)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as NEA
import Data.Date as Date
import Data.Either (Either(..), either, hush, note)
import Data.Enum (toEnum)
import Data.Eq (class Eq1)
import Data.Foldable (foldMap, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isNothing)
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
import Effect (Effect)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap, mapping)
import Lumi.Components.Column (column)
import Lumi.Components.Form.Internal (Forest, FormBuilder, FormBuilder'(..), Tree(..), formBuilder)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Text (body_, subtext, text)
import Prim.RowList as RL
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue)

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



-- | FORMFIELDS
newtype FormField a = FormField {fieldState :: FieldState, value :: a}  
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

fieldState :: ∀ a. FormField a -> FieldState
fieldState = _.fieldState <<< un FormField 

formValue :: ∀ a. FormField a -> a
formValue = _.value <<< un FormField 

updateFieldState :: ∀ a. FieldState -> FormField a -> FormField a 
updateFieldState fs (FormField ff) = FormField ff {fieldState = fs}

-- | Update validation status of a formfield which has been modified
updateValidation :: ∀ a.  ValidationState -> FormField a -> FormField a 
updateValidation v ff = case fieldState ff of
  BeenModified _ -> updateFieldState (BeenModified v) ff
  _              -> ff

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

-- | Internal utility type for modifying the validated state of fields in
-- | records containing `Validated` values.
newtype ModifyFormField = ModifyFormField (FormField ~> FormField)

instance modifyFormField :: Mapping ModifyFormField a a => Mapping ModifyFormField (FormField a) (FormField a) where
  mapping m@(ModifyFormField f) = map (mapping m) <<< f
else instance modifyFormFieldRecord :: (RL.RowToList r xs, MapRecordWithIndex xs (ConstMapping ModifyFormField) r r) => Mapping ModifyFormField {| r} {| r} where
  mapping d = hmap d
else instance modifyFormFieldArray :: Mapping ModifyFormField a a => Mapping ModifyFormField (Array a) (Array a) where
  mapping d = map (mapping d)
else instance modifyFormFieldIdentity :: Mapping ModifyFormField a a where
  mapping _ = identity

-- | Sets all `Validated` fields in a record to `Fresh`, hiding all validation
-- | messages.
setInitial
  :: forall value
   . Mapping ModifyFormField value value
  => value
  -> value
setInitial = mapping (ModifyFormField (updateFieldState Initial))

-- | Sets all `Validated` fields in a record to `Modified`, showing all
-- | validation messages.
setBeenModified
  :: forall value
   . Mapping ModifyFormField value value
  => value
  -> value
setBeenModified = mapping (ModifyFormField (updateFieldState $ BeenModified Valid))

validated
  :: ∀ props validated a b
   . Validator a b
  -> FormBuilder { readonly :: Boolean, waitToShowErrors :: Boolean | props } (FormField validated) a
  -> FormBuilder { readonly :: Boolean, waitToShowErrors :: Boolean | props } (FormField validated) b
validated runValidator editor = FormBuilder \props@{ readonly, waitToShowErrors } ff ->
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
          BeingModifiedFirstTime | waitToShowErrors ->
            hushError valid
          BeingModifiedAgain | waitToShowErrors ->
            hushError valid
          _ -> 
            showError valid

      hushError valid = pure <$> hush (runValidator valid)        
      showError valid = pure $ runValidator valid
      err = either pure (const Nothing) =<< res

   in { edit: \onChange -> (modify err <<< edit) (onChange <<< \f -> f <<< (if isNothing err then identity else updateValidation Invalid))
      , validate: hush =<< res
      }

-- | Restricts input entry to only those which pass a validation function.  Other entries are ignored.
restrictInputOnChange :: ∀ a. Maybe (Validator a a) -> (a -> Effect Unit) -> (a -> Effect Unit)
restrictInputOnChange validateInput f = case validateInput of 
  Just validate -> either mempty f <<< validate
  Nothing       -> f 

-- | A configurable input box makes a `FormBuilder` for strings
inputBox
  :: forall props
   . Input.InputProps
  -> Maybe (Validator String String)
  -> FormBuilder
       { readonly :: Boolean
       , waitToShowErrors :: Boolean
      -- ^ Rather than show errors on first edit, we wait until the user has entered their first value before showing errors
       , newValueOnlyOnFocusChange:: Boolean
      -- ^ this means that new values are only produced on Blur Events  
       | props 
       }
       (FormField String)
       String
inputBox inputProps restrictInput = formBuilder \{ readonly, newValueOnlyOnFocusChange } ff ->
  let edit onChange = if readonly
        then Input.alignToInput $ body_ (formValue ff)
        else Input.input inputProps
              { value = formValue ff
              , onChange = capture targetValue $ traverse_ $ restrictInputOnChange restrictInput (onChange <<< updateFormField <<< ChangeEvent)
              , onBlur = notNull $ capture targetValue $ traverse_ (onChange <<< updateFormField <<< BlurEvent)
              , style = R.css { width: "100%" }
              }
  in { edit
     , validate: 
        if newValueOnlyOnFocusChange && (fieldState ff `elem` [BeingModifiedFirstTime,BeingModifiedAgain]) 
      -- ^ Here we restrict the production of new values only on blur-events 
      -- | TODO: Do we need to add return key presses?
          then Nothing
          else Just (formValue ff)
    }

-- | A simple text box makes a `FormBuilder` for strings
textbox
  :: forall props
   . Maybe (Validator String String) 
   ->  FormBuilder
      { readonly :: Boolean
      , waitToShowErrors :: Boolean
      , newValueOnlyOnFocusChange :: Boolean
      | props 
      } (FormField String) String
textbox = inputBox Input.text_

-- | A simple password box makes a `FormBuilder` for strings
passwordBox
  :: forall props
   . Maybe (Validator String String) 
   ->  FormBuilder
      { readonly :: Boolean
      , waitToShowErrors :: Boolean
      , newValueOnlyOnFocusChange :: Boolean
      | props 
      } (FormField String) String
passwordBox = inputBox Input.password


-- | Attach a validation function to a `FormBuilder p u a`, producing a new
-- | `FormBuilder` that takes a `Validated u` as form state and displays a
-- | warning message if its form data triggers a warning, while still allowing
-- | the form to proceed.
-- warn
--   :: forall props unvalidated validated result
--    . CanValidate unvalidated validated
--   => WarningValidator result
--   -> FormBuilder { readonly :: Boolean | props } unvalidated result
--   -> FormBuilder { readonly :: Boolean | props } (Validated validated) result
-- warn warningValidator editor = FormBuilder \props@{ readonly } v ->
--   let { edit, validate } = un FormBuilder editor props (fromValidated v)

--       innerColumn_ children =
--         column
--           { style: R.css { maxWidth: "100%", maxHeight: "100%" }
--           , children
--           }

--       modify :: Forest -> Forest
--       modify forest =
--           case Array.unsnoc forest of
--             Nothing -> [Child { key: Nothing, child: errLine }]
--             Just { init, last: Child c } ->
--               Array.snoc init (Child c { child = innerColumn_ [c.child, errLine] })
--             Just { init, last: Wrapper c } ->
--               Array.snoc init (Wrapper c { children = modify c.children })
--             Just { init, last: Node n } ->
--               Array.snoc init (Node n { validationError = Warning <$> message })

--       errLine :: JSX
--       errLine =
--         guard (not readonly) message # foldMap \s ->
--           text subtext
--             { className = notNull "labeled-field--validation-warning"
--             , children = [ R.text s ]
--             }

--       message :: Maybe String
--       message =
--         case v of
--           Fresh _ ->
--             Nothing
--           _ ->
--             warningValidator =<< validate

--    in { edit: \onChange -> (modify <<< edit) (onChange <<< \f ->
--           case _ of
--             v'@(Fresh u) -> review modified (f (fromValidated v'))
--             v'@(Modified u) -> review modified (f (fromValidated v'))
--         )
--       , validate
--       }
