module Lumi.Components.Form.AdamValidation where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as NEA
import Data.Date as Date
import Data.Either (Either(..), either, note)
import Data.Enum (toEnum)
import Data.Eq (class Eq1)
import Data.Foldable (foldMap, for_, traverse_)
import Data.Int as Int
import Data.Lens.Record (prop)
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
import Effect (Effect)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap, mapping)
import Lumi.Components.Column (column)
import Lumi.Components.Form (focus)
import Lumi.Components.Form.Internal (Forest, FormBuilder, FormBuilder'(..), Tree(..), formBuilder)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Text (body_, subtext, text)
import Prim.RowList as RL
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, key, targetValue)
import React.Basic.Events (handler_, merge)
import Unsafe.Coerce (unsafeCoerce)

-- | Ah blessed convenience
focusProp = focus <<< prop

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
newtype FormField' b a = FormField 
  { inputValue :: InputValue a
  , fieldState :: FieldState
  , validationState :: ValidationState
  , value :: a
  , validatedValue :: Maybe b 
  }  
derive instance ntFormField :: Newtype (FormField' b a) _
derive instance eqeqFormField :: (Eq b, Eq a) => Eq (FormField' b a)
derive instance eq1FormField :: Eq b => Eq1 (FormField' b) 
derive instance ordFormField :: (Ord b, Ord a) => Ord (FormField' b a)
derive instance ord1FormField :: Ord b => Ord1 (FormField' b)
derive instance functorFormField :: Functor (FormField' b)
instance showFormField :: (Show a, Show b) => Show (FormField' b a) where
  show (FormField ff) = fold
    [ "inputValue: "
    , show ff.inputValue
    , ", fieldState: "
    , show ff.fieldState
    , ", validationState: "
    , show ff.validationState
    , ", value: "
    , show ff.value
    , ", validatedValue: "
    , show ff.validatedValue
    ]

type FormField a = FormField' a String  

pure_ x = FormField 
  { inputValue: {inputEvent: Init, value: x}
  , fieldState: Initial
  , validationState: Valid
  , value: x
  , validatedValue: Nothing 
  } 

fieldState :: ∀ a b. FormField' b a -> FieldState
fieldState = _.fieldState <<< un FormField 

formValue :: ∀ a b. FormField' b a -> a
formValue = _.value <<< un FormField 

validatedValue :: ∀ a b. FormField' b a -> Maybe b
validatedValue = _.validatedValue <<< un FormField 

updateFieldState :: ∀ a b. FieldState -> FormField' b a -> FormField' b a 
updateFieldState fs (FormField ff) = FormField ff {fieldState = fs}

getErrorMessage :: ∀ a b. FormField' b a -> Maybe String
getErrorMessage (FormField ff) 
  | Invalid err <- ff.validationState
  = Just err
getErrorMessage _ = Nothing

-- | A FieldState correctly describes the state of a form field
data FieldState 
  = Initial 
 -- ^ The Intital Value supplied at creation
  | BeingModifiedFirstTime
 -- ^ The first time a user tries to modfiy a value (needed for waitToWarn)
  | BeingModifiedAgain
 -- ^ The value is being modified by the user/a new value is in the process of being constructed by the user.  We may have a warning to show based on their input.
  | BeenModified 
 -- ^ The value has been modified by the user/a new value has been constructed by the user.  We may have a Valid or Invalid new value.

derive instance eqFieldState :: Eq FieldState
derive instance ordFieldState :: Ord FieldState
instance showFieldState :: Show FieldState where
  show = case _ of 
    Initial                -> "Initial"
    BeingModifiedFirstTime -> "BeingModifiedFirstTime"
    BeingModifiedAgain     -> "BeingModifiedAgain"
    BeenModified           -> "BeenModified"

isInitial :: FieldState -> Boolean
isInitial Initial = true
isInitial _       = false 

data ValidationState 
  = Valid
 -- ^ The updated value is Valid
  | Invalid String
 -- ^ The updated value is Invalid 
  | BeingValidated 
 -- ^ The updated value is in an the process of being validated asynchronously

derive instance eqValidationState :: Eq ValidationState
derive instance ordValidationState :: Ord ValidationState
instance showValidationState :: Show ValidationState where
  show = case _ of 
    Valid          -> "Valid"
    Invalid s      -> "Invalid: " <> s
    BeingValidated -> "BeingValidated"

-- | Internal utility type for modifying the validated state of fields in
-- | records containing `Validated` values.
newtype ModifyFormField = ModifyFormField (∀ b. FormField' b ~> FormField' b)

instance modifyFormField 
  :: Mapping ModifyFormField a a
  => Mapping ModifyFormField 
    (FormField' b a) (FormField' b a) where
    mapping m@(ModifyFormField f) = map (mapping m) <<< f

else instance modifyFormFieldRecord
  :: (RL.RowToList r xs, MapRecordWithIndex xs (ConstMapping ModifyFormField) r r)
  => Mapping ModifyFormField {| r} {| r} where
    mapping d = hmap d

else instance modifyFormFieldArray
  :: Mapping ModifyFormField a a 
  => Mapping ModifyFormField (Array a) (Array a) where
    mapping d = map (mapping d)

else instance modifyFormFieldIdentity 
  :: Mapping ModifyFormField a a where
    mapping _ = identity

-- | Sets all `Validated` fields in a record to `Fresh`, hiding all validation
-- | messages.
setInitial :: ∀ value
   . Mapping ModifyFormField value value
  => value
  -> value
setInitial = mapping (ModifyFormField (updateFieldState $ Initial))

-- | Sets all `Validated` fields in a record to `Modified`, showing all
-- | validation messages.
setBeenModified :: ∀ value
   . Mapping ModifyFormField value value
  => value
  -> value
setBeenModified = mapping (ModifyFormField (updateFieldState $ BeenModified))

-- | Restricts input entry to only those which pass a validation function.  Other entries are ignored.
restrictInputOnChange :: ∀ a. Maybe (Validator a a) -> (a -> Effect Unit) -> (a -> Effect Unit)
restrictInputOnChange validateInput f = case validateInput of 
  Just validate -> either mempty f <<< validate
  Nothing       -> f 

data InputEvent = ChangeEvent | BlurEvent | Init

derive instance eqeqInputEvent :: Eq InputEvent
derive instance ordInputEvent :: Ord InputEvent
instance showInputEvent :: Show InputEvent where
  show = case _ of 
    ChangeEvent -> "ChangeEvent"
    BlurEvent   -> "BlurEvent"
    Init        -> "Init"

type InputValue a = {inputEvent :: InputEvent, value :: a}

type UpdateProps = {newValueOnlyOnFocusChange :: Boolean}

deafultUpdateProps :: UpdateProps
deafultUpdateProps = {newValueOnlyOnFocusChange: true}

updateFormField :: ∀ a b. Eq a =>  UpdateProps -> Validator a b -> FormField' b a -> FormField' b a
updateFormField {newValueOnlyOnFocusChange} validate ff@(FormField input)
  = case input.inputValue.inputEvent, fieldState ff of 
      -- if the user is changing the initial value, then its being modified the first time
      ChangeEvent, Initial  
        -> case validate input.inputValue.value of
              Left err -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeingModifiedFirstTime
                  , validationState: Invalid err
                  , value: input.inputValue.value
                  -- newValueOnlyOnFocusChange means there is no value when the user is modifiying the input
                  , validatedValue: Nothing
                  }
              Right valid -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeingModifiedFirstTime
                  , validationState: Valid
                  , value: input.inputValue.value
                  -- newValueOnlyOnFocusChange means there is no value when the user is modifiying the input
                  , validatedValue: if newValueOnlyOnFocusChange then Nothing else Just valid 
                  }

      -- on the first change we make sure the user has done something before we change the state
      BlurEvent, st | isInitial st && formValue ff == input.inputValue.value -> ff

      -- if the user is done, then its been modified and we validate the value if needed
      BlurEvent, _       
        -> case validate input.inputValue.value of 
              Left err -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeenModified 
                  , validationState: Invalid err
                  , value: input.inputValue.value
                  , validatedValue: Nothing
                  }
              Right valid -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeenModified
                  , validationState: Valid
                  , value: input.inputValue.value
                  , validatedValue: Just valid 
                  }

      -- if we are modifiying a previously modified value, then its being modified again
      ChangeEvent, BeenModified  
        -> case validate input.inputValue.value of
              Left err -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeingModifiedAgain
                  , validationState: Invalid err
                  , value: input.inputValue.value
                  -- newValueOnlyOnFocusChange means there is no value when the user is modifiying the input
                  , validatedValue: Nothing
                  }
              Right valid -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: BeingModifiedAgain
                  , validationState: Valid
                  , value: input.inputValue.value
                  -- newValueOnlyOnFocusChange means there is no value when the user is modifiying the input
                  , validatedValue: if newValueOnlyOnFocusChange then Nothing else Just valid 
                  }
      -- if we are changing a beingmodified state, then this just means we are not done yet
    --  This case is actually:  ChangeEvent v, prevState | prevState == BeingModifiedFirstTime || prevState == BeingModifiedAgain
      ChangeEvent, prevState 
        -> case validate input.inputValue.value of
              Left err -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: prevState
                  , validationState: Invalid err
                  , value: input.inputValue.value
                  -- There is no value when the user is modifiying the input
                  , validatedValue: Nothing
                  }
              Right valid -> 
                FormField 
                  { inputValue: input.inputValue
                  , fieldState: prevState
                  , validationState: Valid
                  , value: input.inputValue.value
                  -- There is no value when the user is modifiying the input
                  , validatedValue: if newValueOnlyOnFocusChange then Nothing else Just valid 
                  }

        -- Init is never thrown from an input, so this always means
        -- we are processing the initial value
      Init, prevState 
        -> case validate input.inputValue.value of
            Left err -> 
              FormField 
                { inputValue: input.inputValue
                , fieldState: prevState
                , validationState: Invalid err
                , value: input.inputValue.value
                -- There is no value when the user is modifiying the input
                , validatedValue: Nothing
                }
            Right valid -> 
              FormField 
                { inputValue: input.inputValue
                , fieldState: prevState
                , validationState: Valid
                , value: input.inputValue.value
                -- There is no value when the user is modifiying the input
                , validatedValue: Just valid 
                } 

validated :: ∀ props a b
  .  Eq a  
  => Validator a b
  -> FormBuilder (InputBoxProps   props) (FormField' b a) a
  -> FormBuilder (ValidationProps props) (FormField' b a) b
validated = validatedAs Error

warning :: ∀ props a b
  .  Eq a  
  => Validator a b
  -> FormBuilder (InputBoxProps   props) (FormField' b a) a
  -> FormBuilder (ValidationProps props) (FormField' b a) b
warning = validatedAs Warning

validatedAs :: ∀ props a b
  .  Eq a  
  => (String -> ValidationMessage)
  -> Validator a b
  -> FormBuilder (InputBoxProps   props) (FormField' b a) a
  -> FormBuilder (ValidationProps props) (FormField' b a) b
validatedAs toError validateValue editor = FormBuilder \props@{readonly, waitToShowErrors, newValueOnlyOnFocusChange} ff ->
  let 
       -- THIS IS WHERE THE MAGIC HAPPENS
      -- ================================
      validatedFF = updateFormField {newValueOnlyOnFocusChange} validateValue ff

      contractProps :: ∀ p p'. ValidationProps p -> InputBoxProps p'
      contractProps = unsafeCoerce

      innerColumn_ children =
        column
          { style: R.css {maxWidth: "100%", maxHeight: "100%"}
          , children
          }

      {edit, validate} = un FormBuilder editor (contractProps props) ff

      modify :: Maybe String -> Forest -> Forest
      modify message forest =
          case Array.unsnoc forest of
            Nothing -> [Child {key: Nothing, child: errLine}]

            Just {init, last: Child c} ->
              Array.snoc init (Child c {child = innerColumn_ [c.child, errLine]})

            Just {init, last: Wrapper c} ->
              Array.snoc init (Wrapper c {children = modify message c.children})

            Just {init, last: Node n} ->
              Array.snoc init (Node n {validationError = Error <$> message})
        where
        errLine =
          guard (not readonly) message # foldMap \s ->
            case toError s of
              Error e ->
                text subtext
                  { className = notNull "labeled-field--validation-error"
                  , children = [R.text e]
                  }
              Warning w ->
                text subtext
                  { className = notNull "labeled-field--validation-warning"
                  , children = [R.text w]
                  }

      hushErrorsByFieldState :: Maybe String -> Maybe String 
      hushErrorsByFieldState merr = case fieldState validatedFF of
      -- We don't show errors on initial values by default
        Initial ->
          Nothing
        -- We don't show errors on values that are being modified if the flag is set
        BeingModifiedFirstTime | waitToShowErrors ->
          Nothing
        -- We don't show errors on values that are being modified if the flag is set
        BeingModifiedAgain | waitToShowErrors ->
          Nothing
        -- In all other cases we keep the error
        _ -> 
          merr

   in { edit: \onChange -> (modify 
          (hushErrorsByFieldState
          -- We take the error message straight out of the formfield.  
          $ getErrorMessage validatedFF) 
          <<< edit) 
          -- IMPORTANT
          -- we ignore the raw state and manually feed in the validated value 
          (onChange <<< \f _ -> f validatedFF)

        -- We take the valeu straight from the validated formfield
      , validate: validatedValue validatedFF
      }

-- | A configurable input box makes a `FormBuilder` for strings
type InputBoxProps props
  = { readonly :: Boolean
    , waitToShowErrors :: Boolean
  -- ^ Rather than show errors on first edit, we wait until the user has entered their first value before showing errors
    , blurEventOnKey:: Maybe String
  -- ^ this means that new values are only produced on Blur Events  
    | props 
    }

type ValidationProps props = InputBoxProps (newValueOnlyOnFocusChange:: Boolean | props)

inputBox
  :: forall props a
   . Input.InputProps
  -> Maybe (Validator String String)
  -> FormBuilder (InputBoxProps props) (FormField a) String
inputBox inputProps restrictInput = formBuilder \{readonly, blurEventOnKey} (FormField input) ->
  let 
      edit onChange = if readonly
        then Input.alignToInput $ body_ input.inputValue.value

        else Input.input inputProps
              { value = input.inputValue.value
              , onChange = capture targetValue 
                $ traverse_ 
                $ restrictInputOnChange restrictInput
                  (onChange
                  <<< (\inp (FormField ff) -> FormField ff {inputValue = inp})
                  <<< {inputEvent: ChangeEvent, value: _})
              
              , onBlur = notNull $ capture targetValue
                $ traverse_ 
                  (onChange 
                  <<< (\inp (FormField ff) -> FormField ff {inputValue = inp})
                  <<< {inputEvent: BlurEvent, value: _})
             
              , style = R.css {width: "100%"}

              , onKeyUp = notNull $ case blurEventOnKey of 
                  Just keycode -> capture (merge {key, targetValue}) \{key, targetValue} -> 
                      for_ key \code -> 
                        if code == keycode 
                          then for_ targetValue 
                            (onChange 
                            <<< (\inp (FormField ff) -> FormField ff {inputValue = inp})
                            <<< {inputEvent: BlurEvent, value: _}) 
 
                         else mempty

                  Nothing -> handler_  mempty
              }
  in { edit
     , validate: Just input.inputValue.value
    }

-- | A simple text box makes a `FormBuilder` for strings
textbox :: forall props a. Maybe (Validator String String) -> FormBuilder (InputBoxProps props) (FormField a) String
textbox = inputBox Input.text_

-- | A simple password box makes a `FormBuilder` for strings
passwordBox :: forall props a. Maybe (Validator String String) -> FormBuilder (InputBoxProps props) (FormField a) String
passwordBox = inputBox Input.password
