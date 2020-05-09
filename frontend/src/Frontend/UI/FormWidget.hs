module Frontend.UI.FormWidget
  ( HasInitialValue(..)
  , MakeConfig(..)
  , FormWidgetConfig
  , _formWidgetConfig_initialValue
  , _formWidgetConfig_setValue
  , formWidgetConfig_initialValue
  , formWidgetConfig_setValue
  , iec2fwc
  , fwc2iec
  , PrimFormWidgetConfig
  , _primFormWidgetConfig_fwc
  , _primFormWidgetConfig_initialAttributes
  , _primFormWidgetConfig_modifyAttributes
  , primFormWidgetConfig_fwc
  , primFormWidgetConfig_initialAttributes
  , primFormWidgetConfig_modifyAttributes
  , mkPfwc
  , iec2pfwc
  , pfwc2iec
  , FormWidget(..)
  , formWidget_value
  , formWidget_input
  , formWidget_hasFocus
  , ie2iw
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Map (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------

class HasInitialValue a where
  type InitialValue a :: *
  _initialValue :: a -> (InitialValue a)
  initialValue :: Lens' a (InitialValue a)

class MakeConfig cfg a | cfg -> a where
  mkCfg :: a -> cfg

-- A dot-separated namespace
--newtype WidgetName = WidgetName { unWidgetName :: Text }
--  deriving (Eq, Ord, IsString)

data FormWidgetConfig t a = FormWidgetConfig
  { _formWidgetConfig_initialValue :: a
  , _formWidgetConfig_setValue :: Maybe (Event t a)

  --, _formWidgetConfig_name :: WidgetName
  --, _formWidgetConfig_initialAttributes :: Map WidgetName (Map AttributeName Text)
  --, _formWidgetConfig_modifyAttributes :: Map WidgetName (Maybe (Event t (Map AttributeName (Maybe Text))))
  }

formWidgetConfig_initialValue :: Lens' (FormWidgetConfig t a) a
formWidgetConfig_initialValue f (FormWidgetConfig iv sv) = (\iv' -> FormWidgetConfig iv' sv) <$> f iv

formWidgetConfig_setValue :: Lens' (FormWidgetConfig t a) (Maybe (Event t a))
formWidgetConfig_setValue f (FormWidgetConfig iv sv) = (\sv' -> FormWidgetConfig iv sv') <$> f sv

instance Reflex t => Functor (FormWidgetConfig t) where
  fmap f (FormWidgetConfig iv sv) = FormWidgetConfig (f iv) (fmap f <$> sv)

instance MakeConfig (FormWidgetConfig t a) a where
  mkCfg a = FormWidgetConfig a Nothing
  {-# INLINABLE mkCfg #-}

instance HasInitialValue (FormWidgetConfig t a) where
  type InitialValue (FormWidgetConfig t a) = a
  _initialValue = _formWidgetConfig_initialValue
  initialValue = formWidgetConfig_initialValue

instance HasSetValue (FormWidgetConfig t a) where
  type SetValue (FormWidgetConfig t a) = Maybe (Event t a)
  setValue = formWidgetConfig_setValue

-- TODO Come up with better name
fwc2iec :: (Reflex t, DomSpace s) => (a -> Text) -> FormWidgetConfig t a -> InputElementConfig EventResult t s
fwc2iec toText (FormWidgetConfig iv sv) = def
  & inputElementConfig_initialValue .~ (toText iv)
  & inputElementConfig_setValue .~ (maybe never (fmap toText) sv)

-- TODO Come up with better name
iec2fwc :: Reflex t => (Text -> a) -> InputElementConfig EventResult t s -> FormWidgetConfig t a
iec2fwc fromText iec = FormWidgetConfig
  (fromText $ _inputElementConfig_initialValue iec)
  (fmap fromText <$> (_inputElementConfig_setValue iec))

------------------------------------------------------------------------------

-- A primitive config defines one set of attributes for a single element.
data PrimFormWidgetConfig t a = PrimFormWidgetConfig
  { _primFormWidgetConfig_fwc :: FormWidgetConfig t a
  , _primFormWidgetConfig_initialAttributes :: Map AttributeName Text
  , _primFormWidgetConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
  }

primFormWidgetConfig_fwc :: Lens' (PrimFormWidgetConfig t a) (FormWidgetConfig t a)
primFormWidgetConfig_fwc f a =
  (\newval -> a { _primFormWidgetConfig_fwc = newval }) <$> f (_primFormWidgetConfig_fwc a)

primFormWidgetConfig_initialAttributes :: Lens' (PrimFormWidgetConfig t a) (Map AttributeName Text)
primFormWidgetConfig_initialAttributes f a =
  (\newval -> a { _primFormWidgetConfig_initialAttributes = newval }) <$> f (_primFormWidgetConfig_initialAttributes a)

primFormWidgetConfig_modifyAttributes :: Reflex t => Lens' (PrimFormWidgetConfig t a) (Event t (Map AttributeName (Maybe Text)))
primFormWidgetConfig_modifyAttributes f a =
    (\newval -> a { _primFormWidgetConfig_modifyAttributes = Just newval }) <$> f (getter a)
  where
    getter = fromMaybe never . _primFormWidgetConfig_modifyAttributes

instance MakeConfig (PrimFormWidgetConfig t a) a where
  mkCfg a = mkPfwc (mkCfg a)
  {-# INLINABLE mkCfg #-}

-- TODO Come up with better name
mkPfwc :: FormWidgetConfig t a -> PrimFormWidgetConfig t a
mkPfwc fwc = PrimFormWidgetConfig fwc mempty Nothing

instance Reflex t => Functor (PrimFormWidgetConfig t) where
  fmap f (PrimFormWidgetConfig fwc ia ma) = PrimFormWidgetConfig (f <$> fwc) ia ma

instance InitialAttributes (PrimFormWidgetConfig t a) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = primFormWidgetConfig_initialAttributes

instance ModifyAttributes t (PrimFormWidgetConfig t a) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = primFormWidgetConfig_modifyAttributes

instance HasInitialValue (PrimFormWidgetConfig t a) where
  type InitialValue (PrimFormWidgetConfig t a) = a
  _initialValue = _initialValue . _primFormWidgetConfig_fwc
  initialValue = primFormWidgetConfig_fwc . initialValue

instance HasSetValue (PrimFormWidgetConfig t a) where
  type SetValue (PrimFormWidgetConfig t a) = Maybe (Event t a)
  setValue = primFormWidgetConfig_fwc . setValue

-- TODO Come up with better name
pfwc2iec :: (Reflex t, DomSpace s) => (a -> Text) -> PrimFormWidgetConfig t a -> InputElementConfig EventResult t s
pfwc2iec toText (PrimFormWidgetConfig (FormWidgetConfig iv sv) ia ma) = def
  & inputElementConfig_initialValue .~ (toText iv)
  & inputElementConfig_setValue .~ (maybe never (fmap toText) sv)
  & initialAttributes .~ ia
  & modifyAttributes .~ fromMaybe never ma

-- TODO Come up with better name
iec2pfwc :: Reflex t => (Text -> a) -> InputElementConfig EventResult t s -> PrimFormWidgetConfig t a
iec2pfwc fromText iec = PrimFormWidgetConfig fwc ia ma
  where
    ia = (_elementConfig_initialAttributes $ _inputElementConfig_elementConfig iec)
    ma = (_elementConfig_modifyAttributes $ _inputElementConfig_elementConfig iec)
    fwc = FormWidgetConfig
            (fromText $ _inputElementConfig_initialValue iec)
            (fmap fromText <$> (_inputElementConfig_setValue iec))

------------------------------------------------------------------------------

-- Implementation note: the _input field is 'Event t ()' instead of 'Event t a'
-- because this makes it possible to construct an Applicative instance which is
-- very useful when you need to compose input widgets. If you need the 'Event t
-- a' formulation, you can get it by tagging the _value field with the _input
-- event.

data FormWidget t a = FormWidget
  { _formWidget_value :: Dynamic t a
  -- ^ Contains the definitive value for this widget
  , _formWidget_input :: Event t ()
  -- ^ Fires whenever input happens in the widget itself.  This does not fire when the setValue event fires.
  , _formWidget_hasFocus :: Dynamic t Bool
  -- ^ Tells whether any of the sub-widgets in this widget have focus.
  }

formWidget_value :: Lens' (FormWidget t a) (Dynamic t a)
formWidget_value f (FormWidget v i hf) = (\v' -> FormWidget v' i hf) <$> f v

formWidget_input :: Lens' (FormWidget t a) (Event t ())
formWidget_input f (FormWidget v i hf) = (\i' -> FormWidget v i' hf) <$> f i

formWidget_hasFocus :: Lens' (FormWidget t a) (Dynamic t Bool)
formWidget_hasFocus f (FormWidget v i hf) = (\hf' -> FormWidget v i hf') <$> f hf

instance Reflex t => Functor (FormWidget t) where
  fmap f (FormWidget v i hf) = FormWidget (f <$> v) i hf

instance Reflex t => Applicative (FormWidget t) where
  pure a = FormWidget (constDyn a) never (constDyn False)
  FormWidget fv fi fh <*> FormWidget v i h = FormWidget (fv <*> v) (leftmost [fi, i]) ((||) <$> fh <*> h)

instance Reflex t => Monad (FormWidget t) where
  (FormWidget v i hf) >>= f = FormWidget v2 (leftmost [i, i2]) ((||) <$> hf <*> hf2)
    where
      diw = f <$> v
      v2 = join $ _formWidget_value <$> diw
      i2 = switchPromptlyDyn $ _formWidget_input <$> diw
      hf2 = join $ _formWidget_hasFocus <$> diw

instance HasValue (FormWidget t a) where
  type Value (FormWidget t a) = Dynamic t a
  value = _formWidget_value

-- TODO Come up with better name
ie2iw :: Reflex t => (Text -> a) -> InputElement er d t -> FormWidget t a
ie2iw fromText ie = FormWidget
  (fromText <$> value ie) (() <$ _inputElement_input ie) (_inputElement_hasFocus ie)
