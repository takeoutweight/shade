module Shade.Core.Internal.Events where

data EventProperties e =
  EventProperties { bubbles :: !Bool
                  , cancelable :: !Bool
                  , currentTarget :: !e -- NativeElem
                  , defaultPrevented :: !Bool
                  , eventPhase :: !Int
                  , isTrusted :: !Bool
                    -- ,  nativeEvent :: DOMEvent
                    -- , preventDefault :: IO ()
                    -- ,  stopPropagation :: IO ()
                  , evtTarget :: !e -- NativeElem
                    --, timeStamp :: Date
                  , eventType :: !String -- type
                  }

data ModifierKeys =
  ModifierKeys { altKey :: !Bool
               , ctrlKey :: !Bool
               , metaKey :: !Bool
               , shiftKey :: !Bool
               }

data MouseEvent e =
  MouseEvent { mouseEventProperties :: !(EventProperties e)
             , mouseModifierKeys :: !ModifierKeys
             , buttonNum :: !Int -- "button"
               -- , buttons :: Int
             , clientX :: !Double
             , clientY :: !Double
             , pageX :: !Double
             , pageY :: !Double
               -- , relatedTarget :: Unpacked
             , screenX :: !Double
             , screenY :: !Double
             }

data KeyboardEvent e =
  KeyboardEvent { keyboardEventProperties :: ! (EventProperties e)
                , keyboardModifierKeys :: !ModifierKeys
                , charCode :: !Int
                , key :: !String
                , keyCode :: !Int
                , locale :: !String
                , location :: !Int
                , repeat :: !Bool
                , which :: !Int
                }

newtype ChangeEvent s = ChangeEvent {changeEventValue :: s} -- NativeString

data FocusEvent e =
  FocusEvent { focusEventProperties :: ! (EventProperties e)
             , domEventTarget :: !e -- NativeElem
             , relatedTarget :: !e -- NativeElem
             }
