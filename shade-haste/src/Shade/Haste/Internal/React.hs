{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shade.Haste.Internal.React where
import Haste.DOM (Elem)
import Haste.Prim (Ptr, JSAny, JSString, fromJSStr, toJSStr, toPtr, fromPtr)
import Haste.Foreign (Pack, Unpack, ffi)
import Shade.Core.Internal.Attributes
import Shade.Core.Internal.Events
import System.IO.Unsafe (unsafePerformIO) -- TODO Not necessary! our event parsing can easily be Applicative

newtype React = React JSAny deriving (Pack, Unpack)
newtype RawAttrs = RawAttrs JSAny  deriving (Pack, Unpack)
newtype ReactArray = ReactArray JSAny deriving (Pack, Unpack)

foreign import ccall js_empty_object :: IO RawAttrs
foreign import ccall "js_set_field" js_set_field_String :: RawAttrs -> JSString -> JSString -> IO ()
foreign import ccall "js_set_field" js_set_field_Double :: RawAttrs -> JSString -> Double -> IO ()
foreign import ccall "js_set_field" js_set_field_Int :: RawAttrs -> JSString -> Int -> IO ()
foreign import ccall "js_set_field" js_set_field_Obj :: RawAttrs -> JSString -> RawAttrs -> IO ()
foreign import ccall js_set_field_True :: RawAttrs -> JSString -> IO ()
foreign import ccall js_set_field_False :: RawAttrs -> JSString -> IO ()
foreign import ccall js_targetValue :: RawChangeEvent -> JSString

foreign import ccall js_set_onClick       :: Ptr (RawMouseEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onDoubleClick :: Ptr (RawMouseEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onChange      :: Ptr (RawChangeEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyUp       :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyPress    :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onKeyDown     :: Ptr (RawKeyboardEvent -> IO ()) -> RawAttrs -> IO ()
foreign import ccall js_set_onBlur        :: Ptr (RawFocusEvent -> IO ()) -> RawAttrs -> IO ()

foreign import ccall "js_empty" js_ReactArray_empty :: IO ReactArray
foreign import ccall "js_push" js_ReactArray_push :: ReactArray -> React -> IO ()

foreign import ccall js_React_DOM_a :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_abbr :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_address :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_article :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_aside :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_audio :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_b :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_bdi :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_bdo :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_big :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_blockquote :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_body :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_button :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_canvas :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_caption :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_cite :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_code :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_colgroup :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_data :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_datalist :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_dd :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_del :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_details :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_dfn :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_div :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_dl :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_dt :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_em :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_fieldset :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_figcaption :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_figure :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_footer :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_form :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h1 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h2 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h3 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h4 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h5 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_h6 :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_head :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_header :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_html :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_i :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_iframe :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_ins :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_kbd :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_label :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_legend :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_li :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_main :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_map :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_mark :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_menu :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_menuitem :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_meter :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_nav :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_noscript :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_object :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_ol :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_optgroup :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_option :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_output :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_p :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_pre :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_progress :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_q :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_rp :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_rt :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_ruby :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_s :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_samp :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_section :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_select :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_small :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_span :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_strong :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_sub :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_summary :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_sup :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_table :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_tbody :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_td :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_tfoot :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_th :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_thead :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_time :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_tr :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_u :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_ul :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_var :: RawAttrs -> ReactArray -> IO React
foreign import ccall js_React_DOM_video :: RawAttrs -> ReactArray -> IO React

foreign import ccall js_React_DOM_area :: RawAttrs -> IO React
foreign import ccall js_React_DOM_base :: RawAttrs -> IO React
foreign import ccall js_React_DOM_br :: RawAttrs -> IO React
foreign import ccall js_React_DOM_col :: RawAttrs -> IO React
foreign import ccall js_React_DOM_embed :: RawAttrs -> IO React
foreign import ccall js_React_DOM_hr :: RawAttrs -> IO React
foreign import ccall js_React_DOM_img :: RawAttrs -> IO React
foreign import ccall js_React_DOM_input :: RawAttrs -> IO React
foreign import ccall js_React_DOM_keygen :: RawAttrs -> IO React
foreign import ccall js_React_DOM_link :: RawAttrs -> IO React
foreign import ccall js_React_DOM_meta :: RawAttrs -> IO React
foreign import ccall js_React_DOM_param :: RawAttrs -> IO React
foreign import ccall js_React_DOM_source :: RawAttrs -> IO React
foreign import ccall js_React_DOM_track :: RawAttrs -> IO React
foreign import ccall js_React_DOM_wbr :: RawAttrs -> IO React

foreign import ccall js_React_DOM_script :: RawAttrs -> JSString -> IO React
foreign import ccall js_React_DOM_style :: RawAttrs -> JSString -> IO React
foreign import ccall js_React_DOM_textarea :: RawAttrs -> JSString -> IO React
foreign import ccall js_React_DOM_title :: RawAttrs -> JSString -> IO React

foreign import ccall "js_id" js_React_DOM_text :: JSString -> IO React


-- Parsing synthetic DOM events

-- TODO Don't need this to use unsafePerformIO
-- | eg: let a = fieldIn "fieldA" :: MyStructPtr -> String
fieldIn :: (Unpack a, Pack b) => String -> a -> b
fieldIn f = unsafePerformIO . (ffi (toJSStr (fieldIn' f)))
  where
    fieldIn' f = "(function(u){return u."++f++";})"

newtype RawMouseEvent = RawMouseEvent JSAny deriving (Pack, Unpack)
parseMouseEvent :: RawMouseEvent -> (MouseEvent Elem)
parseMouseEvent e = MouseEvent
                    (EventProperties
                     (fieldIn "bubbles" e)
                     (fieldIn "cancelable" e)
                     (fieldIn "currentTarget" e)
                     (fieldIn "defaultPrevented" e)
                     (fieldIn "eventPhase" e)
                     (fieldIn "isTrusted" e)
                     (fieldIn "target" e)
                     (fieldIn "eventType" e))
                    (ModifierKeys
                     (fieldIn "altKey" e)
                     (fieldIn "ctrlKey" e)
                     (fieldIn "metaKey" e)
                     (fieldIn "shiftKey" e))
                    (fieldIn "button" e)
--                 (fieldIn "buttons" e)
                    (fieldIn "clientX" e)
                    (fieldIn "clientY" e)
                    (fieldIn "pageX" e)
                    (fieldIn "pageY" e)
--                 (fieldIn "relatedTarget" e)
                    (fieldIn "screenX" e)
                    (fieldIn "screenY" e)

newtype RawKeyboardEvent = RawKeyboardEvent JSAny deriving (Pack, Unpack)
parseKeyboardEvent :: RawKeyboardEvent -> (KeyboardEvent Elem)
parseKeyboardEvent e = KeyboardEvent
                 (EventProperties
                  (fieldIn "bubbles" e)
                  (fieldIn "cancelable" e)
                  (fieldIn "currentTarget" e)
                  (fieldIn "defaultPrevented" e)
                  (fieldIn "eventPhase" e)
                  (fieldIn "isTrusted" e)
                  (fieldIn "target" e)
                  (fieldIn "eventType" e))
                 (ModifierKeys
                  (fieldIn "altKey" e)
                  (fieldIn "ctrlKey" e)
                  (fieldIn "metaKey" e)
                  (fieldIn "shiftKey" e))
                 (fieldIn "charCode" e)
                 (fieldIn "key" e)
                 (fieldIn "keyCode" e)
                 (fieldIn "locale" e)
                 (fieldIn "location" e)
                 (fieldIn "repeat" e)
                 (fieldIn "which" e)

newtype RawChangeEvent = RawChangeEvent JSAny deriving (Pack, Unpack)
parseChangeEvent :: RawChangeEvent -> (ChangeEvent JSString)
parseChangeEvent e = ChangeEvent (js_targetValue e)

newtype RawFocusEvent = RawFocusEvent JSAny deriving (Pack, Unpack)
parseFocusEvent :: RawFocusEvent -> (FocusEvent Elem)
parseFocusEvent e = FocusEvent
                    (EventProperties
                     (fieldIn "bubbles" e)
                     (fieldIn "cancelable" e)
                     (fieldIn "currentTarget" e)
                     (fieldIn "defaultPrevented" e)
                     (fieldIn "eventPhase" e)
                     (fieldIn "isTrusted" e)
                     (fieldIn "target" e)
                     (fieldIn "eventType" e))
                    (fieldIn "DOMEventTarget" e)
                    (fieldIn "relatedTarget" e)

-- EventHanlders for Callbacks

newtype EventHandler = EventHandler {unEventHandler :: RawAttrs -> IO ()}

onClick :: ((MouseEvent Elem) -> IO ()) -> EventHandler
onClick cb = EventHandler (js_set_onClick (toPtr (cb . parseMouseEvent)))

onDoubleClick :: ((MouseEvent Elem) -> IO ()) -> EventHandler
onDoubleClick cb = EventHandler (js_set_onDoubleClick (toPtr (cb . parseMouseEvent)))

onChange :: ((ChangeEvent JSString) -> IO ()) -> EventHandler
onChange cb = EventHandler (js_set_onChange (toPtr (cb . parseChangeEvent)))

onKeyDown :: ((KeyboardEvent Elem) -> IO ()) -> EventHandler
onKeyDown cb = EventHandler (js_set_onKeyDown (toPtr (cb . parseKeyboardEvent)))

onKeyPress :: ((KeyboardEvent Elem) -> IO ()) -> EventHandler
onKeyPress cb = EventHandler (js_set_onKeyPress (toPtr (cb . parseKeyboardEvent)))

onKeyUp :: ((KeyboardEvent Elem) -> IO ()) -> EventHandler
onKeyUp cb = EventHandler (js_set_onKeyUp (toPtr (cb . parseKeyboardEvent)))

onBlur :: ((FocusEvent Elem) -> IO ()) -> EventHandler
onBlur cb = EventHandler (js_set_onBlur (toPtr (cb . parseFocusEvent)))
    
element :: (Wrapper r) => (RawAttrs -> ReactArray -> IO React) -> [r (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
element constructor attrs ehs content =
  do attr <- js_empty_object
     mapM ((setField attr) . unWrap) attrs
     mapM_ ($ attr) (map unEventHandler ehs)
     chldArr <- js_ReactArray_empty
     mapM (js_ReactArray_push chldArr) content
     constructor attr chldArr
  where
    setField attr (fld, (AttrString v)) = js_set_field_String attr fld v
    setField attr (fld, (AttrBool True)) = js_set_field_True attr fld
    setField attr (fld, (AttrBool False)) = js_set_field_False attr fld
    setField attr (fld, (AttrDouble v)) = js_set_field_Double attr fld v
    setField attr (fld, (AttrInt v)) = js_set_field_Int attr fld v
    setField attr (fld, (AttrDict vs)) = do d <- js_empty_object
                                            mapM_ (\(k,v)-> js_set_field_String d k v) vs
                                            js_set_field_Obj attr fld d
    setField attr (fld, AttrNil) = return ()

voidElement :: (Wrapper r) => (RawAttrs -> IO React) -> [r (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
voidElement constructor attrs ehs = element (\a c -> constructor a) attrs ehs []

textElement :: (Wrapper r) => (RawAttrs -> JSString -> IO React) -> [r (JSString, (AttrValue JSString))] -> [EventHandler] -> String -> IO React
textElement constructor attrs ehs t = element (\a c -> constructor a (toJSStr t)) attrs ehs =<< sequence [text t]


-- React.DOM.*

a :: [AAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
a = element js_React_DOM_a

abbr :: [AbbrAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
abbr = element js_React_DOM_abbr

address :: [AddressAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
address = element js_React_DOM_address

area :: [AreaAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
area = voidElement js_React_DOM_area

article :: [ArticleAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
article = element js_React_DOM_article

aside :: [AsideAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
aside = element js_React_DOM_aside

audio :: [AudioAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
audio = element js_React_DOM_audio

b :: [BAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
b = element js_React_DOM_b

base :: [BaseAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
base = voidElement js_React_DOM_base

bdi :: [BdiAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
bdi = element js_React_DOM_bdi

bdo :: [BdoAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
bdo = element js_React_DOM_bdo

big :: [BigAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
big = element js_React_DOM_big

blockquote :: [BlockquoteAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
blockquote = element js_React_DOM_blockquote

body :: [BodyAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
body = element js_React_DOM_body

br :: [BrAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
br = voidElement js_React_DOM_br

button :: [ButtonAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
button = element js_React_DOM_button

canvas :: [CanvasAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
canvas = element js_React_DOM_canvas

caption :: [CaptionAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
caption = element js_React_DOM_caption

cite :: [CiteAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
cite = element js_React_DOM_cite

code :: [CodeAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
code = element js_React_DOM_code

col :: [ColAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
col = voidElement js_React_DOM_col

colgroup :: [ColgroupAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
colgroup = element js_React_DOM_colgroup

dataElt :: [DataAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
dataElt = element js_React_DOM_data

datalist :: [DatalistAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
datalist = element js_React_DOM_datalist

dd :: [DdAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
dd = element js_React_DOM_dd

del :: [DelAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
del = element js_React_DOM_del

details :: [DetailsAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
details = element js_React_DOM_details

dfn :: [DfnAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
dfn = element js_React_DOM_dfn

div :: [DivAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
div = element js_React_DOM_div

dl :: [DlAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
dl = element js_React_DOM_dl

dt :: [DtAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
dt = element js_React_DOM_dt

em :: [EmAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
em = element js_React_DOM_em

embed :: [EmbedAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
embed = voidElement js_React_DOM_embed

fieldset :: [FieldsetAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
fieldset = element js_React_DOM_fieldset

figcaption :: [FigcaptionAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
figcaption = element js_React_DOM_figcaption

figure :: [FigureAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
figure = element js_React_DOM_figure

footer :: [FooterAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
footer = element js_React_DOM_footer

form :: [FormAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
form = element js_React_DOM_form

h1 :: [H1Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h1 = element js_React_DOM_h1

h2 :: [H2Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h2 = element js_React_DOM_h2

h3 :: [H3Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h3 = element js_React_DOM_h3

h4 :: [H4Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h4 = element js_React_DOM_h4

h5 :: [H5Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h5 = element js_React_DOM_h5

h6 :: [H6Attr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
h6 = element js_React_DOM_h6

head :: [HeadAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
head = element js_React_DOM_head

header :: [HeaderAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
header = element js_React_DOM_header

hr :: [HrAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
hr = voidElement js_React_DOM_hr

html :: [HtmlAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
html = element js_React_DOM_html

i :: [IAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
i = element js_React_DOM_i

iframe :: [IframeAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
iframe = element js_React_DOM_iframe

img :: [ImgAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
img = voidElement js_React_DOM_img

input :: [InputAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
input = voidElement js_React_DOM_input

ins :: [InsAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
ins = element js_React_DOM_ins

kbd :: [KbdAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
kbd = element js_React_DOM_kbd

keygen :: [KeygenAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
keygen = voidElement js_React_DOM_keygen

label :: [LabelAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
label = element js_React_DOM_label

legend :: [LegendAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
legend = element js_React_DOM_legend

li :: [LiAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
li = element js_React_DOM_li

link :: [LinkAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
link = voidElement js_React_DOM_link

main :: [MainAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
main = element js_React_DOM_main

mapElt :: [MapAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
mapElt = element js_React_DOM_map

mark :: [MarkAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
mark = element js_React_DOM_mark

menu :: [MenuAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
menu = element js_React_DOM_menu

menuitem :: [MenuitemAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
menuitem = element js_React_DOM_menuitem

meta :: [MetaAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
meta = voidElement js_React_DOM_meta

meter :: [MeterAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
meter = element js_React_DOM_meter

nav :: [NavAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
nav = element js_React_DOM_nav

noscript :: [NoscriptAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
noscript = element js_React_DOM_noscript

object :: [ObjectAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
object = element js_React_DOM_object

ol :: [OlAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
ol = element js_React_DOM_ol

optgroup :: [OptgroupAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
optgroup = element js_React_DOM_optgroup

option :: [OptionAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
option = element js_React_DOM_option

output :: [OutputAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
output = element js_React_DOM_output

p :: [PAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
p = element js_React_DOM_p

param :: [ParamAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
param = voidElement js_React_DOM_param

pre :: [PreAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
pre = element js_React_DOM_pre

progress :: [ProgressAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
progress = element js_React_DOM_progress

q :: [QAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
q = element js_React_DOM_q

rp :: [RpAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
rp = element js_React_DOM_rp

rt :: [RtAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
rt = element js_React_DOM_rt

ruby :: [RubyAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
ruby = element js_React_DOM_ruby

s :: [SAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
s = element js_React_DOM_s

samp :: [SampAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
samp = element js_React_DOM_samp

script :: [ScriptAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> String -> IO React
script = textElement js_React_DOM_script

section :: [SectionAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
section = element js_React_DOM_section

select :: [SelectAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
select = element js_React_DOM_select

small :: [SmallAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
small = element js_React_DOM_small

source :: [SourceAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
source = voidElement js_React_DOM_source

span :: [SpanAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
span = element js_React_DOM_span

strong :: [StrongAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
strong = element js_React_DOM_strong

styleElt :: [StyleAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> String -> IO React
styleElt = textElement js_React_DOM_style

sub :: [SubAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
sub = element js_React_DOM_sub

summary :: [SummaryAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
summary = element js_React_DOM_summary

sup :: [SupAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
sup = element js_React_DOM_sup

table :: [TableAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
table = element js_React_DOM_table

tbody :: [TbodyAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
tbody = element js_React_DOM_tbody

td :: [TdAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
td = element js_React_DOM_td

textarea :: [TextareaAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> String -> IO React
textarea = textElement js_React_DOM_textarea

tfoot :: [TfootAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
tfoot = element js_React_DOM_tfoot

th :: [ThAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
th = element js_React_DOM_th

thead :: [TheadAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
thead = element js_React_DOM_thead

time :: [TimeAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
time = element js_React_DOM_time

titleElt :: [TitleAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> String -> IO React
titleElt = textElement js_React_DOM_title

tr :: [TrAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
tr = element js_React_DOM_tr

track :: [TrackAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
track = voidElement js_React_DOM_track

u :: [UAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
u = element js_React_DOM_u

ul :: [UlAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
ul = element js_React_DOM_ul

var :: [VarAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
var = element js_React_DOM_var

video :: [VideoAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> [React] -> IO React
video = element js_React_DOM_video

wbr :: [WbrAttr (JSString, (AttrValue JSString))] -> [EventHandler] -> IO React
wbr = voidElement js_React_DOM_wbr

text :: String -> IO React
text s = ffi (toJSStr "(function(txt){return txt;})") s

renderComponent :: Elem -> React -> IO ()
renderComponent = ffi (toJSStr "(function(e,r){React.renderComponent(r,e);})")
