module Shade.Core.Internal.Attributes where
import Data.List (intercalate)
import Data.String (IsString, fromString)

data AttrValue s = AttrString s -- NativeString
                 | AttrBool Bool
                 | AttrDouble Double
                 | AttrInt Int
                 | AttrDict [(s,s)]
                 | AttrNil

stringAttr :: (IsString s) => String -> AttrValue s
stringAttr a = AttrString (fromString a)

class Wrapper w where
  wrap :: a -> w a
  unWrap :: w a -> a

-- Attributes

class (Wrapper w) => Accept w where
  accept :: (IsString s) => String -> w (s, AttrValue s)
  accept a = wrap (fromString "accept", stringAttr a)
instance Accept FormAttr
instance Accept InputAttr

class (Wrapper w) => Action w where
  action :: (IsString s) => String -> w (s, AttrValue s)
  action a = wrap (fromString "action", stringAttr a)
instance Action FormAttr
-- 

class (Wrapper w) => AllowFullScreen w where
  allowFullScreen :: (IsString s) => String -> w (s, AttrValue s)
  allowFullScreen a = wrap (fromString "allowFullScreen", stringAttr a)
instance AllowFullScreen IframeAttr
-- 

class (Wrapper w) => AllowTransparency w where
  allowTransparency :: (IsString s) => String -> w (s, AttrValue s)
  allowTransparency a = wrap (fromString "allowTransparency", stringAttr a)
instance AllowTransparency IframeAttr
-- 

class (Wrapper w) => Async w where
  async :: (IsString s) => Bool -> w (s, AttrValue s) -- - present/not-present
  async a = wrap (fromString "async", AttrBool a)
instance Async ScriptAttr
-- 

class (Wrapper w) => AutoFocus w where
  autoFocus :: (IsString s) => Bool -> w (s, AttrValue s)
  autoFocus a = wrap (fromString "autoFocus", AttrBool a)
instance AutoFocus TextareaAttr
instance AutoFocus SelectAttr
instance AutoFocus KeygenAttr
instance AutoFocus InputAttr
instance AutoFocus ButtonAttr
--

class (Wrapper w) => AutoComplete w where
  autoComplete :: (IsString s) => Bool -> w (s, AttrValue s)
  autoComplete a = wrap (fromString "autoComplete", AttrBool a)
instance AutoComplete InputAttr
instance AutoComplete FormAttr
--

class (Wrapper w) => AutoPlay w where
  autoPlay :: (IsString s) => Bool -> w (s, AttrValue s) -- test
  autoPlay a = wrap (fromString "autoPlay", AttrBool a)
instance AutoPlay VideoAttr
instance AutoPlay AudioAttr
--

class (Wrapper w) => CellPadding w where
  cellPadding :: (IsString s) => String -> w (s, AttrValue s) -- (deprecated).
  cellPadding a = wrap (fromString "cellPadding", stringAttr a)
instance CellPadding TableAttr
-- 

class (Wrapper w) => CellSpacing w where
  cellSpacing :: (IsString s) => String -> w (s, AttrValue s)
  cellSpacing a = wrap (fromString "cellSpacing", stringAttr a)
instance CellSpacing TableAttr
-- 

class (Wrapper w) => Charset w where
  charset :: (IsString s) => String -> w (s, AttrValue s)
  charset a = wrap (fromString "charset", stringAttr a)
instance Charset ScriptAttr
instance Charset MetaAttr
--

class (Wrapper w) => Checked w where
  checked :: (IsString s) => Bool -> w (s, AttrValue s)
  checked a = wrap (fromString "checked", AttrBool a)
instance Checked InputAttr
instance Checked CommandAttr
--

class (Wrapper w) => Cols w where
  cols :: (IsString s) => String -> w (s, AttrValue s)
  cols a = wrap (fromString "cols", stringAttr a)
instance Cols TextareaAttr
-- 

class (Wrapper w) => ColSpan w where
  colSpan :: (IsString s) => Int -> w (s, AttrValue s) -- Q: Merge with wowspan?
  colSpan a = wrap (fromString "colSpan", AttrInt a)
instance ColSpan ThAttr
instance ColSpan TdAttr
--

class (Wrapper w) => Content w where
  content :: (IsString s) => String -> w (s, AttrValue s)
  content a = wrap (fromString "content", stringAttr a)
instance Content MetaAttr
-- 

class (Wrapper w) => Controls w where
  controls :: (IsString s) => Bool -> w (s, AttrValue s)
  controls a = wrap (fromString "controls", AttrBool a)
instance Controls VideoAttr
instance Controls AudioAttr
--

class (Wrapper w) => DataURL w where
  dataURL :: (IsString s) => String -> w (s, AttrValue s)
  dataURL a = wrap (fromString "dataURL", stringAttr a)
instance DataURL ObjectAttr
-- 

class (Wrapper w) => DateTime w where
  dateTime :: (IsString s) => String -> w (s, AttrValue s)
  dateTime a = wrap (fromString "dateTime", stringAttr a)
instance DateTime TimeAttr
instance DateTime InsAttr
instance DateTime DelAttr
--

class (Wrapper w) => Defer w where
  defer :: (IsString s) => String -> w (s, AttrValue s)
  defer a = wrap (fromString "defer", stringAttr a)
instance Defer ScriptAttr
-- 

class (Wrapper w) => Disabled w where
  disabled :: (IsString s) => Bool -> w (s, AttrValue s)
  disabled a = wrap (fromString "disabled", AttrBool a)
instance Disabled TextareaAttr
instance Disabled SelectAttr
instance Disabled OptionAttr
instance Disabled OptgroupAttr
instance Disabled KeygenAttr
instance Disabled InputAttr
instance Disabled FieldsetAttr
instance Disabled CommandAttr
instance Disabled ButtonAttr
--

class (Wrapper w) => EncType w where
  encType :: (IsString s) => String -> w (s, AttrValue s)
  encType a = wrap (fromString "encType", stringAttr a)
instance EncType FormAttr
-- 

class (Wrapper w) => FrameBorder w where
  frameBorder :: (IsString s) => String -> w (s, AttrValue s) -- HTML4 only
  frameBorder a = wrap (fromString "frameBorder", stringAttr a)
instance FrameBorder IframeAttr
-- 

class (Wrapper w) => FormId w where
  formId :: (IsString s) => String -> w (s, AttrValue s) -- TODO Id
  formId a = wrap (fromString "form", stringAttr a)
instance FormId TextareaAttr
instance FormId SelectAttr
instance FormId ProgressAttr
instance FormId OutputAttr
instance FormId ObjectAttr
instance FormId MeterAttr
instance FormId LabelAttr
instance FormId KeygenAttr
instance FormId InputAttr
instance FormId FieldsetAttr
instance FormId ButtonAttr
--

class (Wrapper w) => FormNoValidate w where
  formNoValidate :: (IsString s) => Bool -> w (s, AttrValue s) -- not listed on mdn
  formNoValidate a = wrap (fromString "formNoValidate", AttrBool a)
instance FormNoValidate InputAttr
instance FormNoValidate ButtonAttr
--

class (Wrapper w) => Height w where
  height :: (IsString s) => String -> w (s, AttrValue s) -- Q: Combine with width?
  height a = wrap (fromString "height", stringAttr a)
instance Height VideoAttr
instance Height ObjectAttr
instance Height InputAttr
instance Height ImgAttr
instance Height IframeAttr
instance Height EmbedAttr
instance Height CanvasAttr
--

class (Wrapper w) => Href w where
  href :: (IsString s) => String -> w (s, AttrValue s)
  href a = wrap (fromString "href", stringAttr a)
instance Href LinkAttr
instance Href BaseAttr
instance Href AreaAttr
instance Href AAttr
--

class (Wrapper w) => HtmlFor w where
  htmlFor :: (IsString s) => String -> w (s, AttrValue s) -- TODO Id
  htmlFor a = wrap (fromString "htmlFor", stringAttr a)
instance HtmlFor OutputAttr
instance HtmlFor LabelAttr
--

class (Wrapper w) => HttpEquiv w where
  httpEquiv :: (IsString s) => String -> w (s, AttrValue s)
  httpEquiv a = wrap (fromString "httpEquiv", stringAttr a)
instance HttpEquiv MetaAttr
-- 

class (Wrapper w) => Icon w where
  icon :: (IsString s) => String -> w (s, AttrValue s)
  icon a = wrap (fromString "icon", stringAttr a)
instance Icon CommandAttr
-- 

class (Wrapper w) => LabelName w where
  labelName :: (IsString s) => String -> w (s, AttrValue s)
  labelName a = wrap (fromString "label", stringAttr a)
instance LabelName TrackAttr
-- 

class (Wrapper w) => List w where
  list :: (IsString s) => String -> w (s, AttrValue s) -- Id of a datalist
  list a = wrap (fromString "list", stringAttr a)
instance List InputAttr
-- 

class (Wrapper w) => Loop w where
  loop :: (IsString s) => Bool -> w (s, AttrValue s)
  loop a = wrap (fromString "loop", AttrBool a)
instance Loop VideoAttr
instance Loop AudioAttr
--

class (Wrapper w) => Max w where
  max :: (IsString s) => String -> w (s, AttrValue s)
  max a = wrap (fromString "max", stringAttr a)
instance Max ProgressAttr
instance Max MeterAttr
instance Max InputAttr
--

class (Wrapper w) => MaxLength w where
  maxLength :: (IsString s) => (Maybe Int) -> w (s, AttrValue s)
  maxLength a = wrap (fromString "maxLength", case a of  -- FIXME - Is this right?
                                             Just i -> AttrInt i
                                             Nothing -> AttrNil)
instance MaxLength TextareaAttr
instance MaxLength InputAttr
--

class (Wrapper w) => Method w where
  method :: (IsString s) => String -> w (s, AttrValue s)
  method a = wrap (fromString "method", stringAttr a)
instance Method FormAttr
-- 

class (Wrapper w) => Min w where
  min :: (IsString s) => String -> w (s, AttrValue s)
  min a = wrap (fromString "min", stringAttr a)
instance Min MeterAttr
instance Min InputAttr
--

class (Wrapper w) => Multiple w where
  multiple :: (IsString s) => Bool -> w (s, AttrValue s)
  multiple a = wrap (fromString "multiple", AttrBool a)
instance Multiple SelectAttr
instance Multiple InputAttr
--

class (Wrapper w) => Name w where
  name :: (IsString s) => String -> w (s, AttrValue s)
  name a = wrap (fromString "name", stringAttr a)
instance Name ParamAttr
instance Name MetaAttr
instance Name MapAttr
instance Name TextareaAttr
instance Name SelectAttr
instance Name OutputAttr
instance Name ObjectAttr
instance Name KeygenAttr
instance Name InputAttr
instance Name IframeAttr
instance Name FieldsetAttr
instance Name FormAttr
instance Name ButtonAttr
--

class (Wrapper w) => NoValidate w where
  noValidate :: (IsString s) => String -> w (s, AttrValue s)
  noValidate a = wrap (fromString "noValidate", stringAttr a)
instance NoValidate FormAttr
-- 

class (Wrapper w) => Pattern w where
  pattern :: (IsString s) => String -> w (s, AttrValue s)
  pattern a = wrap (fromString "pattern", stringAttr a)
instance Pattern InputAttr
-- 

class (Wrapper w) => Placeholder w where
  placeholder :: (IsString s) => String -> w (s, AttrValue s)
  placeholder a = wrap (fromString "placeholder", stringAttr a)
instance Placeholder TextareaAttr
instance Placeholder InputAttr
--

class (Wrapper w) => Preload w where
  preload :: (IsString s) => String -> w (s, AttrValue s) -- todo none, metadata(default), auto
  preload a = wrap (fromString "preload", stringAttr a)
instance Preload VideoAttr
instance Preload AudioAttr
--

class (Wrapper w) => Poster w where
  poster :: (IsString s) => String -> w (s, AttrValue s) -- url
  poster a = wrap (fromString "poster", stringAttr a)
instance Poster VideoAttr
-- 

class (Wrapper w) => RadioGroup w where
  radioGroup :: (IsString s) => String -> w (s, AttrValue s)
  radioGroup a = wrap (fromString "radioGroup", stringAttr a)
instance RadioGroup CommandAttr
-- 

class (Wrapper w) => ReadOnly w where
  readOnly :: (IsString s) => Bool -> w (s, AttrValue s)
  readOnly a = wrap (fromString "readOnly", AttrBool a)
instance ReadOnly TextareaAttr
instance ReadOnly InputAttr
--

class (Wrapper w) => Rel w where
  rel :: (IsString s) => String -> w (s, AttrValue s)
  rel a = wrap (fromString "rel", stringAttr a)
instance Rel LinkAttr
instance Rel AreaAttr
instance Rel AAttr
--

class (Wrapper w) => Required w where
  required :: (IsString s) => Bool -> w (s, AttrValue s)
  required a = wrap (fromString "required", AttrBool a)
instance Required TextareaAttr
instance Required SelectAttr
instance Required InputAttr
--

class (Wrapper w) => Rows w where
  rows :: (IsString s) => String -> w (s, AttrValue s)
  rows a = wrap (fromString "rows", stringAttr a)
instance Rows TextareaAttr
-- 

class (Wrapper w) => RowSpan w where
  rowSpan :: (IsString s) => Int -> w (s, AttrValue s)
  rowSpan a = wrap (fromString "rowSpan", AttrInt a)
instance RowSpan ThAttr
instance RowSpan TdAttr
--

class (Wrapper w) => Sandbox w where
  sandbox :: (IsString s) => String -> w (s, AttrValue s)
  sandbox a = wrap (fromString "sandbox", stringAttr a)
instance Sandbox IframeAttr
-- 

class (Wrapper w) => Scope w where
  scope :: (IsString s) => String -> w (s, AttrValue s)
  scope a = wrap (fromString "scope", stringAttr a)
instance Scope ThAttr
-- 

class (Wrapper w) => Seamless w where
  seamless :: (IsString s) => Bool -> w (s, AttrValue s)
  seamless a = wrap (fromString "seamless", AttrBool a)
instance Seamless IframeAttr
-- 

class (Wrapper w) => Selected w where
  selected :: (IsString s) => Bool -> w (s, AttrValue s)
  selected a = wrap (fromString "selected", AttrBool a)
instance Selected OptionAttr
-- 

class (Wrapper w) => Size w where
  size :: (IsString s) => Int -> w (s, AttrValue s)
  size a = wrap (fromString "size", AttrInt a)
instance Size SelectAttr
instance Size InputAttr
--

class (Wrapper w) => SpanWidth w where
  spanWidth :: (IsString s) => Int -> w (s, AttrValue s)
  spanWidth a = wrap (fromString "span", AttrInt a)
instance SpanWidth ColgroupAttr
instance SpanWidth ColAttr
--

class (Wrapper w) => Src w where
  src :: (IsString s) => String -> w (s, AttrValue s)
  src a = wrap (fromString "src", stringAttr a)
instance Src VideoAttr
instance Src TrackAttr
instance Src SourceAttr
instance Src ScriptAttr
instance Src InputAttr
instance Src ImgAttr
instance Src IframeAttr
instance Src EmbedAttr
instance Src AudioAttr
--

class (Wrapper w) => SrcDoc w where
  srcDoc :: (IsString s) => String -> w (s, AttrValue s)
  srcDoc a = wrap (fromString "srcDoc", stringAttr a)
instance SrcDoc IframeAttr
-- 

class (Wrapper w) => Step w where
  step :: (IsString s) => String -> w (s, AttrValue s) -- 'any' or  positive float
  step a = wrap (fromString "step", stringAttr a)
instance Step InputAttr
-- 

class (Wrapper w) => Target w where
  target :: (IsString s) => String -> w (s, AttrValue s)
  target a = wrap (fromString "target", stringAttr a)
instance Target FormAttr
instance Target BaseAttr
instance Target AreaAttr
instance Target AAttr
--

class (Wrapper w) => TypeInfo w where
  typeInfo :: (IsString s) => String -> w (s, AttrValue s)
  typeInfo a = wrap (fromString "type", stringAttr a)
instance TypeInfo MenuAttr
instance TypeInfo StyleAttr
instance TypeInfo SourceAttr
instance TypeInfo ScriptAttr
instance TypeInfo ObjectAttr
instance TypeInfo EmbedAttr
instance TypeInfo CommandAttr
instance TypeInfo InputAttr
instance TypeInfo ButtonAttr
--

class (Wrapper w) => Value w where
  value :: (IsString s) => s -> w (s, AttrValue s)
  value a = wrap (fromString "value", AttrString a)
instance Value ParamAttr
instance Value ProgressAttr
instance Value MeterAttr
instance Value LiAttr
instance Value InputAttr
instance Value OptionAttr
instance Value ButtonAttr
--

class (Wrapper w) => Width w where
  width :: (IsString s) => String -> w (s, AttrValue s)
  width a = wrap (fromString "width", stringAttr a)
instance Width VideoAttr
instance Width ObjectAttr
instance Width InputAttr
instance Width ImgAttr
instance Width IframeAttr
instance Width EmbedAttr
instance Width CanvasAttr
--

class (Wrapper w) => Wmode w where
  wmode :: (IsString s) => String -> w (s, AttrValue s)
  wmode a = wrap (fromString "wmode", stringAttr a)
instance Wmode ObjectAttr
instance Wmode EmbedAttr
--

-- Global Attributes

-- Global attributes
accessKey       :: (IsString s, Wrapper w) => String -> w (s, AttrValue s)
accessKey a       = wrap (fromString "accessKey",stringAttr a)
className       :: (IsString s, Wrapper w) => [String] -> w (s, AttrValue s)
className a       = wrap (fromString "className",stringAttr (intercalate " " a))
contentEditable :: (IsString s, Wrapper w) => Bool -> w (s, AttrValue s)
contentEditable a = wrap (fromString "contentEditable",AttrBool a)
contextMenu     :: (IsString s, Wrapper w) => String -> w (s, AttrValue s) -- TODO
contextMenu a     = wrap (fromString "contextMenu",stringAttr a)
dir             :: (IsString s, Wrapper w) => String -> w (s, AttrValue s) -- TODO ltr,rtl,auto
dir a             = wrap (fromString "dir",stringAttr a)
draggable       :: (IsString s, Wrapper w) => String -> w (s, AttrValue s) -- TODO "true","false","auto"
draggable a       = wrap (fromString "draggable",stringAttr a)
hidden          :: (IsString s, Wrapper w) => Bool -> w (s, AttrValue s)
hidden a          = wrap (fromString "hidden",AttrBool a)
idName          :: (IsString s, Wrapper w) => String -> w (s, AttrValue s)
idName a          = wrap (fromString "id",stringAttr a)
lang            :: (IsString s, Wrapper w) => String -> w (s, AttrValue s)
lang a            = wrap (fromString "lang",stringAttr a)
spellCheck      :: (IsString s, Wrapper w) => Bool -> w (s, AttrValue s) -- TODO true false inherited
spellCheck a      = wrap (fromString "spellCheck",AttrBool a)
style           :: (IsString s, Wrapper w) => [(String,String)] -> w (s, AttrValue s) -- TODO Clay?
style a           = wrap (fromString "style",AttrDict (map (\(k,v) -> (fromString k, fromString v)) a))
tabIndex        :: (IsString s, Wrapper w) => Int -> w (s, AttrValue s)
tabIndex a        = wrap (fromString "tabIndex",AttrInt a)
title           :: (IsString s, Wrapper w) => String -> w (s, AttrValue s)
title a           = wrap (fromString "title",stringAttr a)


-- Tags for elements to select appropriate Attributes

newtype AAttr a = AAttr {unAAttr :: a}
instance Wrapper AAttr where wrap = AAttr; unWrap = unAAttr
        
newtype AbbrAttr a = AbbrAttr {unAbbrAttr :: a}
instance Wrapper AbbrAttr where wrap = AbbrAttr; unWrap = unAbbrAttr
  
newtype AddressAttr a = AddressAttr {unAddressAttr :: a}
instance Wrapper AddressAttr where wrap = AddressAttr; unWrap = unAddressAttr
  
newtype AreaAttr a = AreaAttr {unAreaAttr :: a}
instance Wrapper AreaAttr where wrap = AreaAttr; unWrap = unAreaAttr
  
newtype ArticleAttr a = ArticleAttr {unArticleAttr :: a}
instance Wrapper ArticleAttr where wrap = ArticleAttr; unWrap = unArticleAttr
  
newtype AsideAttr a = AsideAttr {unAsideAttr :: a}
instance Wrapper AsideAttr where wrap = AsideAttr; unWrap = unAsideAttr
  
newtype AudioAttr a = AudioAttr {unAudioAttr :: a}
instance Wrapper AudioAttr where wrap = AudioAttr; unWrap = unAudioAttr
  
newtype BAttr a = BAttr {unBAttr :: a}
instance Wrapper BAttr where wrap = BAttr; unWrap = unBAttr
  
newtype BaseAttr a = BaseAttr {unBaseAttr :: a}
instance Wrapper BaseAttr where wrap = BaseAttr; unWrap = unBaseAttr
  
newtype BdiAttr a = BdiAttr {unBdiAttr :: a}
instance Wrapper BdiAttr where wrap = BdiAttr; unWrap = unBdiAttr
  
newtype BdoAttr a = BdoAttr {unBdoAttr :: a}
instance Wrapper BdoAttr where wrap = BdoAttr; unWrap = unBdoAttr
  
newtype BigAttr a = BigAttr {unBigAttr :: a}
instance Wrapper BigAttr where wrap = BigAttr; unWrap = unBigAttr
  
newtype BlockquoteAttr a = BlockquoteAttr {unBlockquoteAttr :: a}
instance Wrapper BlockquoteAttr where wrap = BlockquoteAttr; unWrap = unBlockquoteAttr
  
newtype BodyAttr a = BodyAttr {unBodyAttr :: a}
instance Wrapper BodyAttr where wrap = BodyAttr; unWrap = unBodyAttr
  
newtype BrAttr a = BrAttr {unBrAttr :: a}
instance Wrapper BrAttr where wrap = BrAttr; unWrap = unBrAttr
  
newtype ButtonAttr a = ButtonAttr {unButtonAttr :: a}
instance Wrapper ButtonAttr where wrap = ButtonAttr; unWrap = unButtonAttr

newtype CanvasAttr a = CanvasAttr {unCanvasAttr :: a}
instance Wrapper CanvasAttr where wrap = CanvasAttr; unWrap = unCanvasAttr
  
newtype CaptionAttr a = CaptionAttr {unCaptionAttr :: a}
instance Wrapper CaptionAttr where wrap = CaptionAttr; unWrap = unCaptionAttr
  
newtype CiteAttr a = CiteAttr {unCiteAttr :: a}
instance Wrapper CiteAttr where wrap = CiteAttr; unWrap = unCiteAttr
  
newtype CodeAttr a = CodeAttr {unCodeAttr :: a}
instance Wrapper CodeAttr where wrap = CodeAttr; unWrap = unCodeAttr
  
newtype ColAttr a = ColAttr {unColAttr :: a}
instance Wrapper ColAttr where wrap = ColAttr; unWrap = unColAttr
  
newtype ColgroupAttr a = ColgroupAttr {unColgroupAttr :: a}
instance Wrapper ColgroupAttr where wrap = ColgroupAttr; unWrap = unColgroupAttr
  
newtype DataAttr a = DataAttr {unDataAttr :: a}
instance Wrapper DataAttr where wrap = DataAttr; unWrap = unDataAttr
  
newtype DatalistAttr a = DatalistAttr {unDatalistAttr :: a}
instance Wrapper DatalistAttr where wrap = DatalistAttr; unWrap = unDatalistAttr
  
newtype DdAttr a = DdAttr {unDdAttr :: a}
instance Wrapper DdAttr where wrap = DdAttr; unWrap = unDdAttr
  
newtype DelAttr a = DelAttr {unDelAttr :: a}
instance Wrapper DelAttr where wrap = DelAttr; unWrap = unDelAttr
  
newtype DetailsAttr a = DetailsAttr {unDetailsAttr :: a}
instance Wrapper DetailsAttr where wrap = DetailsAttr; unWrap = unDetailsAttr
  
newtype DfnAttr a = DfnAttr {unDfnAttr :: a}
instance Wrapper DfnAttr where wrap = DfnAttr; unWrap = unDfnAttr
  
newtype DivAttr a = DivAttr {unDivAttr :: a}
instance Wrapper DivAttr where wrap = DivAttr; unWrap = unDivAttr
  
newtype DlAttr a = DlAttr {unDlAttr :: a}
instance Wrapper DlAttr where wrap = DlAttr; unWrap = unDlAttr
  
newtype DtAttr a = DtAttr {unDtAttr :: a}
instance Wrapper DtAttr where wrap = DtAttr; unWrap = unDtAttr
  
newtype EmAttr a = EmAttr {unEmAttr :: a}
instance Wrapper EmAttr where wrap = EmAttr; unWrap = unEmAttr
  
newtype EmbedAttr a = EmbedAttr {unEmbedAttr :: a}
instance Wrapper EmbedAttr where wrap = EmbedAttr; unWrap = unEmbedAttr
  
newtype FieldsetAttr a = FieldsetAttr {unFieldsetAttr :: a}
instance Wrapper FieldsetAttr where wrap = FieldsetAttr; unWrap = unFieldsetAttr
  
newtype FigcaptionAttr a = FigcaptionAttr {unFigcaptionAttr :: a}
instance Wrapper FigcaptionAttr where wrap = FigcaptionAttr; unWrap = unFigcaptionAttr
  
newtype FigureAttr a = FigureAttr {unFigureAttr :: a}
instance Wrapper FigureAttr where wrap = FigureAttr; unWrap = unFigureAttr
  
newtype FooterAttr a = FooterAttr {unFooterAttr :: a}
instance Wrapper FooterAttr where wrap = FooterAttr; unWrap = unFooterAttr
  
newtype FormAttr a = FormAttr {unFormAttr :: a}
instance Wrapper FormAttr where wrap = FormAttr; unWrap = unFormAttr
  
newtype H1Attr a = H1Attr {unH1Attr :: a}
instance Wrapper H1Attr where wrap = H1Attr; unWrap = unH1Attr
  
newtype H2Attr a = H2Attr {unH2Attr :: a}
instance Wrapper H2Attr where wrap = H2Attr; unWrap = unH2Attr
  
newtype H3Attr a = H3Attr {unH3Attr :: a}
instance Wrapper H3Attr where wrap = H3Attr; unWrap = unH3Attr
  
newtype H4Attr a = H4Attr {unH4Attr :: a}
instance Wrapper H4Attr where wrap = H4Attr; unWrap = unH4Attr
  
newtype H5Attr a = H5Attr {unH5Attr :: a}
instance Wrapper H5Attr where wrap = H5Attr; unWrap = unH5Attr
  
newtype H6Attr a = H6Attr {unH6Attr :: a}
instance Wrapper H6Attr where wrap = H6Attr; unWrap = unH6Attr
  
newtype HeadAttr a = HeadAttr {unHeadAttr :: a}
instance Wrapper HeadAttr where wrap = HeadAttr; unWrap = unHeadAttr
  
newtype HeaderAttr a = HeaderAttr {unHeaderAttr :: a}
instance Wrapper HeaderAttr where wrap = HeaderAttr; unWrap = unHeaderAttr
  
newtype HrAttr a = HrAttr {unHrAttr :: a}
instance Wrapper HrAttr where wrap = HrAttr; unWrap = unHrAttr
  
newtype HtmlAttr a = HtmlAttr {unHtmlAttr :: a}
instance Wrapper HtmlAttr where wrap = HtmlAttr; unWrap = unHtmlAttr
  
newtype IAttr a = IAttr {unIAttr :: a}
instance Wrapper IAttr where wrap = IAttr; unWrap = unIAttr
  
newtype IframeAttr a = IframeAttr {unIframeAttr :: a}
instance Wrapper IframeAttr where wrap = IframeAttr; unWrap = unIframeAttr
  
newtype ImgAttr a = ImgAttr {unImgAttr :: a}
instance Wrapper ImgAttr where wrap = ImgAttr; unWrap = unImgAttr
  
newtype InputAttr a = InputAttr {unInputAttr :: a}
instance Wrapper InputAttr where wrap = InputAttr; unWrap = unInputAttr
  
newtype InsAttr a = InsAttr {unInsAttr :: a}
instance Wrapper InsAttr where wrap = InsAttr; unWrap = unInsAttr
  
newtype KbdAttr a = KbdAttr {unKbdAttr :: a}
instance Wrapper KbdAttr where wrap = KbdAttr; unWrap = unKbdAttr
  
newtype KeygenAttr a = KeygenAttr {unKeygenAttr :: a}
instance Wrapper KeygenAttr where wrap = KeygenAttr; unWrap = unKeygenAttr
  
newtype LabelAttr a = LabelAttr {unLabelAttr :: a}
instance Wrapper LabelAttr where wrap = LabelAttr; unWrap = unLabelAttr
  
newtype LegendAttr a = LegendAttr {unLegendAttr :: a}
instance Wrapper LegendAttr where wrap = LegendAttr; unWrap = unLegendAttr
  
newtype LiAttr a = LiAttr {unLiAttr :: a}
instance Wrapper LiAttr where wrap = LiAttr; unWrap = unLiAttr
  
newtype LinkAttr a = LinkAttr {unLinkAttr :: a}
instance Wrapper LinkAttr where wrap = LinkAttr; unWrap = unLinkAttr
  
newtype MainAttr a = MainAttr {unMainAttr :: a}
instance Wrapper MainAttr where wrap = MainAttr; unWrap = unMainAttr
  
newtype MapAttr a = MapAttr {unMapAttr :: a}
instance Wrapper MapAttr where wrap = MapAttr; unWrap = unMapAttr
  
newtype MarkAttr a = MarkAttr {unMarkAttr :: a}
instance Wrapper MarkAttr where wrap = MarkAttr; unWrap = unMarkAttr
  
newtype MenuAttr a = MenuAttr {unMenuAttr :: a}
instance Wrapper MenuAttr where wrap = MenuAttr; unWrap = unMenuAttr
  
newtype MenuitemAttr a = MenuitemAttr {unMenuitemAttr :: a}
instance Wrapper MenuitemAttr where wrap = MenuitemAttr; unWrap = unMenuitemAttr
  
newtype MetaAttr a = MetaAttr {unMetaAttr :: a}
instance Wrapper MetaAttr where wrap = MetaAttr; unWrap = unMetaAttr
  
newtype MeterAttr a = MeterAttr {unMeterAttr :: a}
instance Wrapper MeterAttr where wrap = MeterAttr; unWrap = unMeterAttr
  
newtype NavAttr a = NavAttr {unNavAttr :: a}
instance Wrapper NavAttr where wrap = NavAttr; unWrap = unNavAttr
  
newtype NoscriptAttr a = NoscriptAttr {unNoscriptAttr :: a}
instance Wrapper NoscriptAttr where wrap = NoscriptAttr; unWrap = unNoscriptAttr
  
newtype ObjectAttr a = ObjectAttr {unObjectAttr :: a}
instance Wrapper ObjectAttr where wrap = ObjectAttr; unWrap = unObjectAttr
  
newtype OlAttr a = OlAttr {unOlAttr :: a}
instance Wrapper OlAttr where wrap = OlAttr; unWrap = unOlAttr
  
newtype OptgroupAttr a = OptgroupAttr {unOptgroupAttr :: a}
instance Wrapper OptgroupAttr where wrap = OptgroupAttr; unWrap = unOptgroupAttr
  
newtype OptionAttr a = OptionAttr {unOptionAttr :: a}
instance Wrapper OptionAttr where wrap = OptionAttr; unWrap = unOptionAttr
  
newtype OutputAttr a = OutputAttr {unOutputAttr :: a}
instance Wrapper OutputAttr where wrap = OutputAttr; unWrap = unOutputAttr
  
newtype PAttr a = PAttr {unPAttr :: a}
instance Wrapper PAttr where wrap = PAttr; unWrap = unPAttr
  
newtype ParamAttr a = ParamAttr {unParamAttr :: a}
instance Wrapper ParamAttr where wrap = ParamAttr; unWrap = unParamAttr
  
newtype PreAttr a = PreAttr {unPreAttr :: a}
instance Wrapper PreAttr where wrap = PreAttr; unWrap = unPreAttr
  
newtype ProgressAttr a = ProgressAttr {unProgressAttr :: a}
instance Wrapper ProgressAttr where wrap = ProgressAttr; unWrap = unProgressAttr
  
newtype QAttr a = QAttr {unQAttr :: a}
instance Wrapper QAttr where wrap = QAttr; unWrap = unQAttr
  
newtype RpAttr a = RpAttr {unRpAttr :: a}
instance Wrapper RpAttr where wrap = RpAttr; unWrap = unRpAttr
  
newtype RtAttr a = RtAttr {unRtAttr :: a}
instance Wrapper RtAttr where wrap = RtAttr; unWrap = unRtAttr
  
newtype RubyAttr a = RubyAttr {unRubyAttr :: a}
instance Wrapper RubyAttr where wrap = RubyAttr; unWrap = unRubyAttr
  
newtype SAttr a = SAttr {unSAttr :: a}
instance Wrapper SAttr where wrap = SAttr; unWrap = unSAttr
  
newtype SampAttr a = SampAttr {unSampAttr :: a}
instance Wrapper SampAttr where wrap = SampAttr; unWrap = unSampAttr
  
newtype ScriptAttr a = ScriptAttr {unScriptAttr :: a}
instance Wrapper ScriptAttr where wrap = ScriptAttr; unWrap = unScriptAttr
  
newtype SectionAttr a = SectionAttr {unSectionAttr :: a}
instance Wrapper SectionAttr where wrap = SectionAttr; unWrap = unSectionAttr
  
newtype SelectAttr a = SelectAttr {unSelectAttr :: a}
instance Wrapper SelectAttr where wrap = SelectAttr; unWrap = unSelectAttr
  
newtype SmallAttr a = SmallAttr {unSmallAttr :: a}
instance Wrapper SmallAttr where wrap = SmallAttr; unWrap = unSmallAttr
  
newtype SourceAttr a = SourceAttr {unSourceAttr :: a}
instance Wrapper SourceAttr where wrap = SourceAttr; unWrap = unSourceAttr
  
newtype SpanAttr a = SpanAttr {unSpanAttr :: a}
instance Wrapper SpanAttr where wrap = SpanAttr; unWrap = unSpanAttr
  
newtype StrongAttr a = StrongAttr {unStrongAttr :: a}
instance Wrapper StrongAttr where wrap = StrongAttr; unWrap = unStrongAttr
  
newtype StyleAttr a = StyleAttr {unStyleAttr :: a}
instance Wrapper StyleAttr where wrap = StyleAttr; unWrap = unStyleAttr
  
newtype SubAttr a = SubAttr {unSubAttr :: a}
instance Wrapper SubAttr where wrap = SubAttr; unWrap = unSubAttr
  
newtype SummaryAttr a = SummaryAttr {unSummaryAttr :: a}
instance Wrapper SummaryAttr where wrap = SummaryAttr; unWrap = unSummaryAttr
  
newtype SupAttr a = SupAttr {unSupAttr :: a}
instance Wrapper SupAttr where wrap = SupAttr; unWrap = unSupAttr
  
newtype TableAttr a = TableAttr {unTableAttr :: a}
instance Wrapper TableAttr where wrap = TableAttr; unWrap = unTableAttr
  
newtype TbodyAttr a = TbodyAttr {unTbodyAttr :: a}
instance Wrapper TbodyAttr where wrap = TbodyAttr; unWrap = unTbodyAttr
  
newtype TdAttr a = TdAttr {unTdAttr :: a}
instance Wrapper TdAttr where wrap = TdAttr; unWrap = unTdAttr
  
newtype TextareaAttr a = TextareaAttr {unTextareaAttr :: a}
instance Wrapper TextareaAttr where wrap = TextareaAttr; unWrap = unTextareaAttr
  
newtype TfootAttr a = TfootAttr {unTfootAttr :: a}
instance Wrapper TfootAttr where wrap = TfootAttr; unWrap = unTfootAttr
  
newtype ThAttr a = ThAttr {unThAttr :: a}
instance Wrapper ThAttr where wrap = ThAttr; unWrap = unThAttr
  
newtype TheadAttr a = TheadAttr {unTheadAttr :: a}
instance Wrapper TheadAttr where wrap = TheadAttr; unWrap = unTheadAttr
  
newtype TimeAttr a = TimeAttr {unTimeAttr :: a}
instance Wrapper TimeAttr where wrap = TimeAttr; unWrap = unTimeAttr
  
newtype TitleAttr a = TitleAttr {unTitleAttr :: a}
instance Wrapper TitleAttr where wrap = TitleAttr; unWrap = unTitleAttr
  
newtype TrAttr a = TrAttr {unTrAttr :: a}
instance Wrapper TrAttr where wrap = TrAttr; unWrap = unTrAttr
  
newtype TrackAttr a = TrackAttr {unTrackAttr :: a}
instance Wrapper TrackAttr where wrap = TrackAttr; unWrap = unTrackAttr
  
newtype UAttr a = UAttr {unUAttr :: a}
instance Wrapper UAttr where wrap = UAttr; unWrap = unUAttr
  
newtype UlAttr a = UlAttr {unUlAttr :: a}
instance Wrapper UlAttr where wrap = UlAttr; unWrap = unUlAttr
  
newtype VarAttr a = VarAttr {unVarAttr :: a}
instance Wrapper VarAttr where wrap = VarAttr; unWrap = unVarAttr
  
newtype VideoAttr a = VideoAttr {unVideoAttr :: a}
instance Wrapper VideoAttr where wrap = VideoAttr; unWrap = unVideoAttr
  
newtype WbrAttr a = WbrAttr {unWbrAttr :: a}
instance Wrapper WbrAttr where wrap = WbrAttr; unWrap = unWbrAttr

newtype CommandAttr a = CommandAttr {unCommandAttr :: a}
instance Wrapper CommandAttr where wrap = CommandAttr; unWrap = unCommandAttr
