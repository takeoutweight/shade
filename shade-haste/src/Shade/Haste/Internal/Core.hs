{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeSynonymInstances #-}
module Shade.Haste.Internal.Core where
import Control.Applicative (Applicative)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as D
import Data.Maybe (catMaybes)
import Haste.DOM (Elem)
import Haste.Prim (JSString, fromJSStr)
import Shade.Core
import qualified Shade.Haste.Internal.React as R

-- a is the return type of the Async. various "i" sources can trigger it, and various handlers can be installed into the callback.
-- the callback is i -> IO () because the actual buttons ec will call it with their triggering data. "listen" ensures it's converted to an 'a'
-- We could have the atuall callbacks use the i->a function too, but probably same difference?
data AsyncSource a = forall i . AsyncSource (i -> a, MVar [i -> IO ()])
data AsyncImpl a = AsyncImpl [AsyncSource a]

instance Functor AsyncImpl where
  fmap f (AsyncImpl as) = AsyncImpl (map (\(AsyncSource (a,b)) -> AsyncSource (f . a, b)) as)
  
instance FireFirst AsyncImpl where
  fireFirst as = AsyncImpl (concat (map unwrap as))
    where
      unwrap (AsyncImpl v) = v

mkAsyncs :: IO (R.React -> IO (), [(R.EventHandler, IO Bool)], ElemAsyncs ShadeHaste)
mkAsyncs = 
  do (mvClick, asClick) <- mkMVar
     (mvDoubleClick, asDoubleClick) <- mkMVar
     (mvChange, asChange) <- mkMVar
     (mvKeyUp, asKeyUp) <- mkMVar
     (mvKeyPress, asKeyPress) <- mkMVar
     (mvKeyDown, asKeyDown) <- mkMVar
     (mvBlur, asBlur) <- mkMVar
     domVar <- newMVar []
     return
       ( \r -> modifyMVar_ domVar (\v -> return (r : v))
       , [(R.onClick (fire mvClick), someListeners mvClick)
         ,(R.onDoubleClick (fire mvDoubleClick), someListeners mvDoubleClick)
         ,(R.onChange (fire mvChange), someListeners mvChange)
         ,(R.onKeyUp (fire mvKeyUp), someListeners mvKeyUp)
         ,(R.onKeyPress (fire mvKeyPress), someListeners mvKeyPress)
         ,(R.onKeyDown (fire mvKeyDown), someListeners mvKeyDown)
         ,(R.onBlur (fire mvBlur), someListeners mvBlur)
         ]
       , (ElemAsyncs
            { onClick = asClick
            , onDoubleClick = asDoubleClick
            , onChange = asChange
            , onKeyUp = asKeyUp
            , onKeyPress = asKeyPress
            , onKeyDown = asKeyDown
            , onBlur = asBlur
            , domElements = fmap catMaybes (mapM R.getDomNode =<< readMVar domVar)
            }))
  where
    mkMVar = do mv <- newMVar []
                return (mv, AsyncImpl [AsyncSource (id, mv)])
    fire mv evt = do cbs <- readMVar mv
                     sequence_ (map ($! evt) cbs) -- React pools and wrecks existing event objects, so we have to strictly read them asap.
    someListeners mv = fmap (not . null) (readMVar mv)


listenedCallbacks cbs = mapM (\(h,s) -> do some <- s
                                           if some
                                             then return (Just h)
                                             else return Nothing) cbs

defaultElement constructor attrs children =
  ShadeHaste $ do (_, chlds) <- liftIO (runWriterT (runShadeHaste children))
                  (writeReactElt, callbacks, asyncs) <- liftIO mkAsyncs
                  tell (D.singleton
                        (do c <- (sequence (D.toList chlds))
                            mcbs <- listenedCallbacks callbacks
                            r <- constructor attrs (catMaybes mcbs) c
                            writeReactElt r
                            return r))
                  return asyncs
  
voidElement constructor attrs =
  ShadeHaste $ do (writeReactElt, callbacks, asyncs) <- liftIO mkAsyncs
                  tell (D.singleton
                        (do mcbs <- listenedCallbacks callbacks
                            r <- constructor attrs (catMaybes mcbs)
                            writeReactElt r
                            return r))
                  return asyncs
  
instance ToString JSString where
  toString = fromJSStr

newtype ShadeHaste a = ShadeHaste {runShadeHaste :: WriterT (DList (IO R.React)) IO a} deriving (Functor, Applicative, Monad)

instance Shade ShadeHaste where
  type Async ShadeHaste = AsyncImpl
  type NativeString ShadeHaste = JSString
  type NativeElem ShadeHaste = Elem
  button a c = defaultElement R.button a c
  div a c = defaultElement R.div a c
  header a c = defaultElement R.header a c
  h1 a c = defaultElement R.h1 a c
  section a c = defaultElement R.section a c
  ul a c = defaultElement R.ul a c
  li a c = defaultElement R.li a c
  label a c = defaultElement R.label a c
  footer a c = defaultElement R.footer a c
  span a c = defaultElement R.span a c
  strong a c = defaultElement R.strong a c
  a attrs c = defaultElement R.a attrs c
  input a = voidElement R.input a
  text s = ShadeHaste ((tell (D.singleton (R.text s))) >> return ())
  letElt c = ShadeHaste $ do (s, chlds) <- liftIO (runWriterT (runShadeHaste c))
                             return (s, ShadeHaste (tell chlds >> return s))

listen :: AsyncImpl a -> (a -> IO ()) -> IO ()
listen (AsyncImpl as) callb = mapM_ addCB as
  where
    addCB (AsyncSource (itoa, mv)) = modifyMVar_ mv (\cbs -> return ( (callb . itoa) : cbs))

runClient :: ShadeHaste a -> IO (a, [IO R.React])
runClient c = do (s, cs) <- runWriterT (runShadeHaste c)
                 return (s, D.toList cs)

renderClient :: Elem -> [IO R.React] -> IO ()
renderClient e rs = 
  do relts <- sequence rs
     case relts of
        (a:_) -> R.renderComponent e a -- TODO: Just attaching first thing at the toplevel. Defensive div wrapper? is multiple OK?
        _ -> return ()
