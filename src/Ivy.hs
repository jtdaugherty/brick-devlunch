{-# LANGUAGE ForeignFunctionInterface #-}
module Ivy
    ( ivyMain
    )
where
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Control.Concurrent.STM

import System.IO (hPutStrLn, stderr)

foreign import ccall "IvyStart" ivyStart :: CString -> IO ()
foreign import ccall "IvyStop" _ivyStop :: IO ()
foreign import ccall "IvyMainLoop" ivyMainLoop :: IO ()
foreign import ccall "IvySendMsg" _ivySendMsg :: Ptr CChar -> IO ()

foreign import ccall "IvyInit" 
    ivyInit :: Ptr CChar -> -- application name
               Ptr CChar -> -- ready message
               Ptr a ->
               Ptr a ->
               Ptr a ->
               Ptr a ->
               IO ()

foreign import ccall "IvyBindMsg"
    ivyBindMsg :: FunPtr ( -- MsgCallback:
            Ptr a -> -- IvyClientPtr app
            Ptr a -> -- user data
            Int -> -- int argc
            Ptr (CString) ->  -- char **argv 
            IO ()) -> -- return void
        Ptr a -> -- void * user data
        CString -> -- const char *fmt_regexp
        IO () -- return IO void

foreign import ccall "wrapper"
    createIvyCb :: (Ptr a -> -- IvyClientPtr app
                    Ptr a -> -- void user data
                    Int -> -- int argv
                    Ptr (CString) -> -- char **argv 
                    IO ()) -> -- return void
        IO (FunPtr (
            Ptr a -> -- void IvyClientPtr app
            Ptr a -> -- void* user data
            Int -> -- int argv
            Ptr (CString) -> --char **argv 
            IO ())) -- return void
 
appName :: String
appName = "Haskell plotter"

-- `data_var` for sharing data
-- `source_var` is for sharing the sender ID (like AC_ID or for example 'ground')
-- source is always the first element of the parsed callback data
-- `expr` is the regular expression used for binding
-- `index` determines which index holds the data
ivyMain :: TVar Double -> TVar String -> String -> Int -> IO ()
ivyMain data_var source_var expr index = do
    app_name <- newCString appName
    ready_msg <- newCString $ appName ++ " ready!"
    ivyInit app_name ready_msg nullPtr nullPtr nullPtr nullPtr
    addr <- newCString ""
    ivyStart addr
    regexp <- newCString expr
    cb <- createIvyCb $ myCallback data_var source_var index
    ivyBindMsg cb nullPtr regexp
    ivyMainLoop

-- TODO: make this more flexible?
myCallback:: TVar Double -> TVar String ->
             Int -> Ptr a -> Ptr a -> Int -> Ptr (CString) -> IO ()
myCallback datapoint source index _ _ _ dataPtr = do
    val <- peekArray 2 dataPtr
    hPutStrLn stderr (show val)
    src <- peekCString (val !! 0)
    str <- peekCString (val !! 1)
    hPutStrLn stderr ("str = " ++ show str)
    let values = splitOn str
    hPutStrLn stderr ("values = " ++ concat values)
    atomically $ writeTVar datapoint (read ( values !! index ) :: Double)
    atomically $ writeTVar source src

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

splitOn :: String -> [String]
splitOn = wordsWhen (==' ') 
