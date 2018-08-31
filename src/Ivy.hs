{-# LANGUAGE ForeignFunctionInterface #-}
module Ivy
    ( ivyMain
    )
where
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Control.Concurrent.STM

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

ivyMain :: TVar Double -> String -> Int -> IO ()
ivyMain data_var expr index = do
    app_name <- newCString appName
    ready_msg <- newCString $ appName ++ " ready!"
    ivyInit app_name ready_msg nullPtr nullPtr nullPtr nullPtr
    addr <- newCString ""
    ivyStart addr
    regexp <- newCString expr
    cb <- createIvyCb $ myCallback data_var index
    ivyBindMsg cb nullPtr regexp
    ivyMainLoop

myCallback:: TVar Double -> Int -> Ptr a -> Ptr a -> Int -> Ptr (CString) -> IO ()
myCallback myVar index _ _ _ dataPtr = do
    val <- peek dataPtr
    str <- peekCString val
    atomically $ writeTVar myVar (read ( (splitOn str) !! index ) :: Double)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

splitOn :: String -> [String]
splitOn = wordsWhen (==' ') 
