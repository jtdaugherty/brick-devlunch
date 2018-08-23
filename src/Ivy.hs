{-# LANGUAGE ForeignFunctionInterface #-}
module Ivy where
import Foreign.C
import Foreign.Ptr

foreign import ccall "IvyStart" ivyStart :: CString -> IO ()
foreign import ccall "IvyStop" ivyStop :: IO ()
foreign import ccall "IvyMainLoop" ivyMainLoop :: IO ()

foreign import ccall "IvyInit" 
    ivyInit :: Ptr CChar -> -- application name
               Ptr CChar -> -- ready message
               Ptr a ->
               Ptr a ->
               Ptr a ->
               Ptr a ->
               IO ()
foreign import ccall "IvySendMsg" ivySendMsg :: Ptr CChar -> IO ()

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
 
