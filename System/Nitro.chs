{-# LANGUAGE ForeignFunctionInterface #-}
module System.Nitro (
       nitroRuntimeStart
     , Socket
     , withSocket
     , socket
     , setWantFd
     , fileno
     , bind
     , connect
     , recv
     , recvFrame
     , send
     , NitroFrame
     , bstrToFrame
     , reply
     , relayFw
     , relayBk
     , sub
     , unsub
     , pub
     , close
     , Flag(NoWait)
     , NitroError(..)
     ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String

import Data.IORef
import Data.Bits
import Data.ByteString.Internal
import Control.Monad (when)
import Control.Exception (bracket)

#include "nitro.h"
#include "frame.h"
#include "err.h"

{#pointer *nitro_frame_t as NitroFrame#}

{#pointer *nitro_socket_t as NitroSocket#}

{#pointer *nitro_sockopt_t as NitroSockOpt#}

--  void nitro_runtime_start()
{#fun nitro_runtime_start as ^
    {} -> `()' #}

--  nitro_frame_t *nitro_frame_new_copy(void *d, uint32_t size)
{#fun nitro_frame_new_copy as ^
    { id `Ptr ()', id `CUInt' } -> `NitroFrame' id #}

--  void *nitro_frame_data(nitro_frame_t *f)
{#fun nitro_frame_data as ^
    { id `NitroFrame' } -> `Ptr ()' id #}

--  uint32_t nitro_frame_size(nitro_frame_t *f)
{#fun nitro_frame_size as ^
    { id `NitroFrame' } -> `CUInt' id #}

--  nitro_socket_t * nitro_socket_bind(char *location, nitro_sockopt_t *opt)
{#fun nitro_socket_bind as ^
  { `String', id `NitroSockOpt' } -> `NitroSocket' id #}

--  nitro_socket_t * nitro_socket_connect(char *location, nitro_sockopt_t *opt)
{#fun nitro_socket_connect as ^
  { `String', id `NitroSockOpt' } -> `NitroSocket' id #}

--  nitro_sockopt_t *nitro_sockopt_new()
{#fun nitro_sockopt_new as ^
  {} -> `NitroSockOpt' id #}

--  void nitro_sockopt_set_hwm(nitro_sockopt_t *opt, int hwm)
{#fun nitro_sockopt_set_hwm as ^
  { id `NitroSockOpt', `Int' } -> `()' #}

--  void nitro_sockopt_set_want_eventfd(nitro_sockopt_t *opt, int want_eventfd)
{#fun nitro_sockopt_set_want_eventfd as ^
  { id `NitroSockOpt', `Int' } -> `()' #}

{#fun nitro_socket_close as ^
  { id `NitroSocket' } -> `()' #}

#c
int nitro_send_(nitro_frame_t *fr, nitro_socket_t *s, int flags)
{
  int out = nitro_send(&fr, s, flags);
  return (out);
}

nitro_frame_t * nitro_recv_(nitro_socket_t *s, int flags)
{
  nitro_frame_t * out = nitro_recv(s, flags);
  return (out);
}

int nitro_reply_(nitro_frame_t *snd, nitro_frame_t *fr, nitro_socket_t *s, int flags)
{
  int out = nitro_reply(snd, &fr, s, flags);
  return (out);
}

int nitro_relay_fw_(nitro_frame_t *snd, nitro_frame_t *fr, nitro_socket_t *s, int flags)
{
  int out = nitro_relay_fw(snd, &fr, s, flags);
  return (out);
}

int nitro_relay_bk_(nitro_frame_t *snd, nitro_frame_t *fr, nitro_socket_t *s, int flags)
{
  int out = nitro_relay_bk(snd, &fr, s, flags);
  return (out);
}

int nitro_sub_(nitro_socket_t *s, uint8_t *key, size_t length)
{
  int out = nitro_sub(s, key, length);
  return (out);
}

int nitro_unsub_(nitro_socket_t *s, uint8_t *key, size_t length)
{
  int out = nitro_unsub(s, key, length);
  return (out);
}

int nitro_pub_(nitro_frame_t *fr, uint8_t *key, size_t length, nitro_socket_t * s, int flags)
{
  int out = nitro_pub(&fr, key, length, s, flags);
  return (out);
}

int nitro_eventfd_(nitro_socket_t *s)
{
  return (nitro_eventfd(s));
}

void nitro_frame_destroy_(nitro_frame_t *f)
{
  return (nitro_frame_destroy(f));
}
#endc

{#fun nitro_send_ as ^
  { id `NitroFrame', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_recv_ as ^
  { id `NitroSocket', `Int' } -> `NitroFrame' id #}

{#fun nitro_reply_ as ^
  { id `NitroFrame', id `NitroFrame', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_relay_fw_ as ^
  { id `NitroFrame', id `NitroFrame', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_relay_bk_ as ^
  { id `NitroFrame', id `NitroFrame', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_sub_ as ^
  { id `NitroSocket', id `Ptr CUChar', id `CULong' } -> `Int' #}

{#fun nitro_unsub_ as ^
  { id `NitroSocket', id `Ptr CUChar', id `CULong' } -> `Int' #}

{#fun nitro_pub_ as ^
  { id `NitroFrame', id `Ptr CUChar', id `CULong', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_eventfd_ as ^
  { id `NitroSocket' } -> `Int' #}

{#fun nitro_frame_destroy_ as ^
  { id `NitroFrame' } -> `()' #}


--flags api
data Flag = NoFlag
     	  | Reuse
     	  | NoWait
	  deriving (Show,Eq,Enum)

toflag :: [Flag] -> Int
toflag = fromIntegral . foldr ((.|.) . fromEnum) (fromEnum Reuse)


--error api
{#enum NITRO_ERROR as NitroError {underscoreToCase} deriving (Show, Eq) #}

{#fun nitro_error as ^
  {} -> `Int' #}

{#fun nitro_errmsg as ^
  { `Int' } -> `String' #}

throwNitroError fname e = case e == (fromEnum NitroErrEagain) of
  True -> error $ fname ++ ": " ++ "Nitro Empty"
  False -> do
      msg <- nitroErrmsg e
      error $ fname ++ ": " ++ msg


--socket api
type Socket = IORef Socket'

data Socket' = Socket' {
     sock :: Maybe NitroSocket
  ,  opt :: NitroSockOpt
  }

withSocket :: (Socket -> IO a) -> IO a
withSocket action = bracket socket close action

socket :: IO Socket
socket = do
  newOpt <- nitroSockoptNew
  when (newOpt == nullPtr) $ error "socket: sock opt points to null"
  newIORef $ Socket' {
  	   sock = Nothing
    	 , opt = newOpt
	 }

setWantFd :: Socket -> Int -> IO ()
setWantFd s v = do
  s' <- readIORef s
  nitroSockoptSetWantEventfd (opt s') v

setHighWaterMark :: Socket -> Int -> IO ()
setHighWaterMark s hwm = do
  s' <- readIORef s
  nitroSockoptSetHwm (opt s') hwm

fileno :: Socket -> IO (Int)
fileno s = do
  s' <- readIORef s
  maybe (error "fileno: socket not connected nor bound") nitroEventfd $ sock s'

bind :: String -> Socket -> IO ()
bind location s = do
  s' <-  readIORef s
  bound <- nitroSocketBind location $ opt s'
  when (bound == nullPtr) $ error "bind: socket points to null"
  writeIORef s $ s' { sock = Just bound }

connect :: String -> Socket -> IO ()
connect location s = do
  s' <-  readIORef s
  connected <- nitroSocketConnect location $ opt s'
  when (connected == nullPtr) $ error "connect: socket points to null"
  writeIORef s $ s' { sock = Just connected }

recv :: Socket -> [Flag] -> IO (ByteString)
recv s flags = (return . fst) =<< recvFrame s flags

recvFrame :: Socket -> [Flag] -> IO (ByteString, NitroFrame)
recvFrame s flags = recvFrame' . sock =<< readIORef s
  where
    recvFrame' Nothing = error "recv: socket not connected nor bound"
    recvFrame' (Just s') = do
        fr <- nitroRecv s' (toflag flags)
	when (fr == nullPtr) $ do
	    e <- nitroError
	    throwNitroError "recv" e
	bstr <- frameToBstr fr
    	return (bstr, fr)

    frameToBstr fr = do
    	data' <- nitroFrameData fr
    	size <- nitroFrameSize fr
	fptr <- newForeignPtr_ (castPtr data')
    	return $ fromForeignPtr fptr 0 (fromIntegral size)

send :: Socket -> ByteString -> [Flag] -> IO ()
send s (PS ps off size) flags = send' . sock =<< readIORef s
  where
    send' Nothing = error "send: socket not connected nor bound"
    send' (Just s')  = do
        fr <- withForeignPtr ps $ \p -> nitroFrameNewCopy (castPtr p `plusPtr` off) (fromIntegral size)
        e <- nitroSend fr s' (toflag flags)
  	when (e < 0) $ throwNitroError "send" e

bstrToFrame :: ByteString -> IO NitroFrame
bstrToFrame (PS ps off size) = withForeignPtr ps $ \p -> nitroFrameNewCopy (castPtr p `plusPtr` off) (fromIntegral size)

reply :: Socket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
reply s snd fr flags = reply' . sock =<< readIORef s
  where
    reply' Nothing = error "reply: socket not connected nor bound"
    reply' (Just s') = do
	e <- nitroReply snd fr s' (toflag flags)
	when (e < 0) $ throwNitroError "reply" e

relayFw :: Socket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
relayFw s snd fr flags = relayFw' . sock =<< readIORef s
  where
    relayFw' Nothing = error "relayFw: socket not connected nor bound"
    relayFw' (Just s') = do
	e <- nitroRelayFw snd fr s' (toflag flags)
	when (e < 0) $ throwNitroError "relayFw" e

relayBk :: Socket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
relayBk s snd fr flags = relayBk' . sock =<< readIORef s
  where
    relayBk' Nothing = error "relayBk: socket not connected nor bound"
    relayBk' (Just s') = do
	e <- nitroRelayBk snd fr s' (toflag flags)
	when (e < 0) $ throwNitroError "relayBk" e

type Key = ByteString

sub :: Socket -> Key -> IO ()
sub s (PS key off size) = sub' . sock =<< readIORef s
  where
    sub' Nothing = error "sub: socket not connected nor bound"
    sub' (Just s') = do
        e <- withForeignPtr key $ \k -> nitroSub s' (castPtr k `plusPtr` off) (fromIntegral size)
	when (e < 0)  $ throwNitroError "sub" e

unsub :: Socket -> Key -> IO ()
unsub s (PS key off size) = unsub' . sock =<< readIORef s
  where
    unsub' Nothing = error "unsub: socket not connected nor bound"
    unsub' (Just s') = do
        e <- withForeignPtr key $ \k -> nitroSub s' (castPtr k `plusPtr` off) (fromIntegral size)
	when (e < 0)  $ throwNitroError "unsub" e

pub :: Socket -> ByteString -> Key -> [Flag] -> IO Int
pub s (PS ps off size) (PS key offk sizek) flags = pub' . sock =<< readIORef s
  where
    pub' Nothing = error "pub: socket not connected nor bound"
    pub' (Just s') = do
	fr <- withForeignPtr ps $ \p -> nitroFrameNewCopy (castPtr p `plusPtr` off) (fromIntegral size)
	messagesSent <- withForeignPtr key $ \k -> nitroPub fr (castPtr k `plusPtr` offk) (fromIntegral sizek) s' (toflag flags)
	return messagesSent

close :: Socket -> IO ()
close s = do
  s' <- readIORef s
  maybe (error "close: socket not connected nor bound") nitroSocketClose $ sock s'
