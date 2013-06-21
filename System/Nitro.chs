
-- |
-- Module:	System.Nitro
-- License:	BSD3
-- Maintainer:	Erin Dahlgren <edahlgren@bu.mp>
-- Stability:	experimental
-- Portability: non-portable
--
-- Nitro is a fast, secure transport layer for sending messages across TCP and Inproc sockets.  It is ideal for building scalable network applications.
-- Nitro depends on the c libraries nitro and nitronacl (<https://github.com/bumptech/nitro>).
module System.Nitro (
     -- * How to use Nitro sockets
     -- $use

       nitroRuntimeStart
     , NitroSocket
     , SocketOptions(..)
     , defaultOpts
     , bind
     , connect
     , withSocket
     , close

     -- * Distributing messages
     -- $distributed
     , NitroFrame
     , bstrToFrame
     , frameToBstr
     , recv
     , send

     -- * Routing messages
     -- $routing
     , reply

     -- * Proxying messages
     -- $proxying
     , relayFw
     , relayBk

     -- * Pub/Sub messages
     -- $pubsub
     , sub
     , unsub
     , pub

     -- * Advanced
     -- $advanced
     , fileno

     -- * Types
     , Flag(NoWait)
     , NitroError(..)
     ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import qualified Foreign.Concurrent as FC
import Foreign.Marshal.Alloc

import Data.IORef
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Control.Monad (when)
import Control.Exception (bracket)

import System.IO (hPutStrLn, stderr)

-- $use
--
-- > {-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- > import System.Nitro
-- >
-- > main = do
-- >     nitroRuntimeStart
-- >
-- >     server <- bind "tcp://127.0.0.1:7777" defaultOpts
-- >     client <- connect "tcp://127.0.0.1:7777" defaultOpts
-- >
-- >     fr <- bstrToFrame "Hi I'm a client"
-- >     send client fr []
-- >     recv server [] >>= frameToBstr >>= print

-- $distributed
--
-- > {-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- > import System.Nitro
-- > import Control.Concurrent (forkIO, threadDelay)
-- > import Control.Monad (forever)
-- >
-- > main = do
-- >     nitroRuntimeStart
-- >
-- >     server <- bind "tcp://*:7777" defaultOpts
-- >
-- >     let serverWorker = (\i -> forkIO $ forever $ do
-- >                             fr <- recv server []
-- >                             threadDelay 1000000
-- >                             print ("Thread #" ++ (show i))
-- >                             print . frameToBstr $ msg
-- >                         )
-- >
-- >     mapM_ serverWorker [1..2]
-- >
-- >     client <- connect "tcp://127.0.0.1:7777" client
-- >     fr1 <- bstrToFrame "Here's a request"
-- >     send client fr1 []
-- >     fr2 <- bstrToFrame "Here's another request"
-- >     send client fr2 []
-- >
-- >     threadDelay 2000000
--
-- Nitro wraps messages in a transport layer called a NitroFrame.  NitroFrames encode routing information about the sender of a message.  When you receive a NitroFrame you can use it to reply to the original sender.
-- Compile all multithreaded Nitro code with ghc-option: -threaded

-- $routing
--
-- > {-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- > import System.Nitro
-- > import Control.Concurrent (forkIO, threadDelay)
-- > import Control.Monad (forever)
-- >
-- > main = do
-- >     nitroRuntimeStart
-- >
-- >     client1 <- connect "tcp://127.0.0.1:7777" defaultOpts
-- >     client2 <- connect "tcp://127.0.0.1:7777" defaultOpts
-- >
-- >     fr1 <- bstrToFrame "Hi I want a response"
-- >     send client1 fr1 []
-- >     fr2 <- bstrToFrame "Hi I also want a response"
-- >     send client2 fr2 []
-- >
-- >     forkIO $ withSocket (bind "tcp://127.0.0.1:7777" defaultOpts)
-- >                         (\echoServer -> forever $ do
-- >                             frame <- recv echoServer []
-- >                             reply echoServer frame frame []
-- >                         )
-- >
-- >     recv client1 [] >>= frameToBstr >>= print
-- >     recv client2 [] >>= frameToBstr >>= print
--
-- Nitro sockets are threadsafe.  Many worker threads can receive messages on a shared socket without overlap.

-- $proxying
--
-- > {-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- > import System.Nitro
-- > import Data.ByteString as BS
-- > import Control.Concurrent (threadDelay, forkIO)
-- > import Control.Monad (forever, when)
-- >
-- > proxy = withSocket (bind "tcp://127.0.0.1:7777" defaultOpts)
-- >                    (\proxyRecv -> do
-- >                        withSocket (connect "tcp://127.0.0.1:7778" defaultOpts)
-- >                        (\proxySend -> forever $ do
-- >                            frame <- recv proxyRecv []
-- >                            msg <- frameToBstr
-- >                            when (BS.length msg < 50) $
-- >                            relayFw proxySend frame frame []
-- >                        )
-- >                    )
-- >
-- > server = withSocket (bind "tcp://127.0.0.1:7778" defaultOpts)
-- >                     (\server -> forever $ do
-- >                         fr <- recv server []
-- >                         print . frameToBstr $ fr
-- >                     )
-- >
-- > main = do
-- >     nitroRuntimeStart
-- >
-- >     forkIO $ server
-- >     forkIO $ proxy
-- >
-- >     client <- connect "tcp://127.0.0.1:7777" defaultOpts
-- >     fr1 <- bstrToFrame "Here's a short message"
-- >     send client fr1  []
-- >     fr2 <- "This message is too long for our server, it will be blocked"
-- >     send client fr2  []
-- >     threadDelay 1000000

-- $pubsub
--
-- > {-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
-- > import System.Nitro
-- > import Control.Concurrent (threadDelay)
-- >
-- > main = do
-- >    nitroRuntimeStart
-- >
-- >    server <- bind "tcp://127.0.0.1:7777" defaultOpts
-- >    client <- connect "tcp://127.0.0.1:7777" defaultOpts
-- >
-- >    sub client "con"
-- >    threadDelay 1000000
-- >
-- >    fr <- bstrToFrame "You don't understand"
-- >    pub server fr "contender" []
-- >
-- >    recv client [] >>= frameToBstr >>= print

-- $advanced
--
-- Nitro sockets support a NoWait flag, which makes calls to recv nonblocking.  To make this useful, Nitro exposes an Int that represents the file descriptor of a Nitro socket.  Registering an intent to read from this file descriptor using the GHC event manager is one way to know when it is safe to do a nonblocking recv.

#include "nitro.h"
#include "frame.h"
#include "err.h"

-- | A Nitro frame, which contains a message and routing information about the message's sender.
type NitroFrame = ForeignPtr ()

{#pointer *nitro_frame_t as NitroFrameInternal#}

-- | A Nitro socket
{#pointer *nitro_socket_t as NitroSocket#}

{#pointer *nitro_sockopt_t as NitroSockOpt#}

--  void nitro_runtime_start()
-- | Start the Nitro runtime manager.  This function must be called and must return before calling any other Nitro functions.
{#fun nitro_runtime_start as ^
    {} -> `()' #}

--  nitro_frame_t *nitro_frame_new_copy(void *d, uint32_t size)
{#fun nitro_frame_new_copy as ^
    { id `Ptr ()', id `CUInt' } -> `NitroFrameInternal' id #}

--  void *nitro_frame_data(nitro_frame_t *f)
{#fun nitro_frame_data as ^
    { id `NitroFrameInternal' } -> `Ptr ()' id #}

--  uint32_t nitro_frame_size(nitro_frame_t *f)
{#fun nitro_frame_size as ^
    { id `NitroFrameInternal' } -> `CUInt' id #}

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
  { id `NitroFrameInternal', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_recv_ as ^
  { id `NitroSocket', `Int' } -> `NitroFrameInternal' id #}

{#fun nitro_reply_ as ^
  { id `NitroFrameInternal', id `NitroFrameInternal', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_relay_fw_ as ^
  { id `NitroFrameInternal', id `NitroFrameInternal', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_relay_bk_ as ^
  { id `NitroFrameInternal', id `NitroFrameInternal', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_sub_ as ^
  { id `NitroSocket', id `Ptr CUChar', id `CULong' } -> `Int' #}

{#fun nitro_unsub_ as ^
  { id `NitroSocket', id `Ptr CUChar', id `CULong' } -> `Int' #}

{#fun nitro_pub_ as ^
  { id `NitroFrameInternal', id `Ptr CUChar', id `CULong', id `NitroSocket', `Int' } -> `Int' #}

{#fun nitro_eventfd_ as ^
  { id `NitroSocket' } -> `Int' #}

{#fun nitro_frame_destroy_ as ^
  { id `NitroFrameInternal' } -> `()' #}


--flags api
data Flag = NoFlag
     	  | Reuse
     	  | NoWait
	  deriving (Show,Eq,Enum)

toflag :: Flag -> [Flag] -> Int
toflag baseFlag = fromIntegral . foldr ((.|.) . fromEnum) (fromEnum baseFlag)


--error api
{#enum NITRO_ERROR as NitroError {underscoreToCase} deriving (Show, Eq) #}

{#fun nitro_error as ^
  {} -> `Int' #}

{#fun nitro_errmsg as ^
  { `Int' } -> `String' #}

throwNitroError fname e = case e == (fromEnum NitroErrEagain) of
  True -> error $ fname ++ ": " ++ "Nitro Empty"
  False -> do
      hPutStrLn stderr $ fname ++ ": nitro_error code: " ++ (show e)
      msg <- nitroErrmsg e
      error $ fname ++ ": " ++ msg

-- API

data SocketOptions = SocketOptions {
      wantFd :: Bool
  }

-- | Default socket options
--
-- > defaultOpts = SocketOptions {
-- >       wantFd = False
-- > }
defaultOpts = SocketOptions {
      wantFd = False
  }

-- | Set the WantFd flag on a Nitro socket to True or False.  Once the socket is connected or bound, calling fileno on the socket will give an Int representing a valid file descriptor for the socket.
setWantFd :: NitroSockOpt -> Bool -> IO ()
setWantFd opt v =
  nitroSockoptSetWantEventfd opt (if v then (1 :: Int) else (0 :: Int))

-- | Set the high water mark on a Nitro socket.
setHighWaterMark :: NitroSockOpt -> Int -> IO ()
setHighWaterMark opt hwm =
  nitroSockoptSetHwm opt hwm

setSockOpts :: NitroSockOpt -> SocketOptions -> IO ()
setSockOpts opt setopts =
  setWantFd opt (wantFd setopts)

newNitroSockOpt :: SocketOptions -> IO NitroSockOpt
newNitroSockOpt opts = do
  newOpt <- nitroSockoptNew
  when (newOpt == nullPtr) $ do
    e <- nitroError
    throwNitroError "connect" e
  setSockOpts newOpt opts
  return newOpt

-- | Create a Nitro socket bound to a TCP address.
bind :: String -> SocketOptions -> IO NitroSocket
bind location opts = do
  bound <- nitroSocketBind location =<< newNitroSockOpt opts
  when (bound == nullPtr) $ do
    e <- nitroError
    throwNitroError "connect" e
  return bound

-- | Create a Nitro socket connected to a TCP address.
connect :: String -> SocketOptions -> IO NitroSocket
connect location opts = do
  connected <- nitroSocketConnect location =<< newNitroSockOpt opts
  when (connected == nullPtr) $ do
    e <- nitroError
    throwNitroError "connect" e
  return connected

-- | Run an action with a Nitro socket.  The socket is garaunteed to close when the action finishes or when an error occurs.
withSocket :: (IO NitroSocket) -> (NitroSocket -> IO a) -> IO a
withSocket create action = bracket create close action

-- | Close a Nitro socket that is either connected or bound.
close :: NitroSocket -> IO ()
close = nitroSocketClose

-- | Get the Int representation of a Nitro socket's file descriptor.  If wantFd has not been set to True at the creation the Nitro socket, this Int will be meaningless.
--
-- > defaultOpt { wantFd = True }
--
fileno :: NitroSocket -> IO Int
fileno = nitroEventfd

-- | Receive a NitroFrame on a Nitro socket.  The NitroFrame includes routing information about the sender of the bytestring.  The NitroFrame can be given to reply or to the relaying functions in order to route responses back to the sender.
recv :: NitroSocket -> [Flag] -> IO NitroFrame
recv s flags = do
  fr <- nitroRecv s (toflag NoFlag flags)
  fp <- FC.newForeignPtr fr (nitroFrameDestroy fr)
  when (fr == nullPtr) $ do
    e <- nitroError
    throwNitroError "recv" e
  bstr <- frameToBstr fp
  return fp

-- | Convert a NitroFrame to a strict bytestring.
frameToBstr :: NitroFrame -> IO ByteString
frameToBstr fp =
  withForeignPtr fp $ \fr -> do
    data' <- nitroFrameData fr
    size <- nitroFrameSize fr
    fptr <- newForeignPtr_ (castPtr data')
    return $ BS.copy (PS fptr 0 (fromIntegral size))

-- | Send a strict bytestring on a Nitro socket.  Nitro sockets do not set a high water mark by default.
send :: NitroSocket -> NitroFrame -> [Flag] -> IO ()
send s fp flags = do
  withForeignPtr fp $ \fr -> do
    e <- nitroSend fr s (toflag Reuse flags)
    when (e < 0) $ throwNitroError "send" e

-- | Convert a strict bytestring to a NitroFrame.
bstrToFrame :: ByteString -> IO NitroFrame
bstrToFrame (PS ps off size) = do
  fr <- withForeignPtr ps $ \p -> nitroFrameNewCopy (castPtr p `plusPtr` off) (fromIntegral size)
  FC.newForeignPtr fr (nitroFrameDestroy fr)

-- | Reply to the sender of a NitroFrame.  The first NitroFrame is the the sent NitroFrame, and the second NitroFrame is the response.
reply :: NitroSocket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
reply s snd fr flags =
  withForeignPtr snd $ \ptr1 ->
    withForeignPtr fr $ \ptr2 -> do
      e <- nitroReply ptr1 ptr2 s (toflag Reuse flags)
      when (e < 0) $ throwNitroError "reply" e

-- | Forward a NitroFrame to a new destination, passing along the routing information of the original sender.  The first NitroFrame is from the original sender, and the second NitroFrame contains the message to be forwarded.  Useful for building proxies.
relayFw :: NitroSocket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
relayFw s snd fr flags = do
  withForeignPtr snd $ \ptr1 ->
    withForeignPtr fr $ \ptr2 -> do
      e <- nitroRelayFw ptr1 ptr2 s (toflag Reuse flags)
      when (e < 0) $ throwNitroError "relayFw" e

-- | Relay back a NitroFrame by passing along the routing information from a reply.  The first NitroFrame is from the replier, and the second NitroFrame contains the message to be relayed back.  Useful for building proxies.
relayBk :: NitroSocket -> NitroFrame -> NitroFrame -> [Flag] -> IO ()
relayBk s snd fr flags = do
  withForeignPtr snd $ \ptr1 ->
    withForeignPtr fr $ \ptr2 -> do
      e <- nitroRelayBk ptr1 ptr2 s (toflag Reuse flags)
      when (e < 0) $ throwNitroError "relayBk" e

-- | Subscribe a Nitro socket to a channel prefix.  The channel prefix is a strict bytestring.  This socket can then receive messages on any channel containing that prefix.
sub :: NitroSocket -> ByteString -> IO ()
sub s (PS key off size) = do
  e <- withForeignPtr key $ \k -> nitroSub s (castPtr k `plusPtr` off) (fromIntegral size)
  when (e < 0)  $ throwNitroError "sub" e

-- | Unsubscribe a Nitro socket from a channel prefix.  The channel prefix is a strict bytestring.
unsub :: NitroSocket -> ByteString -> IO ()
unsub s (PS key off size) = do
  e <- withForeignPtr key $ \k -> nitroSub s (castPtr k `plusPtr` off) (fromIntegral size)
  when (e < 0) $ throwNitroError "unsub" e

-- | Publish a NitroFrame to a channel on a Nitro socket.  Any sockets connected to the same location can subscribe to updates from this publisher.
pub :: NitroSocket -> NitroFrame -> ByteString -> [Flag] -> IO Int
pub s fp (PS key offk sizek) flags = do
  withForeignPtr fp $ \fr ->
    withForeignPtr key $ \k -> nitroPub fr (castPtr k `plusPtr` offk) (fromIntegral sizek) s (toflag Reuse flags)
