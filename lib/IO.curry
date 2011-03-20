-----------------------------------------------------------------------------
--- Library for IO operations like reading and writing files
--- that are not already contained in the prelude.
---
--- @author Michael Hanus, Bernd Brassel
--- @version June 2006
-----------------------------------------------------------------------------

module IO(Handle,IOMode(..),SeekMode(..),stdin,stdout,stderr,
          openFile,hClose,hFlush,hIsEOF,isEOF,
          hSeek,hWaitForInput,hWaitForInputs,
          hWaitForInputOrMsg,hWaitForInputsOrMsg,hReady,
          hGetChar,hGetLine,hGetContents,getContents,
          hPutChar,hPutStr,hPutStrLn,hPrint,
          hIsReadable,hIsWritable) where

--- The abstract type of a handle for a stream.
data Handle -- internally defined

--- The modes for opening a file.
data IOMode = ReadMode | WriteMode | AppendMode

--- The modes for positioning with <code>hSeek</code> in a file.
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd


--- Standard input stream.
stdin :: Handle
stdin external

--- Standard output stream.
stdout :: Handle
stdout external

--- Standard error stream.
stderr :: Handle
stderr external

--- Opens a file in specified mode and returns a handle to it.
openFile :: String -> IOMode -> IO Handle
openFile filename mode = (prim_openFile $## filename) $# mode

prim_openFile :: String -> IOMode -> IO Handle
prim_openFile external

--- Closes a file handle and flushes the buffer in case of output file.
hClose :: Handle -> IO ()
hClose h = prim_hClose $# h

prim_hClose :: Handle -> IO ()
prim_hClose external

--- Flushes the buffer associated to handle in case of output file.
hFlush :: Handle -> IO ()
hFlush h = prim_hFlush $# h

prim_hFlush :: Handle -> IO ()
prim_hFlush external

--- Is handle at end of file?
hIsEOF :: Handle -> IO Bool
hIsEOF h = prim_hIsEOF $# h

prim_hIsEOF :: Handle -> IO Bool
prim_hIsEOF external

--- Is standard input at end of file?
isEOF :: IO Bool
isEOF = hIsEOF stdin


--- Set the position of a handle to a seekable stream (e.g., a file).
--- If the second argument is <code>AbsoluteSeek</code>,
--- <code>SeekFromEnd</code>, or <code>RelativeSeek</code>,
--- the position is set relative to the beginning of the file,
--- to the end of the file, or to the current position, respectively.
hSeek :: Handle -> SeekMode -> Int -> IO ()
hSeek h sm pos = ((prim_hSeek $# h) $# sm) $# pos

prim_hSeek :: Handle -> SeekMode -> Int -> IO ()
prim_hSeek external


--- Waits until input is available on the given handle.
--- If no input is available within t milliseconds, it returns False,
--- otherwise it returns True.
--- @param handle - a handle for an input stream
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput handle timeout = (prim_hWaitForInput $# handle)  $## timeout

prim_hWaitForInput :: Handle -> Int -> IO Bool
prim_hWaitForInput external

--- Waits until input is available on some of the given handles.
--- If no input is available within t milliseconds, it returns -1,
--- otherwise it returns the index of the corresponding handle with the available
--- data.
--- @param handles - a list of handles for input streams
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
--- @return -1 if no input is available within the time out, otherwise i
---         if (handles!!i) has data available
hWaitForInputs :: [Handle] -> Int -> IO Int
hWaitForInputs handles timeout = (prim_hWaitForInputs $## handles) $## timeout

prim_hWaitForInputs :: [Handle] -> Int -> IO Int
prim_hWaitForInputs external


--- Waits until input is available on a given handles or a message
--- in the message stream. Usually, the message stream comes from an external port.
--- Thus, this operation implements a committed choice over receiving input
--- from an IO handle or an external port.
---
--- <EM>Note that the implementation of this operation works only with
--- Sicstus-Prolog 3.8.5 or higher (due to a bug in previous versions
--- of Sicstus-Prolog).</EM>
---
--- @param handle - a handle for an input stream
--- @param msgs   - a stream of messages received via an external port (see Ports)
--- @return (Left handle) if the handle has some data available
---         (Right msgs) if the stream msgs is instantiated
---                      with at least one new message at the head

hWaitForInputOrMsg :: Handle -> [msg] -> IO (Either Handle [msg])
hWaitForInputOrMsg handle msgs = do
  input <- hWaitForInputsOrMsg [handle] msgs
  return $ either (\_ -> Left handle) Right input

--- Waits until input is available on some of the given handles or a message
--- in the message stream. Usually, the message stream comes from an external port.
--- Thus, this operation implements a committed choice over receiving input
--- from IO handles or an external port.
---
--- <EM>Note that the implementation of this operation works only with
--- Sicstus-Prolog 3.8.5 or higher (due to a bug in previous versions
--- of Sicstus-Prolog).</EM>
---
--- @param handles - a list of handles for input streams
--- @param msgs    - a stream of messages received via an external port (see Ports)
--- @return (Left i) if (handles!!i) has some data available
---         (Right msgs) if the stream msgs is instantiated
---                      with at least one new message at the head

hWaitForInputsOrMsg :: [Handle] -> [msg] -> IO (Either Int [msg])
hWaitForInputsOrMsg handles msgs =
  seq (normalForm (map ensureNotFree (ensureSpine handles)))
      (prim_hWaitForInputsOrMsg handles msgs)

prim_hWaitForInputsOrMsg :: [Handle] -> [msg] -> IO (Either Int [msg])
prim_hWaitForInputsOrMsg external



--- Checks whether an input is available on a given handle.
hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0


--- Reads a character from an input handle and returns it.
hGetChar    :: Handle -> IO Char
hGetChar h = prim_hGetChar $# h

prim_hGetChar :: Handle -> IO Char
prim_hGetChar external

--- Reads a line from an input handle and returns it.
hGetLine  :: Handle -> IO String
hGetLine h = do c <- hGetChar h
                if c=='\n' then return ""
                           else do cs <- hGetLine h
                                   return (c:cs)

--- Reads the complete contents from an input handle and closes the input handle
--- before returning the contents.
hGetContents  :: Handle -> IO String
hGetContents h = do
  eof <- hIsEOF h
  if eof then hClose h >> return ""
         else do c <- hGetChar h
                 cs <- hGetContents h
                 return (c:cs)

--- Reads the complete contents from the standard input stream until EOF.
getContents  :: IO String
getContents = hGetContents stdin

--- Puts a character to an output handle.
hPutChar    :: Handle -> Char -> IO ()
hPutChar h c = (prim_hPutChar $# h)  $# c

prim_hPutChar :: Handle -> Char -> IO ()
prim_hPutChar external

--- Puts a string to an output handle.
hPutStr :: Handle -> String -> IO ()
hPutStr _ []     = done
hPutStr h (c:cs) = hPutChar h c >> hPutStr h cs

--- Puts a string with a newline to an output handle.
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

--- Converts a term into a string and puts it to an output handle.
hPrint :: Handle -> _ -> IO ()
hPrint h = hPutStrLn h . show


--- Is the handle readable?
hIsReadable :: Handle -> IO Bool
hIsReadable  h = prim_hIsReadable  $# h

prim_hIsReadable :: Handle -> IO Bool
prim_hIsReadable external

--- Is the handle writable?
hIsWritable :: Handle -> IO Bool
hIsWritable h = prim_hIsWritable  $# h

prim_hIsWritable :: Handle -> IO Bool
prim_hIsWritable external


