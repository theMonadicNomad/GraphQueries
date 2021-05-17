{-# LINE 1 "src/Database/Daison/FFI.hsc" #-}
module Database.Daison.FFI where

import Foreign
import Foreign.C



data Btree
data BtCursor

openFlags = (262) :: CInt
{-# LINE 12 "src/Database/Daison/FFI.hsc" #-}

sqlite_OK = (0) :: CInt
{-# LINE 14 "src/Database/Daison/FFI.hsc" #-}
sqlite_ABORT_ROLLBACK = (516) :: CInt
{-# LINE 15 "src/Database/Daison/FFI.hsc" #-}

btreeINTKEY  = (1)  :: CInt
{-# LINE 17 "src/Database/Daison/FFI.hsc" #-}
btreeBLOBKEY = (2) :: CInt
{-# LINE 18 "src/Database/Daison/FFI.hsc" #-}

foreign import ccall "sqlite3Btree.h sqlite3BtreeOpen"
  sqlite3BtreeOpen :: CString -> CString -> Ptr (Ptr Btree) -> CInt -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeClose"
  sqlite3BtreeClose :: Ptr Btree -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeCursor"
  sqlite3BtreeCursor :: Ptr Btree -> CInt -> CInt -> CInt -> CInt -> Ptr (Ptr BtCursor) -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeCloseCursor"
  sqlite3BtreeCloseCursor :: Ptr BtCursor -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeBeginTrans"
  sqlite3BtreeBeginTrans :: Ptr Btree -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeCommit"
  sqlite3BtreeCommit :: Ptr Btree -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeRollback"
  sqlite3BtreeRollback :: Ptr Btree -> CInt -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeInsert"
  sqlite3BtreeInsert :: Ptr BtCursor -> Ptr () -> Int64 -> Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeDelete"
  sqlite3BtreeDelete :: Ptr BtCursor -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeFirst"
  sqlite3BtreeFirst :: Ptr BtCursor -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeNext"
  sqlite3BtreeNext :: Ptr BtCursor -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreePrevious"
  sqlite3BtreePrevious :: Ptr BtCursor -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeLast"
  sqlite3BtreeLast :: Ptr BtCursor -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeMoveTo"
  sqlite3BtreeMoveTo :: Ptr BtCursor -> Ptr () -> Int64 -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeDataSize"
  sqlite3BtreeDataSize :: Ptr BtCursor -> Ptr Word32 -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeDataFetch"
  sqlite3BtreeDataFetch :: Ptr BtCursor -> Ptr Word32 -> IO (Ptr ())

foreign import ccall "sqlite3Btree.h sqlite3BtreeData"
  sqlite3BtreeData :: Ptr BtCursor -> Word32 -> Word32 -> Ptr () -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeRecordCompare"
  sqlite3BtreeRecordCompare :: Int64 -> Ptr () -> Ptr () -> Ptr CInt -> IO Int64

foreign import ccall "sqlite3Btree.h sqlite3BtreeCreateTable"
  sqlite3BtreeCreateTable :: Ptr Btree -> Ptr CInt -> CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeDropTable"
  sqlite3BtreeDropTable :: Ptr Btree -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeGetMeta"
  sqlite3BtreeGetMeta :: Ptr Btree -> CInt -> Ptr Word32 -> IO ()

foreign import ccall "sqlite3Btree.h sqlite3BtreeUpdateMeta"
  sqlite3BtreeUpdateMeta :: Ptr Btree -> CInt -> Word32 -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeLockTable"
  sqlite3BtreeLockTable :: Ptr Btree -> CInt -> Word8 -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeKeySize"
  sqlite3BtreeKeySize :: Ptr BtCursor -> Ptr Int64 -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeKey"
  sqlite3BtreeKey :: Ptr BtCursor -> Word32 -> Word32 -> Ptr () -> IO CInt

foreign import ccall "sqlite3Btree.h sqlite3BtreeErrName"
  sqlite3BtreeErrName :: CInt -> IO CString
