{-# LANGUAGE OverloadedStrings #-}
import IOCP.Windows

import Data.Word
import Foreign

testGUID :: IO ()
testGUID = do
    let guid = GUID 0x01234567 0x89ABCDEF 0xFEDCBA98 0x76543210
        str  = "01234567-89AB-CDEF-FEDC-BA9876543210"
        strB = "{" ++ str ++ "}"
    True <- return $ show guid == strB
    True <- return $ reads str == [(guid, "")]
    True <- return $ reads strB == [(guid, "")]
    True <- return $ reads (str ++ "}") == [(guid, "}")]
    True <- return $ reads ("{" ++ str) == ([] :: [(GUID, String)])
    True <- return $ guid == "01234567-89AB-CDEF-FEDC-BA9876543210"
    True <- return $ guid == "{01234567-89AB-CDEF-FEDC-BA9876543210}"

    True <- return $ sizeOf    guid == 16
    True <- return $ alignment guid == 4
    with guid $ \ptr -> do
        guid' <- peek ptr
        True <- return $ guid' == guid

        -- This assumes the host byte order is little endian.
        bytes <- peekArray 16 (castPtr ptr :: Ptr Word8)
        True <- return $ bytes ==
            [ {- DWORD Data1;    -} 0x67, 0x45, 0x23, 0x01
            , {- WORD  Data2;    -} 0xAB, 0x89
            , {- WORD  Data3;    -} 0xEF, 0xCD
            , {- BYTE  Data4[8]; -} 0xFE, 0xDC, 0xBA, 0x98, 0x76, 0x54, 0x32, 0x10
            ]

        return ()

    return ()

main :: IO ()
main = do
    testGUID
    putStrLn "All tests passed!"
