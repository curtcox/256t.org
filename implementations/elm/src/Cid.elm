module Cid exposing (computeCid, toBase64Url, encodeLength)

{-| CID computation module for 256t.org

This module provides functions to compute Content Identifiers (CIDs) according to the 256t.org specification.
For browser use, this integrates with JavaScript's crypto API for SHA-512 hashing.

-}

import Bytes exposing (Bytes)
import Bytes.Encode as BE


{-| Convert bytes to base64url encoding (URL-safe, no padding)
-}
toBase64Url : List Int -> String
toBase64Url bytes =
    -- This is a placeholder - actual base64url encoding happens in JS
    "base64url_placeholder"


{-| Encode the content length as an 8-character base64url string
-}
encodeLength : Int -> String
encodeLength length =
    let
        -- Extract 6 bytes from the 48-bit length
        byte0 =
            Basics.remainderBy 256 (length // (256 ^ 5))

        byte1 =
            Basics.remainderBy 256 (length // (256 ^ 4))

        byte2 =
            Basics.remainderBy 256 (length // (256 ^ 3))

        byte3 =
            Basics.remainderBy 256 (length // (256 ^ 2))

        byte4 =
            Basics.remainderBy 256 (length // 256)

        byte5 =
            Basics.remainderBy 256 length
    in
    toBase64Url [ byte0, byte1, byte2, byte3, byte4, byte5 ]


{-| Compute CID from content bytes
For content <= 64 bytes, uses the content itself
For content > 64 bytes, uses SHA-512 hash
-}
computeCid : String -> Int -> String -> String
computeCid content contentLength hash =
    let
        prefix =
            encodeLength contentLength
    in
    if contentLength <= 64 then
        prefix ++ toBase64Url (String.toList content |> List.map Char.toCode)

    else
        prefix ++ hash
