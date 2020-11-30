module PwHash exposing (pwhash)

import Crypto.HMAC as HMAC
import Crypto.Hash as Hash
import Word.Bytes as Bytes
import Word.Hex as Hex


pwhash : String -> String -> String -> String
pwhash appSalt email password =
    let
        digest =
            \message key ->
                HMAC.digestBytes HMAC.sha256
                    key
                    (Bytes.fromUTF8 message)

        salt =
            appSalt
                |> Bytes.fromUTF8
                |> digest email
                |> Hex.fromByteList

        hashedPassword =
            salt
                |> Bytes.fromUTF8
                |> digest password
                |> Hex.fromByteList
    in
    hashedPassword
