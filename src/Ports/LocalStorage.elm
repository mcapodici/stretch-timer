port module Ports.LocalStorage exposing (cache, onChange)

import Json.Encode as E


port cache : String -> Cmd msg


port onChange : (String -> msg) -> Sub msg
