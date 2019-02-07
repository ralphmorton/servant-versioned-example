{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
import Network.Wai.Handler.Warp
import Servant
import Servant.Versioned

main :: IO ()
main = run 8080 $ serve api testServerVersioned
    where api = Proxy :: Proxy (Versioned SupportedVersions :> TestVersionedAPI)

--
--
--

-- Specify supported versions (this can be derived from the API type
-- itself, I just haven't implemented that yet).

type SupportedVersions = '["v1", "v2"]

-- Specify servant API. Inject `Versions` combinators to restrict API
-- sections to particular versions.

type TestVersionedAPI = QueryParam "bar" String :> ConcreteHeader "X-Foo" String :>
    (
        Get '[JSON] String
        :<|>
        Versions '["v1"] :> "info" :> Capture "foo" String :> Get '[JSON] String
        :<|>
        Capture "x" Int :> Capture "y" Int :> Versions '["v2"] :> "add" :> Get '[JSON] Int
        :<|>
        "alt-impl" :>
            (
                Versions '["v1"] :> Post '[JSON] String
                :<|>
                Versions '["v2"] :> Post '[JSON] String
            )
    )

-- Specify a server for the API. The required server type is not affected by
-- the presence of `Versions` combinators.

testServerUnversioned :: Server TestVersionedAPI
testServerUnversioned bar fooHeader =
    (
        pure "hello"
        :<|>
        (\foo -> (pure . show) (bar, fooHeader, foo))
        :<|>
        (\x y -> pure (x + y))
        :<|>
        (
            pure "v1 impl"
            :<|>
            pure "v2 impl"
        )
    )

-- Transform the un-versioned server into a versioned server. A versioned API
-- is a set of alternative APIs (one for each supported version), each prefixed
-- with a segment named for the version, and filterd to only expose endpoints
-- that are not excluded for that version.

testServerVersioned :: Server (Versioned SupportedVersions :> TestVersionedAPI)
testServerVersioned = versionedServer supported api badVersion testServerUnversioned
    where
    supported = Proxy :: Proxy SupportedVersions
    api = Proxy :: Proxy TestVersionedAPI
    badVersion = throwError err404

{--

EXAMPLES:

Request: GET http://localhost:8080/v1
Response: 200 "hello"

Request: GET http://localhost:8080/v2
Response: 200 "hello"

Request: GET http://localhost:8080/v1/info/info-val
Response: 200 "(Nothing,Nothing,\"info-val\")"

Request: GET http://localhost:8080/v2/info/info-val
Response: 404

Request: GET http://localhost:8080/v1/1/2/add
Response: 404

Request: GET http://localhost:8080/v2/1/2/add
Response: 200 3

Request: POST http://localhost:8080/v1/alt-impl
Response: 200 "v1 impl"

Request: POST: http://localhost:8080/v2/alt-impl
Response: 200 "v2 impl"

--}
