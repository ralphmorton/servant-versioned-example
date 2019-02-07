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
        Get '[JSON] Bool
        :<|>
        Versions '["v1"] :> "info" :> Get '[JSON] String
        :<|>
        "add" :> Versions '["v2"] :> Capture "x" Int :> Capture "y" Int :> Post '[JSON] Int
        :<|>
        Post '[JSON] Double
    )

-- Specify a server for the API. The required server type is not affected by
-- the presence of `Versions` combinators.

testServerUnversioned :: Server TestVersionedAPI
testServerUnversioned bar fooHeader =
    pure True
    :<|>
    pure (show (bar, fooHeader))
    :<|>
    (\x y -> pure $ x + y)
    :<|>
    pure 42


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
Response: 200 true

Request: GET http://localhost:8080/v2
Response: 200 true

Request: GET http://localhost:8080/v1/info?bar=barval
Response: 200 (Just "barval",Nothing)

Request: GET http://localhost:8080/v2/info?bar=barval
Response: 404

Request: POST http://localhost:8080/v1/add/1/2
Response: 404

Request: POST http://localhost:8080/v2/add/1/2
Response: 200 3

Request: POST http://localhost:8080/v1
Response: 200 42

Request: POST: http://localhost:8080/v2
Response: 200 42

--}
