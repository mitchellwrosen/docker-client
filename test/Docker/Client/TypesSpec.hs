{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fforce-recomp   #-}

module Docker.Client.TypesSpec where

import Docker.Client.Types

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.FileEmbed
import Test.Hspec

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

spec :: Spec
spec = do
  it "Container/FromJSON"       (forM_ containers      (shouldParse @Container))
  it "Port/FromJSON"            (forM_ ports           (shouldParse @Port))
  it "NetworkSettings/FromJSON" (forM_ networkSettings (shouldParse @NetworkSettings))
  it "Network/FromJSON"         (forM_ networks        (shouldParse @Network))
  it "Mount/FromJSON"           (forM_ mounts          (shouldParse @Mount))

 where
  containers      = map snd $(embedDir "test/files/json/containers")
  ports           = map snd $(embedDir "test/files/json/ports")
  networkSettings = map snd $(embedDir "test/files/json/networkSettings")
  networks        = map snd $(embedDir "test/files/json/network")
  mounts          = map snd $(embedDir "test/files/json/mount")

shouldParse :: forall a. (Show a, FromJSON a) => ByteString -> Expectation
shouldParse bytes =
  case eitherDecodeStrict' bytes :: Either String a of
    Left err -> expectationFailure (err ++ ": " ++ pretty bytes)
    _ -> pure ()
 where
  pretty :: ByteString -> String
  pretty = Text.unpack . Text.decodeUtf8
