module Docker.Client.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict   (HashMap)
import Data.Text             (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics          (Generic)

type ContainerId   = Text
type ContainerName = Text
type EndpointId    = Text
type ImageId       = Text
type ImageName     = Text
type MountName     = Text
type NetworkId     = Text
type NetworkName   = Text

data Container = Container
  { containerId              :: ContainerId
  , containerNames           :: [ContainerName]
  , containerImage           :: ImageName
  , containerImageID         :: ImageId
  , containerCommand         :: Text
  , containerCreated         :: POSIXTime
  , containerState           :: ContainerState
  , containerStatus          :: Text
  , containerPorts           :: [Port]
  , containerLabels          :: HashMap Text Text
  , containerSizeRw          :: Maybe Int
  , containerSizeRootFs      :: Maybe Int
  , containerHostConfig      :: HashMap Text Text
  , containerNetworkSettings :: NetworkSettings
  , containerMounts          :: [Mount]
  } deriving (Eq, Generic, Show)

instance FromJSON Container where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 9 }

data ContainerState
  = Created
  | Restarting
  | Running
  | Paused
  | Exited
  | Dead
  deriving (Eq, Show)

instance FromJSON ContainerState where
  parseJSON = withText "text" (\case
    "created"    -> pure Created
    "restarting" -> pure Restarting
    "running"    -> pure Running
    "paused"     -> pure Paused
    "Exited"     -> pure Exited
    "dead"       -> pure Dead
    _            -> fail "Expected one of: created, restarting, running, paused, exited, dead")

data Port = Port
  { portPrivatePort :: Int
  , portPublicPort  :: Int
  , portType        :: Text -- TODO: ADT
  } deriving (Eq, Generic, Show)

instance FromJSON Port where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 4 }

data NetworkSettings = NetworkSettings
  { networkSettingsNetworks :: HashMap NetworkName Network }
  deriving (Eq, Generic, Show)

instance FromJSON NetworkSettings where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 15 }

data Network = Network
  -- TODO: Add these (what type are they?)
  -- { networkIPAMConfig          :: ()
  -- , networkLinks               :: ()
  -- , networkAliases             :: ()
  { networkNetworkID           :: NetworkId
  , networkEndpointID          :: EndpointId
  , networkGateway             :: Text
  , networkIPAddress           :: Text
  , networkIPPrefixLen         :: Int
  , networkIPv6Gateway         :: Text
  , networkGlobalIPv6Address   :: Text
  , networkGlobalIPv6PrefixLen :: Int
  , networkMacAddress          :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Network where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 7 }

data Mount = Mount
  { mountName        :: MountName
  , mountSource      :: Text
  , mountDestination :: Text
  , mountDriver      :: Text
  , mountMode        :: Text
  , mountRW          :: Bool
  , mountPropagation :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Mount where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 5 }
