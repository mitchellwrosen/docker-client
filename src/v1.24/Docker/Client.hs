module Docker.Client
  ( localDockerDaemon
  , ContainersConfig
  , containers
  , containersWith
  -- * Re-exports
  , runClientM
  ) where

import Docker.Client.Types

import Control.Exception.Safe
import Data.Default
import Data.Proxy
import Labels
import Network.HTTP.Client
  (defaultManagerSettings, managerRawConnection, newManager)
import Network.HTTP.Client.Internal (makeConnection)
import Network.Socket               hiding (recv)
import Network.Socket.ByteString    (recv, sendAll)
import Servant.API
import Servant.Client

type DockerAPI = GetContainersJson

type GetContainersJson
  =  "v1.24" :> "containers" :> "json"
  :> QueryParam "all" Bool
  :> QueryParam "limit" Int
  :> QueryParam "since" ContainerId
  :> QueryParam "before" ContainerId
  :> QueryParam "size" Bool
  :> Get '[JSON] [Container]
-- TODO: filters query param

getContainersJson
  :: Maybe Bool -> Maybe Int -> Maybe ContainerId -> Maybe ContainerId
  -> Maybe Bool -> ClientM [Container]

getContainersJson = client (Proxy :: Proxy DockerAPI)


newtype ContainersConfig = ContainersConfig
  ( "all"    := Bool
  , "limit"  := Maybe Int
  , "since"  := Maybe ContainerId
  , "before" := Maybe ContainerId
  , "size"   := Bool
  ) deriving (Has "all" Bool, Has "limit" (Maybe Int),
              Has "since" (Maybe ContainerId), Has "before" (Maybe ContainerId),
              Has "size" Bool)

instance Default ContainersConfig where
  def = ContainersConfig
    ( #all    := False
    , #limit  := Nothing
    , #since  := Nothing
    , #before := Nothing
    , #size   := False
    )

containers :: ClientM [Container]
containers = containersWith def

containersWith :: ContainersConfig -> ClientM [Container]
containersWith (ContainersConfig config) =
  getContainersJson (Just all) limit since before (Just size)
 where
  (_ := all, _ := limit, _ := since, _ := before, _ := size) = config


--------------------------------------------------------------------------------

localDockerDaemon :: FilePath -> IO ClientEnv
localDockerDaemon path = do
  mgr <- newManager defaultManagerSettings
    { managerRawConnection = pure (\_ _ _ ->
        bracketOnError
          (socket AF_UNIX Stream defaultProtocol)
          close
          (\sock -> do
            connect sock (SockAddrUnix path)
            makeConnection (recv sock 8192) (sendAll sock) (close sock)))
    }
  pure (ClientEnv mgr (BaseUrl Http "localhost" 80 ""))
