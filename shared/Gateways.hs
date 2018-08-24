module Gateways where

import Messages
import qualified Data.Map    as M

gatewayMapping = M.fromList [
   ("eui-b827ebffffc6e42c", MsgNMnM 1)
 , ("eui-b827ebffff517cad", MsgNMnM 2)
 , ("eui-b827ebfffe114355", MsgBaseGWIndoor)
 , ("eui-b827ebfffec6e5d0", MsgBaseGWOutdoor)
 ]
