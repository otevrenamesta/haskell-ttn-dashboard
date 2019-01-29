module Gateways where

import Messages
import qualified Data.Map    as M

gatewayMapping = M.fromList [
   ("eui-b827ebffffc6e42c", (MsgNMnM 1,        49.56059767, 16.07257603))
 , ("eui-b827ebffff517cad", (MsgNMnM 2,        49.56448575, 16.07668581))
 , ("eui-b827ebfffedcde55", (MsgNMnM 3,        49.60112994, 16.07348045))
 , ("eui-b827ebfffe114355", (MsgBaseGWIndoor,  49.20410354, 16.59441639))
 , ("eui-b827ebfffec6e5d0", (MsgBaseGWOutdoor, 49.19936100, 16.57974700))
 ]
