module Server
       (
          addStaticDirPolicy
       ,  createMiddleware
       ) where

import qualified Network.Wai                   as W
import qualified Network.Wai.Middleware.Static as W

addStaticDirPolicy :: W.Policy
addStaticDirPolicy = W.addBase "./static"

createMiddleware :: W.Policy -> W.Middleware
createMiddleware = W.staticPolicyWithOptions W.defaultOptions
