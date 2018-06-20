module CoreAnimation where


type CFTimeInterval = Double

foreign import ccall unsafe "CACurrentMediaTime" currentMediaTime :: IO CFTimeInterval
