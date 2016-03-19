import System.PNotify
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import System.Directory

import qualified MaskTests

readLoop :: IWatchHandle -> INBuffer -> IO ()
readLoop hd buf = do
    events <- readEvents hd buf
    mapM_ print events
    threadDelay 1000000
    readLoop hd buf

testFun :: INBuffer -> IO ()
testFun buf = do
    let mask = unMask (inCreate *|* inDelete *|* inModify *|* inMove)
    fd <- _iNotifyInit
    wd <- iNotifyAddWatch fd "." mask
    readLoop wd buf

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Masks" MaskTests.spec
