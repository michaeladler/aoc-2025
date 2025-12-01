import Test.Hspec

import qualified Day01Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day01"     Day01Spec.spec
