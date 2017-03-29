module ManaSpec (spec) where

import Test.Hspec
import Mana

spec :: Spec
spec = do
  describe "payMana" $ do
    it "Pay 2U from a pool with 3U" $ do
      payMana (readMana "2U") (readMana "3U") `shouldBe` Just (readMana "1")

    it "Will allow mana of any color to play for anything" $ do
      payMana (readMana "2R") (readMana "1AA") `shouldBe` Just (readMana "")

    it "Will allow colored mana to pay for colorless" $ do
      payMana (readMana "1") (readMana "WW") `shouldBe` Just (readMana "W")
