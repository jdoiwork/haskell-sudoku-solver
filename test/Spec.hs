import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    describe "parseNumber" $ do
      it "'1' should be Confirmed 1" $ do
        parseNumber '1' `shouldBe` Confirmed 1
      it "'2' should be Confirmed 2" $ do
        parseNumber '2' `shouldBe` Confirmed 2
      it "'3' should be Confirmed 3" $ do
        parseNumber '3' `shouldBe` Confirmed 3
      it "'4' should be Confirmed 4" $ do
        parseNumber '4' `shouldBe` Confirmed 4
      it "'5' should be Confirmed 5" $ do
        parseNumber '5' `shouldBe` Confirmed 5
      it "'6' should be Confirmed 6" $ do
        parseNumber '6' `shouldBe` Confirmed 6
      it "'7' should be Confirmed 7" $ do
        parseNumber '7' `shouldBe` Confirmed 7
      it "'8' should be Confirmed 8" $ do
        parseNumber '8' `shouldBe` Confirmed 8
      it "'9' should be Confirmed 9" $ do
        parseNumber '9' `shouldBe` Confirmed 9
      it "space should be Candidates [1..9]" $ do
        parseNumber ' ' `shouldBe` Candidates []
      it "otherwise should be Candidates [1..9]" $ do
        parseNumber 'あ' `shouldBe` Candidates []
                                                    
    describe "isConfirmed" $ do
      it "Confirmed x should be True" $ do
        isConfirmed (Confirmed 1) `shouldBe` True

      it "Candidates [x] should be False" $ do
        isConfirmed (Candidates []) `shouldBe` False
  




      