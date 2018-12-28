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
  
    describe "updateCell" $ do
      context "case Confirmed A and Confirmed B" $ do
        it "should be Confirmed B" $ do
          updateCell (Confirmed 1) (Confirmed 2) `shouldBe` (Confirmed 2)
      context "case Candidates X and Confirmed B" $ do
        it "should be Confirmed B" $ do
          updateCell (Candidates []) (Confirmed 2) `shouldBe` (Confirmed 2)
      context "case Confirmed A and Candidates X" $ do
        it "should be Confirmed A" $ do
          updateCell (Confirmed 1) (Candidates [1]) `shouldBe` (Confirmed 1)
      context "case Candidates [x,y] and Candidates [x]" $ do
        it "should be Candidates [y]" $ do
          updateCell (Candidates [1,2]) (Candidates [1]) `shouldBe` (Candidates [2])
      context "case Candidates [x,y] and Candidates [z]" $ do
        it "should be Candidates [x,y]" $ do
          updateCell (Candidates [1,2]) (Candidates [3]) `shouldBe` (Candidates [1,2])
      context "case Candidates [] and Candidates [x]" $ do
        it "should be Candidates []" $ do
          updateCell (Candidates []) (Candidates [1]) `shouldBe` (Candidates [])
                
    describe "candiatesSec" $ do
      context "case ((1,1), Confirmed 1)" $ do
        it "should be section1 positions and all cells are Candiates [1]" $ do
          candiatesSec ((1,1), Confirmed 1) `shouldBe`
            [ ((1,1), Candidates [1]), ((1,2), Candidates [1]), ((1,3), Candidates [1])
            , ((2,1), Candidates [1]), ((2,2), Candidates [1]), ((2,3), Candidates [1])
            , ((3,1), Candidates [1]), ((3,2), Candidates [1]), ((3,3), Candidates [1])
            ]
      context "case ((5,5), Confirmed 7)" $ do
        it "should be section5 positions and all cells are Candiates [7]" $ do
          candiatesSec ((5,5), Confirmed 7) `shouldBe`
            [ ((4,4), Candidates [7]), ((4,5), Candidates [7]), ((4,6), Candidates [7])
            , ((5,4), Candidates [7]), ((5,5), Candidates [7]), ((5,6), Candidates [7])
            , ((6,4), Candidates [7]), ((6,5), Candidates [7]), ((6,6), Candidates [7])
            ]
      context "case ((7,9), Confirmed 8)" $ do
        it "should be section9 positions and all cells are Candiates [8]" $ do
          candiatesSec ((7,9), Confirmed 8) `shouldBe`
            [ ((7,7), Candidates [8]), ((7,8), Candidates [8]), ((7,9), Candidates [8])
            , ((8,7), Candidates [8]), ((8,8), Candidates [8]), ((8,9), Candidates [8])
            , ((9,7), Candidates [8]), ((9,8), Candidates [8]), ((9,9), Candidates [8])
            ]
            


      