import Lib
import Data

import Test.Hspec

initialGrid = createInitialGrid
main :: IO ()
main = hspec $ do
    describe "searchWordInGrid" $ do
        it "Should find a word horizontal" $ do
            searchWordInGrid "PHP" initialGrid `shouldBe` Just "PHP" 
        it "Should find a word horizontal reverse" $ do
            searchWordInGrid "GOLANG" initialGrid `shouldBe` Just "GOLANG" 
        it "Should find a word vertical" $ do
            searchWordInGrid "HASKELL" initialGrid `shouldBe` Just "HASKELL" 
        it "Should find a word vertical reverse" $ do
            searchWordInGrid "CSHARP" initialGrid `shouldBe` Just "CSHARP" 
        it "Should find a word diagonal" $ do
            searchWordInGrid "RUBY" initialGrid `shouldBe` Just "RUBY" 
        it "Should find a word diagonal reverse" $ do
            searchWordInGrid "JAVA" initialGrid `shouldBe` Just "JAVA"
        it "Should not find a word that is not in the list" $ do
            searchWordInGrid "PEP" initialGrid `shouldBe` Nothing
    -- describe "searchWordInGrid" $ do
    --     it "Should find a word horizontal" $ do
    --         searchWordInGrid "PHP" gridOfWords `shouldBe` True 
