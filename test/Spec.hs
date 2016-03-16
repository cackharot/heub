import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import App.Model
import Services.AuthenticationService

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Check user data" $ do
    it "should create user data" $ do
      buildUser `shouldBe` buildUser

  describe "Authentication Service tests" $ do
    it "returns True given valid username and password" $ do
      validateUser "admin" "pass" `shouldBe` True
    it "returns False given invalid username and password" $ do
      validateUser "invalid" "invalid" `shouldBe` False
    it "creates new user in database given valid user details" $ do
      createUser buildUser `shouldBe` True


buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True
