module Examples.QuickCheck where

import Test.QuickCheck
import Data.Char
import Data.List

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs ++ [x]

propRevSmall :: Property
propRevSmall = rev [1,2] === [2,1]

propRevTwice :: [Int] -> Property
propRevTwice xs = rev (rev xs) === xs

propRevMedium :: Property
propRevMedium = conjoin [rev [1,2,2] === [2,2,1],
                         rev [1,2,3] === [3,2,1]]

propRevTwo :: [Int] -> [Int] -> Property
propRevTwo xs ys = rev (xs ++ ys) === rev ys ++ rev xs

-- *Examples.QuickCheck> quickCheck propRevSmall
-- +++ OK, passed 1 test.
-- *Examples.QuickCheck> quickCheck propRevTwice
-- +++ OK, passed 100 tests.
-- *Examples.QuickCheck> quickCheck propRevMedium
-- *** Failed! Falsified (after 1 test):
-- [2,3,1] /= [3,2,1]
-- *Examples.QuickCheck> quickCheck propRevTwo
-- *** Failed! Falsified (after 5 tests and 8 shrinks):
-- [0]
-- [0,1]
-- [0,1,0] /= [1,0,0]

propLast :: [Int] -> Property
propLast xs = last xs === head (reverse xs)

propLastFixed :: NonEmptyList Int -> Property
propLastFixed (NonEmpty xs) = last xs === head (reverse xs)

propCycle :: NonEmptyList Int -> NonNegative Int -> Property
propCycle (NonEmpty xs) (NonNegative n) =
  cycle xs !! n === xs !! (mod n (length xs))

propToUpperChanges :: Char -> Property
propToUpperChanges c = toUpper c =/= c

propToUpperChangesLetter :: Property
propToUpperChangesLetter = forAll (elements ['a'..'z']) propToUpperChanges

listHasZero :: [Int] -> Bool
listHasZero xs = elem 0 xs

-- *Examples.QuickCheck> quickCheck (listHasZero [1,0,2])
-- +++ OK, passed 1 test.
-- *Examples.QuickCheck> quickCheck listHasZero
-- *** Failed! Falsified (after 1 test):
-- []

propSort :: NonEmptyList Int -> Property
propSort (NonEmpty xs) =
  forAll (elements xs) (\x -> elem x (sort xs))

propRevTwo' :: [Int] -> [Int] -> Property
propRevTwo' xs ys =
  let input = xs ++ ys
  in counterexample ("Input: " ++ show input) $
     rev input === rev ys ++ rev xs

someLetters :: Gen String
someLetters = do
  c <- elements "xyzw"
  n <- choose (1,10)
  return (replicate n c)

data Switch = On | Off
  deriving (Show, Eq)

toggle :: Switch -> Switch
toggle On = Off
toggle Off = On

propToggleTwice :: Switch -> Property
propToggleTwice s = s === toggle (toggle s)

instance Arbitrary Switch where
  arbitrary = elements [On,Off]

------------------------------------- 과제 -------------------------------------

-------------- let2int 함수 --------------
-- GPT 설명 명세
{-
#요청사항1
1. 카이사를 암호를 하스켈 언어로 구현한다.
2. let2int라는 함수명으로 함수를 만든다.
2-1. a~z사이의 소문자를 그에 대응하는 0과 25사이의 정수로 바꾸는 함수 : let2int :: Char -> Int
3. 앞으로 오는 모든 경우 소문자만을 암호화한다. 대문자나 구두점은 그대로 둔다.

#요청사항2
아래 제공한 QuickCheck.hs 파일의 소스코드를 보고 let2int 함수의 quickcheck 테스트 케이스를 작성하라.

(QuickCheck.hs 파일 소스코드 전문 제공)
-}

-- 함수
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- quickcheck 테스트 케이스
propLet2Int :: Char -> Property
propLet2Int c = forAll (elements ['a'..'z']) $ \c -> let2int c === ord c - ord 'a'

-- ChatGPT 활용 코딩의 장단점
{-
let2int함수와 이 함수의 테스트 케이스를 만들기 위해서 #요청사항1,2만 제공하고 특별히 다른 것을 하지 않았다.
테스트 케이스의 경우 forAll 함수와 elements 함수를 사용하여 a~z 사이의 모든 문자에 대해 테스트를 수행한다.
생성된 코드를 검증하기에 적절하다.
-}

-------------- int2let 함수 --------------
-- GPT 설명 명세
{-
#요청사항3
1. int2let라는 함수명으로 함수를 만든다.
2.let2int와 반대 역할을 하는 함수가 int2let함수다.
3. int2let 함수의 quickcheck 테스트 케이스도 작성한다.
-}

-- 함수
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- quickcheck 테스트 케이스
propInt2Let :: Int -> Property
propInt2Let n = forAll (elements [0..25]) $ \n -> int2let n === chr (ord 'a' + n)

-- ChatGPT 활용 코딩의 장단점
{-
int2let함수와 이 함수의 테스트 케이스를 만들기 위해서 #요청사항3만 제공하고 특별히 다른 것을 하지 않았다.
테스트 케이스의 경우 forAll 함수와 elements 함수를 사용하여 0~25 사이의 모든 정수에 대해 테스트를 수행한다.
생성된 코드를 검증하기에 적절하다.
-}

-------------- shift 함수 --------------
-- GPT 설명 명세
{-
#요청사항4
1.int2let, let2int 함수를 이용하여 치우침 인자를 소문자에 적용하는 shift함수를 작성한다.
2.글자를 수로 바꾼 뒤 치우침 인자를 더한 값을 26으로 나눈 나머지를 구하고 그 나머지 정수값을 다시 소문자로 바꾼다
3.shift 함수의 quickcheck테스트 케이스도 작성한다.
-}

-- 함수
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- quickcheck 테스트 케이스
propShift :: Int -> Char -> Property
propShift n c = forAll (elements ['a'..'z']) $ \c -> shift n c === int2let ((let2int c + n) `mod` 26)

-- ChatGPT 활용 코딩의 장단점
{-
Shift함수와 이 함수의 테스트 케이스를 만들기 위해서 #요청사항4만 제공하고 특별히 다른 것을 하지 않았다.
테스트 케이스의 경우 forAll을 사용하여 a~z에 대해 shift 함수가 올바르게 치우침을 적용하여 문자를 변환하는지 확인한다.
생성된 코드를 작성하기에 적절하다.
-}

-------------- encode, decode 함수 --------------
-- GPT 설명 명세
{-
#요청사항5
1.shift함수를 이용하여 주어진 치우침 인자에 맞게 줄글을 암호화 한다.
2. 암호화 하는 encode 함수, 복호화 하는 decode함수를 작성한다.
3. 두 함수의 quickcheck테스트 케이스도 작성한다.
-}

-- encode함수
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- decode함수
decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]

-- quickcheck 테스트 케이스(encode)
propEncode :: Int -> String -> Property
propEncode n xs = forAll (listOf (elements ['a'..'z'])) $ \xs -> encode n xs === [shift n x | x <- xs]

-- quickcheck 테스트 케이스(decode)
propDecode :: Int -> String -> Property
propDecode n xs = forAll (listOf (elements ['a'..'z'])) $ \xs -> decode n xs === [shift (-n) x | x <- xs]

-- ChatGPT 활용 코딩의 장단점 + 마무리하며
{-
decode, encode함수와 이 함수의 테스트 케이스를 만들기 위해서 #요청사항5만 제공하고 특별히 다른 것을 하지 않았다.
테스트 케이스의 경우 forAll과 elements를 사용하여 a~z 모든 문자열에 대해 encode, decode 함수가 잘 작동하는지 확인한다.
생성된 코드를 작성하기에 적절하다.

여기에는 작성하지 않았지만 처음에 문제를 풀 때
GPT에 소문자만 변환한다는 조건을 입력하지 않아서 quickcheck 테스트 케이스를 작성할 때 대문자까지 고려하는 경우를 GPT가 코드를 짜줬다.

stack ghci QuickCheck.hs
quickCheck prop_____
위 명령어를 사용하여 각 테스트 케이스들이 잘 검증하는가 파악을 할 때 종종 오류가 발생했었다.

혼란이 발생하여 어디가 문제인가 알아보니 조건을 정확하게 주지 않았다는 것을 파악했고 GPT와 처음부터 다시 대화를 시작했다.
처음부터 다시 시작한 대화가 이 파일에 있는 대화 목록이다.
GPT에 조건을 명확하게 주니까 더 이상 혼란도 발생하지 않고 순식간에 과제를 풀었다.

GPT로 과제를 풀면서 느끼는 것은 GPT의 컴퓨팅 능력도 중요하지만 그만큼 중요한 것 또한 질문자의 질문 능력이라는 것을 느꼈다.
앞으로의 시대에서는 어떤 문제에 대해서 어떻게 질문할 것인가, 지금 닥친 문제에서 내가 진짜로 질문하고 싶은 것은 무엇인가 파악하는 능력이 중요하겠다는 생각을 했는데
간단한 말로 메타인지가 중요한 세상이 되겠다는 생각을 했다.

처음 문제를 풀 때 GPT와 대화한 목록은 혼잡하고 너무 길어서 여기에는 작성이 힘들다고 판단해서 링크를 제공한다.
(처음 문제를 풀 때 대화는 과제의 형식인 각 함수 앞에 GPT명세를 제공하기 힘들어서 여기에 작성하지 않았다.)

https://chat.openai.com/share/4d55fb3f-b3d7-4493-8bdb-d4ee4871ed58

어찌 되었든 문제는 풀었지만 너무 복잡하게 풀어서 다시 풀었다.
-}