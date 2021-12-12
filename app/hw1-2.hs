{-# OPTIONS_GHC -Wall #-}

{- Функция принимает число и возвращает список чисел.
 - Применяется проверка условий: если число <= 0, возврадается пустой список.
 - Также реализовано базовое условие - список из одной цифры.
 - Реализована обработка нулей через вспомогательную функцию.
 - Иначе нули будут игнорироваться.
 - Рекурсия запускается во всех остальных случаях.
 - Применяются вспомогательные функции для работы с числом как со списком.
 - -}
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | (== 0) x                        = []
  | (< 0) x                         = []
  | (== []) (tail (show x))         = x : []
  | (== '0') (head (tail $ show x)) = (handler . tail $ show x) ++ [headNum x]
  | otherwise                       = toDigitsRev (tailNum x) ++ [headNum x]
  where
    handler zs
      | (== []) (tail zs)           = [read zs :: Integer]
      | (== '0') (head (tail zs))   = handler (tail zs) ++ [0]
      | otherwise                   = toDigitsRev (read (tail zs) :: Integer) ++ [0]

{- Функция принимает список чисел и увеличивает каждое второе в два раза.
 - Возвращает список чисел.
 - -}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []                 = []
doubleEveryOther [x]                = x : []
doubleEveryOther (x:y:xs)           = x : (y * 2) : doubleEveryOther(xs)

{- Принимает список чисел, раскладывает их на составные цифры и возвращает результат их суммы.
 - -}
sumDigits :: [Integer] -> Integer
sumDigits []                        = 0
sumDigits (x:xs)                    = handler x + sumDigits xs
  where 
    handler z
      | (== "") (tail (show z))     = z
      | otherwise                   = (headNum z) + handler (tailNum z)

{- Функция получает число и проверяет остаток от деления на 10.
 - Если остаток == 0, возращатся True.
 - -}
validate :: Integer -> Bool
validate x
  | (== 0) (rem x 10)               = True
  | otherwise                       = False

{- Функция принимает число и возвращает первую цифру.
 - Аналог head для списков. 
 - -}
headNum :: Integer -> Integer
headNum x                           = read [head $ show x] :: Integer

{- Функция принимает число и возвращает его же, но без первой цифры.
 - Аналог tail для списков. 
 - -}
tailNum :: Integer -> Integer
tailNum x                           = read (tail $ show x) :: Integer

{- Программа валидации номера банковской карты.
 - Запускается через передачу имени программы как аргумента
 - например: stack hw1-1.hs
 - После строки приглашения ожидает на вход число (пример: 4012888888881881)
 - Возвращает булево значение. True - номер карты валиден, False - нет.
 - -}
main :: IO ()
main = do 
  print "Enter the number: "
  num <- getLine
  let cardIsValid = validate (sumDigits (doubleEveryOther (toDigitsRev (read num :: Integer))))
  print cardIsValid 
