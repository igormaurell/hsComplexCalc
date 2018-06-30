plusComplex :: (Num z) => (z, z) -> (z, z) -> (z, z)
plusComplex (a, b) (c, d) = (a + c, b + d)

minusComplex :: (Num z) => (z, z) -> (z, z) -> (z, z)
minusComplex (a, b) (c, d) = (a - c, b - d)

timesComplex :: (Num z) => (z, z) -> (z, z) -> (z, z)
timesComplex (a, b) (c, d) = (a * c + (b * d * (-1)), a * d + b * c)

divideComplex :: (Fractional z) => (z, z) -> (z, z) -> (z, z)
divideComplex (a, b) (c, d) = ((a*c+b*d)/(c*c+d*d), (a*(-d)+(b*c))/(c*c+d*d))

showComplex :: (Num z, Ord z, Show z) => (z, z) -> String
showComplex (a, b) = show a ++ (if b >= 0 then " + " else " - ") ++ show (abs b) ++ "i"

operate op (a, b) (c, d) = case op of "plus"   -> plusComplex (a, b) (c, d)
                                      "minus"  -> minusComplex (a, b) (c, d)
                                      "times"  -> timesComplex (a, b) (c, d)
                                      "divide" -> divideComplex (a, b) (c, d)

processOp op = do putStrLn "1st operand:"
                  n1 <- getLine
                  putStrLn "2nd operand:"
                  n2 <- getLine
                  putStrLn "Result:"
                  putStrLn $ showComplex (operate op (read n1 :: (Float, Float)) (read n2 :: (Float, Float)))

main = do putStrLn "Choose operation:"
          op <- getLine
          case op of "exit" -> putStrLn "Bye!"
                     mathOp -> if mathOp `elem` ["plus", "minus", "times", "divide"]
                                then do processOp mathOp
                                        main
                                else do putStrLn "Invalid operation!"
                                        main