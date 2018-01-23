f : Nat -> Int -> Int -> Int
f (S k) m n = f k m n
f Z m n = if m == n then 0 else 1

main : IO ()
main = do
  putStrLn "hello world"
  printLn $ f 4 3 2
