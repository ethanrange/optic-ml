module Main where
import OpticML ( view, p1, p2, alongside, Lens )

main :: IO ()
main = do
  let x = view p1 (1, 2)
  let y = view p2 (1, 2)

  print x
  print y

  print $ view (p1 `alongside` p2) ((1, 2), (3, 4))
