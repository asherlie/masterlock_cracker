import System.Environment
find_digits :: (Integer, Integer, Integer) -> ([Integer], [Integer], [Integer])
find_digits(resistance, first_lock, second_lock) =
      let
            first_digit = (\n -> if n >= 40 then n-40 else n)(resistance + 5)
            
            lock_pos :: [Integer]
            lock_pos =
                  let
                        gen_lock_spots :: Integer -> Integer
                        gen_lock_spots spot = {-if spot == 0 then first_lock else if spot == 4 then second_lock else gen_lock_spots(spot-1)+10-}
                              case spot of
                                    0 -> first_lock
                                    4 -> second_lock
                                    _ -> (gen_lock_spots(spot-1)+10)
                  in
                        map gen_lock_spots [0..7]
            third_possible = filter (\n -> mod n 4 == (mod first_digit 4) ) [0..39]
            {- TODO: find case that makes the third options empty -}
            {- 0 even 31 is always empty third. why? -}
                  {-1 1 32 is another case-}
            second_ops = (\lst -> if last lst == 41 then 1:init(lst) else lst) (map(+2) third_possible)
            cross_referenced :: [Integer]
            cross_referenced = filter (\val -> elem val lock_pos) third_possible
      in   {- only need to be careful abt second_ops - first_digit already handled & cr exists in [0..39] -}
           {- only need to be handle 40 and 41, which is handled above bc i only map +2 over [0..39]      -}
            ([first_digit], (\lst -> if last lst == 40 then 0:init(lst) else lst)(second_ops), cross_referenced)

clarify :: (([Integer], [Integer], [Integer]), Integer) -> ([Integer], [Integer], [Integer])
clarify((first_digit, second, _), correct_third) =
      let
            wout_2 = filter(\n -> if n+2 == 40 then (n+2)-40 /= correct_third else if n-2 < 0 then (40+(n-2)) /= correct_third else n+2 /= correct_third && n-2 /= correct_third) second
      in
            (first_digit, wout_2, [correct_third])            
            
pretty_print :: ([Integer], [Integer], [Integer]) -> IO ()
pretty_print(x, y, z) =
      let
            pp(lst) =
                  case lst of
                        []   -> "\nthis should never happen. something has gone horribly wrong (ok, i admit it. this is a bug in the code)"
                        f:[] -> show(f)
                        f:r  -> show(f) ++ ", " ++ pp(r)
      in
            do
                  putStrLn("first: "  ++ pp(x))
                  putStrLn("second: " ++ pp(y))
                  putStrLn("third: "  ++ pp(z))

main :: IO ()
main = 
      do 
            a <- getArgs
            case (map(read::String -> Integer) a) of
                  x:y:z:xs -> if ( ((x >= 40 || y >= 40 || z >= 40 || (case xs of {f:_ -> f >= 40; [] -> False} ) ) || ((odd x && odd y && odd z) || (even x && even y && even z)) ) ) then 
                        putStrLn("something has gone horribly wrong (this is not a bug in the code)") 
                        else 
                              case xs of
                              f:r -> if r == [] then pretty_print(clarify(find_digits(x, y, z), f)) else putStrLn("something has gone horribly wrong (this is not a bug in the code)")
                              [] -> pretty_print(find_digits(x, y, z)) 
                  _        -> putStrLn("something has gone horribly wrong (this is not a bug in the code)") 
