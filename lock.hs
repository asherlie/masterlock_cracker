import System.Environment
find_digits :: (Integer, Integer, Integer) -> ([Integer], [Integer], [Integer])
find_digits(resistance, first_lock, second_lock) =
	let
		first_digit = (\n -> if n >= 40 then n-40 else n)(resistance + 5)
		first_mod_4 = mod first_digit 4
		
		lock_pos :: [Integer]
		lock_pos =
			let
				gen_lock_spots :: Integer -> Integer
				gen_lock_spots spot =
					case spot of
						0 -> first_lock
						4 -> second_lock
						_ -> (gen_lock_spots(spot-1)+10)
			in
				map gen_lock_spots [0..7]
		third_possible = filter (\n -> mod n 4 == first_mod_4) [0..39]
		second_ops = map(+2) third_possible
		cross_referenced :: [Integer]
		cross_referenced =
			let
				is_mem(val, lst) =
					case lst of
						[]  -> False
						x:y -> if x == val then True else is_mem(val, y)
				cross_ref(lst_a, lst_b) =
					let
						is_mem_a val = is_mem(val, lst_a)
					in
						filter is_mem_a lst_b
			in
				cross_ref(lock_pos, third_possible)
	in   {- only need to be careful abt second_ops - first_digit already handled & cr exists in [0..39] -}
		([first_digit], (\lst -> if last lst == 40 then 0:init(lst) else lst)(second_ops), cross_referenced)

clarify :: (([Integer], [Integer], [Integer]), Integer) -> ([Integer], [Integer], [Integer])
clarify((first_digit, second, third), correct_third) =
	let
		wout_2 = filter(\n -> n+2 /= correct_third && n-2 /= correct_third) second
	in
		(first_digit, wout_2, [correct_third])		
		
pretty_print(x, y, z) =
	let
		pp(lst) =
			case lst of
				[]   -> "\nsomething has gone horribly wrong"
				x:[] -> show(x)
				x:y  -> show(x) ++ ", " ++ pp(y)
	in
		do
			putStrLn("first: "  ++ pp(x))
			putStrLn("second: " ++ pp(y))
			putStrLn("third: "  ++ pp(z))

main = 
	do 
		a <- getArgs
		if length a == 3 then 
			if (\(x:y:z:xs) -> (x >= 40 || y >= 40 || z >= 40) || (x == y && y == z) || ((odd x && odd y && odd z) || (even x && even y && even z)) ) ((map(read::String->Integer)) a) then 
				putStrLn("something has gone horribly wrong") 
			else
				pretty_print(find_digits((\(x:y:z:xs) -> (x,y,z)) (map(read::String->Integer) a) )) 
		else 
			if length a == 4 then 
				pretty_print(clarify(find_digits((\(x:y:z:xs) -> (x,y,z)) (map(read::String->Integer) a) ), read (last a) :: Integer)) 
			else putStrLn("this program takes only 3 or 4 arguments")
