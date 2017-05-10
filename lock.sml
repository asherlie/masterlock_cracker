fun lock_magic(in_list) = 
	let
		fun find_digits(resistance : int, first_lock : int, second_lock : int) = 
			let
				val first_digit = resistance + 5
				val first_mod_4 = first_digit mod 4

				fun lock_list(n) = 
				let
					fun gen_locks_spots(spot) = 
						case spot of
							0 => (first_lock)
						|	4 => (second_lock) 
						|	_ => (gen_locks_spots(spot-1) + 10)
				in
					case n of
						8 => []
					|	x => gen_locks_spots(x)::lock_list(x+1)
				end
				val lock_pos = lock_list 0

				fun same_mod4() = 
					let
						fun checker num = if num mod 4 = first_mod_4 then true else false
						fun cons_list pos =
							case pos of
								40 => []
							|	x  => if checker x then x::cons_list(x+1) else cons_list(x+1)
					in
						cons_list(0)
					end
				val third_possible = same_mod4()
				fun ad2 n = n + 2
				val second_ops = map ad2 third_possible
					
				fun cross_ref() = 
					let
						fun is_member(el, lst) = 
							case lst of
								[]   => false
							|	x::y => x = el orelse is_member(el, y)
						fun cons_list lst = 
							case lst of
								[]   => []
							|	x::y => if is_member(x, lock_pos) then x::cons_list(y) else cons_list(y)
					in
						cons_list(third_possible)
					end
				val locks_mod4 = cross_ref()

			in
					(* TODO: deal w/ numbers >= 40*)
				(first_digit, second_ops, locks_mod4)
			end

			fun clarify((x, y, z), correct_third) = 
				let
					fun fot(a,b,c) = a
					fun win2(i, j) = i+2 = j orelse i-2 = j
					fun cons_list lst =
						case lst of
							[]   => []
						|	x::y => if win2(x, correct_third) then cons_list(y) else x::cons_list(y)
				in
					(x, cons_list y, [correct_third])
				end
	in
		case in_list of
			(x, y, z, ~1)    => (find_digits(x,y,z))
		|	(x, y, z, q : int) => (clarify(find_digits(x, y, z), q))
	end

