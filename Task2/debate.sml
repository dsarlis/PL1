fun debate filename = 
   let 
	(*reads data from file*)
	fun parse file = 
	   let
		val input = TextIO.openIn file
		val col = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		val row = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
  	        val _ = TextIO.inputLine input
		
		val n = Option.valOf  (TextIO.scanStream (Int.scan StringCvt.DEC) input)

		(*creates a list of xCoordinate*yCoordinate*direction for each arrow*)
		fun read 0 acc = List.rev acc
		|   read k acc = 
     		   let
			val _ = TextIO.inputLine input
 			val a = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
			val b = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
			val _ = Option.valOf (TextIO.scanStream (Char.scan) input)
			val c = Option.valOf (TextIO.scanStream (Char.scan) input) 
			
		   in
			 read (k-1) ((a,b,c)::acc)
		   end
	   in
 		((col, row, n), (read n []))
	   end 	

	(*returns true if an element is member of an int*int list*)
	fun member _ [] = false
  	  | member (x:int*int) ((h:int*int)::t) = if (#1 x = #1 h) andalso (#2 x = #2 h) then true
                              else member x t

	(*marks as true the elements of the BitArray that the arrow coveres when it moves along a row*)
	fun insertRowCell _ _ _ _ 0 = ()
  	  | insertRowCell x y col visited step = 
		let
		    val _ = BitArray.update (visited, ((y-1)*col+x-1), true)
		in
		    insertRowCell (x+1) y col visited (step-1)
		end

	(*marks as true the elements of the BitArray that the arrow coveres when it moves along a column*)
	fun insertColCell _ _ _ _ 0 = ()
  	  | insertColCell x y col visited step = 
		let
		    val _ = BitArray.update (visited, ((y-1)*col+x-1), true)
		in
                    insertColCell x (y+1) col visited (step-1)
		end
		
	(*updates the coordinates of the arrow acording to its direction and the distance that it covers
	and the BitArray that represents the whole matrix using insertRowCell and inserColCell*)
	fun update (c, r, dist, visited, arrows_list) =
           let
              fun update_aux (_, _, _, visited, acc, [])  = acc
              |   update_aux (c, r, dist, visited, acc, ((x, y, d)::t)) =
                  if d = #"u" then
                      if (y-dist) >= 1 then (*stays into matrix' bountaries*) 
			  let 
			     val _ = insertColCell x (y-dist) c visited (dist+1)  
			  in 
			     update_aux (c, r, dist, visited, (x, y-dist, d)::acc, t) 
			  end
                      else 		    (*exceeds matrix' bountaries'*)
			  let 
			     val _ = insertColCell x 1 c visited y 
			  in
			     update_aux (c, r, dist, visited, acc, t)
			  end
                  else if d = #"d" then
                      if (y+dist) <= r then 
		       	  let
		       	     val _ = insertColCell x y c visited (dist+1) 
		       	  in
		       	     update_aux (c, r, dist, visited, (x, y+dist, d)::acc, t)
		       	  end
                      else 
			  let 
			     val _ =  insertColCell x y c visited (r-y+1)
			  in 
			     update_aux (c, r, dist, visited, acc, t)
			  end
                  else if d = #"l" then
                      if (x-dist) >= 1 then 
			  let 
			     val _ = insertRowCell (x-dist) y c visited (dist+1) 
			  in 
			     update_aux (c, r, dist, visited, (x-dist, y, d)::acc, t)
			  end
                      else 
			  let 
			     val _ = insertRowCell 1 y c visited x 
			  in 
			     update_aux (c, r, dist, visited, acc, t)
			  end
                  else
                      if (x+dist) <= c then 
			  let 
			     val _ = insertRowCell x y c visited (dist+1)
			  in 
			     update_aux (c, r, dist, visited, (x+dist, y, d)::acc, t)
			  end
                      else 
			   let
			      val _ = insertRowCell x y c visited (c-x+1)
			   in 
			      update_aux (c, r, dist, visited, acc, t)
			   end
            in	
		update_aux (c, r, dist, visited, [], arrows_list)
	    end
	
	fun append [] [] = []
	  | append l1 [] = l1
	  | append [] l2 = l2
	  | append (h1::t1) l2 = append t1 (h1::l2) 

	(*auxirialy function for min_dist function below*)
        fun dist2 (j, pos, min, _, []) = (pos, min)
          | dist2 (j, pos, min, h1, (h::t)) =
                let
                     val dist =
                           let
                           (*returns the cells between the arrows that crush or -1 if the arrows don't crush*)
                                fun distance ((x1,y1,d1),(x2,y2,d2)) =
                                    (*same row*)
                                    if y1 = y2 then
                                       if x1 < x2 then
                                           if d1 = #"r" andalso d2 = #"l" then (x2-x1-1) else 123456789
                                       else (* x1 > x2 *)
                                           if d1 = #"l" andalso d2 = #"r" then (x1-x2-1) else 123456789
                                    (*same column*)
                                    else if x1 = x2 then
                                          if y1 < y2 then
                                               if d1 = #"d" andalso d2 = #"u" then (y2-y1-1) else 123456789
                                          else (* y1 > y2 *)
                                               if d1 = #"u" andalso d2 = #"d" then (y1-y2-1) else 123456789
                                    (* Diagonia stoixeia *)
                                    else if (abs (y1-y2)) = (abs (x1-x2)) then
                                          if x1 < x2 then
                                               if y1 < y2 then
                                                   if (d1 = #"d" andalso d2 = #"l") orelse (d1 = #"r" andalso d2 = #"u") then (2*abs(y1-y2)-1)
                                                   else 123456789
                                               else
                                               	   if (d1 = #"u" andalso d2 = #"l") orelse (d1 = #"r" andalso d2 = #"d") then (2*abs(y1-y2)-1)
                                                   else 123456789
                                          else (* x1 > x2 *)
                                               if y1 < y2 then
                                                    if (d1 = #"l" andalso d2 = #"u") orelse (d1 = #"d" andalso d2 = #"r") then (2*abs(y1-y2)-1)
                                                    else 123456789
                                               else
                                                    if (d1 = #"u" andalso d2 = #"r") orelse (d1 = #"l" andalso d2 = #"d") then (2*abs(y1-y2)-1)
                                                    else 123456789
                                         else 123456789
                             in
                                  distance (h, h1)
                             end
                 in
                      if  dist < min  then dist2 (j+1, [j], dist, h1, t)
                      else if dist = min then dist2 (j+1, (j::pos), min, h1, t)
                      else dist2 (j+1, pos, min, h1, t)
                 end 

	(*returns the minimum distance between two arrows and a list of the arrows are this minimum distance far from each other*) 
	fun min_dist a =
	   let
      		fun dist1 (_, l, min, []) = (l, min)
   		|   dist1 (i, l, min, (h::t)) =
                        let
                              val result = dist2 (i+1, [], 123456789, h, t)
                              val d = #2 result
                              val pos = #1 result
                        in
                              if d < min then dist1 (i+1, (i::pos), d, t)
			      else if d = min then dist1 (i+1,(i::(append pos l)), min, t)
                              else dist1 (i+1, l, min, t)
                        end
	   in
                dist1 (0, [], 123456789, a)
	   end

	(*returns true if an element is member of an int list*)
	fun memberInt _ [] = false
	  | memberInt (x:int) (h::t) = if x = h then true
				       else memberInt x t
		
	(*takes tow list, l1 of arrows and l2 that contains the potitions of the l1 elements that must be excluded
	 and returns l1 without the elements that are excluded and also a list of the excluded elements*)
	fun exclude l1 l2 =
		let 
		     fun exclude_aux ([], l, _ , acc1, acc2) = (acc1,acc2)
		       | exclude_aux ((h::t), l, n, acc1, acc2) = 
				      if (memberInt n l) then exclude_aux (t, l, (n+1), acc1, (h::acc2))
				      else exclude_aux (t, l, (n+1), (h::acc1), acc2)
	   	in
		     exclude_aux (l1, l2, 0, [], [])
		end

	(*main function that is repeated until the list of arrows is empty, that is either the arrows have cushed or
	they have exceeded the matrix' bountaries'*)
	fun repeat _ _ visited [] = List.length (BitArray.getBits visited) (*the sum of the visited elements of the matrix*)
	|   repeat col row visited ar_list =
		let
		     val result = min_dist ar_list
		     val distance = ((#2 result)+1) div 2 (*the distance that each of the arrows covers*)
		in 
		     if distance = 0 then (*the arrows will crush in 0.5 seconds*)
			let
			      val excluded = exclude ar_list (#1 result)
			      val _ = update (col, row, 1, visited, #2 excluded)
			in
			      repeat col row visited (#1 excluded)
			end
		     else 
			  let
                              val excluded = exclude ar_list (#1 result)
                              val _ = update (col, row, distance, visited, #2 excluded)

		     	      val new_ar_list = update (col, row, distance, visited, (#1 excluded))
        		  in			   
		     	      repeat col row visited new_ar_list
			  end
		end

         val data = parse filename
	 val col = #1 (#1 data)
	 val row = #2 (#1 data)
	 val ar_list = #2 data
	 val visited = BitArray.array (col*row, false)

   in
	repeat col row visited ar_list
   end

