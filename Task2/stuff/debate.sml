fun debate filename = 
   let 
	fun parse file = 
	   let
		val input = TextIO.openIn file
		val col = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		val row = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
  	        val _ = TextIO.inputLine input
		
		val n = Option.valOf  (TextIO.scanStream (Int.scan StringCvt.DEC) input)

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



	fun member _ [] = false
  	  | member (x:int*int) ((h:int*int)::t) = if (#1 x = #1 h) andalso (#2 x = #2 h) then true
                              else member x t


	fun insertRowCell _ _  0  cells = cells
  	  | insertRowCell x y step cells = if (member (x,y) cells) then insertRowCell (x+1) y (step-1) cells
                                           else insertRowCell (x+1) y (step-1) ((x,y)::cells)

	fun insertColCell _ _  0  cells = cells
  	  | insertColCell x y step cells = if (member (x,y) cells) then insertColCell x (y+1) (step-1) cells
                                           else insertColCell x (y+1) (step-1) ((x,y)::cells)

	fun insertDiagCell xrow yrow xcol ycol step cells =
               		insertColCell xcol ycol step (insertRowCell xrow yrow step cells)



	fun (*update (_, _, 0, cells, arrows_list) =*)update (c, r, dist, cells, arrows_list) =
           let
              fun update_aux (_, _, _, cells, acc, [])  = (cells, List.rev acc)
              |   update_aux (c, r, dist, cells, acc, ((x, y, d)::t)) =
                  if d = #"u" then
                      if (y-dist) >= 1 then update_aux (c, r, dist, insertColCell x (y-dist) (dist+1) cells, (x, y-dist, d)::acc, t)
                               else update_aux (c, r, dist, insertColCell x 1 y cells, acc, t)
                  else if d = #"d" then
                      if (y+dist) <= r then update_aux (c, r, dist, insertColCell x y (dist+1) cells, (x, y+dist, d)::acc, t)
                               else update_aux (c, r, dist, insertColCell x y (r-y+1) cells, acc, t)
                  else if d = #"l" then
                      if (x-dist) >= 1 then update_aux (c, r, dist, insertRowCell (x-dist) y (dist+1) cells, (x-dist, y, d)::acc, t)
                               else update_aux (c, r, dist, insertRowCell 1 y x cells, acc, t)
                  else
                      if (x+dist) <= c then update_aux (c, r, dist, insertRowCell x y (dist+1) cells, (x+dist, y, d)::acc, t)
                               else update_aux (c, r, dist, insertRowCell x y (c-x+1) cells, acc, t)
            in
               update_aux (c, r, dist, cells, [], arrows_list)
            end

	
	fun append [] [] = []
	  | append l1 [] = l1
	  | append [] l2 = l2
	  | append (h1::t1) l2 = append t1 (h1::l2) 


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
                                           if d1 = #"r" andalso d2 = #"l" then  real(x2-x1) / 2.0 else 123456789.0
                                       else (* x1 > x2 *)
                                           if d1 = #"l" andalso d2 = #"r" then real(x1-x2) / 2.0 else 123456789.0
                                    (*same column*)
                                    else if x1 = x2 then
                                          if y1 < y2 then
                                               if d1 = #"d" andalso d2 = #"u" then real(y2-y1) / 2.0 else 123456789.0
                                          else (* y1 > y2 *)
                                               if d1 = #"u" andalso d2 = #"d" then real(y1-y2) / 2.0 else 123456789.0
                                    (* Συνθήκη για τα διαγώνια στοιχεία *)
                                    else if (abs (y1-y2)) = (abs (x1-x2)) then
                                          if x1 < x2 then
                                               if y1 < y2 then
                                                   if (d1 = #"d" andalso d2 = #"l") orelse (d1 = #"r" andalso d2 = #"u") then real(abs(y1-y2))
                                                   else 123456789.0
                                               else
                                               	   if (d1 = #"u" andalso d2 = #"l") orelse (d1 = #"r" andalso d2 = #"d") then real(abs(y1-y2))
                                                   else 123456789.0
                                          else (* x1 > x2 *)
                                               if y1 < y2 then
                                                    if (d1 = #"l" andalso d2 = #"u") orelse (d1 = #"d" andalso d2 = #"r") then real(abs(y1-y2))
                                                    else 123456789.0
                                               else
                                                    if (d1 = #"u" andalso d2 = #"r") orelse (d1 = #"l" andalso d2 = #"d") then real(abs(y1-y2))
                                                    else 123456789.0
                                         else 123456789.0
                             in
                                  distance (h, h1)
                             end
                 in
                      if  (dist - min) < ~0.1 then dist2 (j+1, [j], dist, h1, t)
                      else if ( ~0.1 < (dist - min) andalso (dist - min) < 0.1) then dist2 (j+1, (j::pos), min, h1, t)
                      else dist2 (j+1, pos, min, h1, t)
                 end 

	
	fun min_dist a =
	   let
      		fun dist1 (_, l, min, []) = (l, min)
   		|   dist1 (i, l, min, (h::t)) =
                        let
                              val result = dist2 (i+1, [], 123456789.0, h, t)
                              val d = #2 result
                              val pos = #1 result
                        in
                              if (d -min) < ~0.1 then dist1 (i+1, (i::pos), d, t)
			      else if (~0.1 < (d - min) andalso (d - min)< 0.1) then dist1 (i+1,(i::(append pos l)), min, t)
                              else dist1 (i+1, l, min, t)
                        end
	   in
                dist1 (1, [], 123456789.0, a)
	   end

	
	fun memberInt _ [] = false
	  | memberInt (x:int) (h::t) = if x = h then true
				       else memberInt x t
		

	fun exclude l1 l2 =
		let 
		     fun exclude_aux ([], l, _ , acc1, acc2) = (acc1,acc2)
		       | exclude_aux ((h::t), l, n, acc1, acc2) = 
				      if (memberInt n l) then exclude_aux (t, l, (n+1), acc1, (h::acc2))
				      else exclude_aux (t, l, (n+1), (h::acc1), acc2)
	   	in
		     exclude_aux (l1, l2, 1, [], [])
		end

	
	fun repeat _ _ cells [] = List.length cells
	|   repeat col row cells ar_list =
		let
		     val result = min_dist ar_list
		     val distance = Real.floor (#2 result)
		in 
		     if distance = 0 then 
			let
			      val excluded = exclude ar_list (#1 result)
			      val new_cells = #1 (update (col, row, 1, cells, #2 excluded))
			in
				repeat col row new_cells (#1 excluded)
			end
		     else 
			  let
		     	      val update_data = update (col, row, distance, cells, ar_list)
		              val new_cells = #1 update_data
		              val new_ar_list1 = #2 update_data
		              val new_ar_list = #1 (exclude new_ar_list1 (#1 result))
			  in			   
		     	      repeat col row new_cells new_ar_list
			  end
		end

         val data = parse filename
	 val col = #1 (#1 data)
	 val row = #2 (#1 data)
	 val ar_list = #2 data


   in
	repeat col row [] ar_list
	(*min_dist ar_list*)
	(*dist2 (2, [], 123456789, 123456789, (1,5,#"r"), (List.tl ar_list))*)
   end
 

