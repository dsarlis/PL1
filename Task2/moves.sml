fun moves filename = 
	let
		(*reads from file and creates a vector*)
		fun parse file = 
			let
	     			val input = TextIO.openIn file
	     			val voters = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
  	     			val _ = TextIO.inputLine input

	     			fun readChar 0 acc = List.rev acc
	       			  | readChar n acc = 
					let	
			     		     val nextChar = Option.valOf (TextIO.scanStream (Char.scan) input)
					in
			                     readChar (n-1) (nextChar::acc)
					end
			in
				(Vector.fromList (readChar voters []),voters)
			end


		(*returns true if the vector's elements are properly sorted. There are three "states"
		and state 2 is the "final state"*)		
		fun isWinning vect  ~1 2  = true
  	          | isWinning vect pos 0  = if (Vector.sub (vect,pos)) = #"b" then isWinning vect (pos-1) 0
			    	            else if (Vector.sub (vect,pos)) = #"e" then isWinning vect (pos-1) 1
			                    else isWinning vect (pos-1) 2
  		  | isWinning vect pos 1  = if (Vector.sub (vect,pos)) = #"e" then isWinning vect (pos-1) 1
			    		    else if (Vector.sub (vect,pos)) = #"b" then false
			                    else isWinning vect (pos-1) 2
  		  | isWinning vect pos 2  = if (Vector.sub (vect,pos)) = #"w" then isWinning vect (pos-1) 2
			    		    else false
  		  | isWinning _ _ _ = raise Match


		fun equals _ _ ~1 = true
 		  | equals (x:char vector) h n = if Vector.sub(x,n) = Vector.sub(h,n) then equals x h (n-1) 
		   		 		 else false


		fun member _ _ [] = false
  		  | member x voters (h::t) = if equals x h (voters-1) then true else member x voters t


		(* returns true if the vector's elements form a string that cannot lead to problem's solution.
		There are three "states" where state three is the "final state" *)
		fun exclude   _  0 3 #"b" = true
  		  | exclude   _  0 _  _   = false
  		  | exclude vect n 0 #"w" = exclude vect (n-1) 1 (Vector.sub (vect,n-1))
  		  | exclude vect n 0 #"b" = exclude vect (n-1) 0 (Vector.sub (vect,n-1))
  		  | exclude vect n 0 #"e" = exclude vect (n-1) 0 (Vector.sub (vect,n-1))
  		  | exclude vect n 1 #"w" = exclude vect (n-1) 2 (Vector.sub (vect,n-1))
  		  | exclude vect n 1  _   = exclude vect (n-1) 0 (Vector.sub (vect,n-1))
  	          | exclude vect n 2 #"w" = exclude vect (n-1) 2 (Vector.sub (vect,n-1))
  		  | exclude vect n 2 #"b" = exclude vect (n-1) 3 (Vector.sub (vect,n-1))
  		  | exclude vect n 2 #"e" = exclude vect (n-1) 0 (Vector.sub (vect,n-1))
  		  | exclude vect n 3 #"w" = exclude vect (n-1) 2 (Vector.sub (vect,n-1))
  		  | exclude vect n 3 #"e" = exclude vect (n-1) 0 (Vector.sub (vect,n-1))
  		  | exclude vect n 3 #"b" = true
  		  | exclude vect _ _  _   = raise Match


		(* Produces all the next possible states and pushes them into a queue *)
		fun state_gen (array, voters, queue)=
     			let
			    fun gen_aux (_, queue, ~1, _) = queue
			      | gen_aux (arr, queue, i, length) =
	    			   let
					fun swap (ar,i,j) =
     		    			      let
        					    val a = Vector.sub (ar,i) 
        					    val b = Vector.sub (ar,j)
        					    val v = Vector.update (ar,i,b)
     		    			      in
        					    Vector.update (v, j, a)
     		    			      end
					
					fun shift (ar, i, queue, length) =
						if Vector.sub (ar, i) = #"b" then 
                             			     if i = length-1 then ar
			     			     else if Vector.sub (ar, i+1) = #"e" then
							  let
				    				val v = swap(ar,i,i+1)
				    				val _ = Queue.enqueue (queue,v)
			        			  in
				    				ar
			        			  end
			     			     else ar
						else if Vector.sub (ar, i) = #"w" then 
			     				if i = 0 then ar
                             				else if Vector.sub (ar, i-1) = #"e" then
                                			     let     
                                    				  val v = swap(ar,i,i-1)
                                    				  val _ = Queue.enqueue (queue,v)
                                			     in
                                    				ar
                                			     end
			     				else ar
						else ar

					fun jump (ar, i, queue, length) =
						if Vector.sub (ar, i) = #"b" then
							if (i = length-1) orelse (i = length-2) then ar
							else if Vector.sub (ar, i+1) <> #"e" andalso Vector.sub (ar, i+2) = #"e" then 
							     let 
			        	    			   val v = swap(ar,i,i+2)
                                            			   val _ = Queue.enqueue (queue,v)
                                        		     in
                                            			   ar
                                        		     end
				 			else ar
						else if Vector.sub (ar, i) = #"w" then
							if i = 0 orelse i = 1 then ar
                                			else if Vector.sub (ar, i-1) <> #"e" andalso Vector.sub (ar, i-2) = #"e" then 
                                        		     let
                                           	 		  val v = swap(ar,i,i-2)
                                            			  val _ = Queue.enqueue (queue,v)
                                        		     in
                                            			  ar
                                        		     end
					                else ar
						else ar
	
					val _ = jump (arr, i, queue, length)
					val _ = shift (arr, i, queue, length)

	    			  in
					gen_aux (arr, queue, i-1, length)
	    			  end
     			in
			    gen_aux (array, queue, voters-1, voters) 
     			end  


		(*If the queue is empty there aren't new possible states, therego the problem has no solution, the function return's -1
		Else we pop a new state from the queue. If is_winning function returns true the problem is solved. Else if the state is
		not a member of the list "closed", therego it has not been examined before, state_gen produces all next possible states
		and the current state is added to the "closed" list*)
		fun bfsSolver _ _  _ voters true = ~1
  		  | bfsSolver queue closed result voters false = 
			let
				val lst = Queue.contents queue
				val len = List.length lst

				fun bfs_aux []  acc = bfsSolver queue acc (result+1) voters (Queue.isEmpty queue)
		  		  | bfs_aux (h::t) acc = 
					let 
			      		      val _ = Queue.dequeue queue 
					in
			      		      if isWinning h (voters-1) 0 then result
                              		      else if (not (member h voters acc)) andalso (not (exclude h (voters-1) 0 (Vector.sub (h,voters-1)))) then 
				    		    let 
							  val _ = state_gen (h,voters,queue)
				    		    in
							  bfs_aux t (h::acc)
				    		    end
                              		      else bfs_aux t acc
					end

			in
				bfs_aux lst closed
			end
		

		val tuple = parse filename
		val q:char vector Queue.queue = Queue.mkQueue ()
		val _ = Queue.enqueue (q,#1 tuple)
		val voters = #2 tuple

	in
    		bfsSolver q []  0 voters false
	end
	
