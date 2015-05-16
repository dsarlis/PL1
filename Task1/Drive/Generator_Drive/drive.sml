fun drive accel slow file =  
   let
       fun parse file =
	   let
	      val input = TextIO.openIn file
	      val x = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)	
	      val y = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	      fun readPiece 0 0 acc = rev acc
	      |   readPiece a b acc = 
		  let 
		     val number1 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		     val number2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		  in 
		     readPiece number1 number2 ((a,b)::acc)
		  end
	    in 
	       readPiece x y []
	    end
	
	fun first (a,_) = a
	fun snd (_,b) = b
	fun sndInt(_,b:int) = b

	fun backTrack _ [] l = ([],l)
	|   backTrack _ l [] = (l,[])
	|   backTrack 0 l1 l2 = (l1,l2)  
	|   backTrack number (h1::t1) (h2::t2) = if (sndInt h1 = sndInt h2) then  backTrack (number-(first h1)) t1 (((first h2)+(first h1),(snd h2))::t2)
						 else backTrack (number - (first h1)) t1 (h1::h2::t2)

	fun clear [] = [] 
	|   clear (h::t) = if ((first h) = 0) then t else (h::t)    	

	fun drive_aux (_,_,moves,result,_,[]) = if moves > 0 then (List.length result + 1) else (List.length result + 2)
        |   drive_aux (cur_speed,test,moves,result,driven,(h::t)) = 
              	if (test<(cur_speed - slow)) 
			then let 
				val x = backTrack (cur_speed div 10) driven (h::t)
        			val y = first x
				val z = snd x
				val current = (first (List.hd result))
				val change = (snd (List.hd result))-10
			      in
				drive_aux (current,(current+change),((current+change) div 10),(List.tl result),y,z)
			      end
		else
		      if (test <= (snd h)) then
			   if (moves <= (first h)) 
			    	then let
					val newList = clear (((first h)-moves,(snd h))::t)
				     in
					drive_aux (test,(test+accel),((test+accel) div 10),((cur_speed,test-cur_speed)::result),((moves,snd h)::driven),newList)
				     end
			   else drive_aux (cur_speed,test,(moves-(first h)),result,(h::driven),t)
		      else 
			   if (moves = test div 10) then drive_aux (cur_speed,test-10,((test-10) div 10),result,driven,(h::t))
			   else			
			   	let
			            val x =  backTrack ((test div 10)-moves) driven (h::t)
		                    val y = first x
				    val z = snd x
 			        in
				    drive_aux (cur_speed,(test-10),((test-10) div 10),result,y,z)
			        end
    
   in
	drive_aux (0,accel,(accel div 10),[],[],(parse file))
   end  
