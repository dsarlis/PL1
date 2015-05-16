fun omilies file  =
   let
	val input = TextIO.openIn file
	(*reads the number of cities (N) and number of connections (M)*)
	val cities = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	val connections = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	(*ignores new line character*)
	val garbage = TextIO.inputLine input
        val initial = Array.array (cities,[]:(int*int) list)

	(*Reads the text and creates an array of list. Each element of the array represents 
	a city. Each list contains tuples (city*distance) for the cities that communicate 
	with the city that the element of the array represents.*)
	fun readEdges 0 acc = acc
	|   readEdges m acc = 
		let
		     val x = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		     val y = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)	
		     val dist = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		     val garbage = TextIO.inputLine input

		     val _ = Array.update (acc, x-1, (y-1,dist)::Array.sub(acc,x-1))
		     val _ = Array.update (acc, y-1, (x-1,dist)::Array.sub(acc,y-1))
 
		in
		     readEdges (m-1) acc
		end

	val graph = readEdges connections initial
	
	(*reads A, B, C city's position and number of questions (L) and ignores new line*)	
	val cityA = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	val cityB = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	val cityC = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	val quest = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	val garbage =  TextIO.inputLine input

	
	val maxInt = 10000000
	
	(*Initialises an array of tuples for each of the cities A,B,C. Each element of the array
	represents a city of the graph. The first element of the tuple shows the length of the 
	path to the city and the second indicates if the city is visited*)
	val distA = Array.array (cities, (maxInt,false))
	val _ = Array.update (distA, (cityA-1), (0,false))
        val distB = Array.array (cities, (maxInt,false))
	val _ = Array.update (distB, (cityB-1), (0,false))
        val distC = Array.array (cities, (maxInt,false))
	val _ = Array.update (distC, (cityC-1), (0,false))

	fun fst (a,_) = a
	fun snd (_,b) = b

	(*Finds the position of the city for which the path is the shortest and marks it as visited*)
	fun min array cities maxInt =
        let
                val length = cities

                fun minAux (array,minimum,minPos,i) =
                        if (i = length) then minPos
                        else
                              if (((fst (Array.sub(array,i))) < minimum) andalso ((snd (Array.sub(array,i))) = false)) 
			      then minAux (array,(fst (Array.sub(array,i))),i,(i+1))
                              else minAux (array,minimum,minPos,(i+1))
        in
                 minAux  (array,maxInt,~1,0)
        end
	
	(*Updates the path to a city if a path through a neighbour city is shorter*)
	fun update (distArray,u,[]) = ()
	|   update (distArray,u,(head::t)) =
		let 
		    val alt = (snd head) + fst (Array.sub(distArray,u))
		in
		    if (alt < (fst (Array.sub(distArray,fst head)))) then
			let
                           val _ = Array.update(distArray,(fst head),(alt,false))
			in
			    update (distArray,u,t)
			end
	            else update (distArray,u,t)     
		end 
		
	
	fun dijkstra  (0,city,distArray,graph) = distArray
	|   dijkstra  (n,city,distArray,graph) = 
		let
			val _ = Array.update(distArray, city, (fst (Array.sub(distArray,city)),true))
			val _ = update (distArray,city,Array.sub(graph,city))
		in 
			dijkstra ((n-1),min distArray cities maxInt,distArray,graph)
		end

	val dijkA = dijkstra (cities,(cityA-1),distA,graph)
	val dijkB = dijkstra (cities,(cityB-1),distB,graph)
	val dijkC = dijkstra (cities,(cityC-1),distC,graph)

	fun result 0 a b c cities acc = List.rev acc
	|   result i a b c cities acc = 
		let
			(*reads the city for which a question is made*)
			val x = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
        		val garbage =  TextIO.inputLine input
			
			(*checks the distA distB and distC arrays and returns true if the city x is the
			best option else it returns false if there is a better option*)
			fun checkCity x a b c cities =
				let
				    val abc = (fst (Array.sub(a,x-1)),fst (Array.sub(b,x-1)),fst (Array.sub(c,x-1)))
				   
				    fun fst3(a,_,_) = a
				    fun snd3(_,b,_) = b
				    fun thrd3(_,_,c) = c
				    
				    fun loop (i,trinity,cities) = 
						if (i = cities) then true
					        else
						      if ((fst3 trinity) > (fst (Array.sub(a,i))) 
						      andalso (snd3 trinity) > (fst (Array.sub(b,i))) 
						      andalso (thrd3 trinity) > (fst (Array.sub(c,i)))) 
					              then false else loop ((i+1),trinity,cities) 
				in
					loop (0,abc,cities)
				end

		in 
			result (i-1) dijkA dijkB dijkC cities ((checkCity x dijkA dijkB dijkC cities)::acc)
		end
	 
    in
	result quest dijkA dijkB dijkC cities []
    end


