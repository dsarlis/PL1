(*Function for reading the input file and creating an array of lists
holding the debts of the form [[(Time1,Debt1),(Time2,Debt2),...],[...],...].
Each element of the array is initilized to the empty list and matches each of the banks.
For each debt (P,T1,X1,T2,X2) read we put the tuple (X1,~P) to the bank tha owns and
(T2,P) to the bank that receives the amount. The function returns the array created 
and the number of banks.*)
fun parse file = 
	let 
	     val input = TextIO.openIn file
	     val banks = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	     val m = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
         val _ = TextIO.inputLine input
		 val Debts = Array.array(banks,[])

	     fun read_debts 0 pinakas = pinakas
	       | read_debts n pinakas = 
			let
			      val p  = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
			      val t1 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
                  val x1 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
			      val t2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
			      val x2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
				  val _  = Array.update(pinakas,(t1-1),((x1,~p)::Array.sub(pinakas,(t1-1))))
				  val _  = Array.update(pinakas,(t2-1),((x2,p)::Array.sub(pinakas,(t2-1))))
			      val _  = TextIO.inputLine input
			in
			      read_debts (n-1) pinakas
			end
	in
		(banks,read_debts m Debts)
	end 

local
    fun printTR []       _         = ()
      | printTR (h :: t) outStream =
        (   
            TextIO.output(outStream, Int.toString h ^ " ");
            printTR t outStream
        )   
in 
    fun write l file =
        let
            val outStream = TextIO.openOut file
        in 
            (   
                printTR l outStream;
                TextIO.flushOut(outStream)
            )   
        end
end

	
fun mergeSort nil = nil
  | mergeSort [e] = [e]
  | mergeSort theList =
		let
			fun halve nil = (nil, nil)
			  | halve [a] = ([a], nil)
			  | halve (a::b::cs) =
					let
						val (x, y) = halve cs
					in
						(a::x, b::y)
					end;
					
			fun merge (nil, ys) = ys
			  | merge (xs, nil) = xs
			  | merge ((x1,x2)::xs, (y1,y2)::ys) =
						if (x1 < y1) then (x1,x2) :: merge(xs, (y1,y2)::ys)
						else (y1,y2) :: merge((x1,x2)::xs, ys);
						
			val (x, y) = halve theList
		in
			merge(mergeSort x, mergeSort y)
		end;

(*Function for sorting the debts of each bank in increasing time order*)	
fun timeSort 0 pinakas = ()	
  | timeSort n pinakas = 
		let
			 val List = Array.sub(pinakas,(n-1))
			 val _ = Array.update(pinakas,(n-1), (mergeSort List))
		in
			 timeSort (n-1) pinakas
		end;
		
(*Function for suming together all debts refering to
a certain time for each of the banks.
Example: Bank1 has these debts [(1,2),(1,-1),(2,4),(3,5),(4,-7),(4,5),(5,2)]
The result wil be [(1,1),(2,4),(3,5),(4,-2),(5,2)]*)
fun timeSum 0 pinakas = ()
  |	timeSum n pinakas = 
		let 
			fun timeSumAux _ _ [] acc = acc
			  | timeSumAux k sum [(time:int,debts)] acc = if k = time then List.rev ((time,sum+debts)::acc) else List.rev ((time,debts)::(k,sum)::acc)
			  | timeSumAux k sum ((time:int,debts)::t) acc = 
						if k = time then timeSumAux k (sum+debts) t acc
						else timeSumAux time debts t ((k,sum)::acc)
						
			val theList = Array.sub(pinakas,(n-1))
		in 
			if theList = nil then timeSum (n-1) pinakas
			else 
				let
					val (time,_) = List.hd theList
					val _  = Array.update(pinakas,(n-1), (timeSumAux time 0 theList []))
				in
					timeSum (n-1) pinakas
				end
		end;

(*Function for determing how much money should a bank have
so it can pay it's debts. We find the sum of all debts across time.
If the sum in some point is negative we add the opposite value
in an accumulator so the sum can come back to zero. If the sum 
is positive we simply continue. When the list of debts is empty 
the accumulator holds the initial fund the bank should have*)		
fun balance [] sum acc = acc
  | balance ((_,debts)::t) sum acc = 
		let 
			val check = sum + debts
		in
			if check < 0 then balance t 0 (~check + acc)
			else balance t check acc
 		end;

(*Function for finding the initial fund for each bank.
It uses the auxiliary function balance*)
fun giveFunds 0 pinakas acc = acc		
  |	giveFunds n pinakas acc = 
			let 
				val theList = Array.sub(pinakas,(n-1))
				val fund = balance theList 0 0
			in
				giveFunds (n-1) pinakas (fund::acc)
			end;

(*Main function of the solution. It creates the array of debts 
in the desired form and calculates the initial funds each bank should have.
It returns the result in a list.*)
fun neurozone filename = 
		let
			val (banks,DebtsArray) = parse filename
			val _ = timeSort banks DebtsArray
			val _ = timeSum banks DebtsArray
		in
			 giveFunds banks DebtsArray []
		end;
