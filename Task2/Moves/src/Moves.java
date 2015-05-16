import java.io.*;



public class Moves {
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int N;
		char [] voters;
		
		if (args.length > 0){
			try{
				//reads the input file and create the array holding the initial state
				BufferedReader input = new BufferedReader(new FileReader(args[0]));
				N = Integer.parseInt(input.readLine());
				voters = new char[N];
				input.read(voters);
				input.close();
				
				//creates a new VoteState object and calls BfsSolver to solve the problem
				VoteState vot = new VoteState(voters);
				BfsSolver solver = new BfsSolver();
				int result = solver.solve(vot);
				System.out.println(result);
			}
			catch (IOException e){
				e.printStackTrace();
			}
		}
		else
			System.out.println("Wrong input. Usage: Moves inputFile.txt");
	}
}
