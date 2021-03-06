import java.io.*;
import java.util.*;

public class Debate {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int row;
		int col;
		int N;
		int temp =-1;
		int distance;
		Arrow[] lista;
		
		if (args.length > 0){
			try{
				//read from input file and create the array holding the Arrow objects
				BufferedReader input = new BufferedReader (new FileReader (args[0]));
				String line = input.readLine();
				String[] a = line.split(" ");
				col = Integer.parseInt(a[0]);
				row = Integer.parseInt(a[1]);
				line = input.readLine();
				N = Integer.parseInt(line);
				lista = new Arrow[N];
				for (int i = 0; i < N; i++){
					line = input.readLine();
					a = line.split(" ");
					Arrow element = new Arrow(Integer.parseInt(a[0]), Integer.parseInt(a[1]), a[2].charAt(0));
					lista[i] = element;
				}
				
				Arguments arguments = new Arguments(lista);
				Visited visited = new Visited(col*row);
				
				while (true){
					Stack<Integer> s = arguments.min_dist();  //minimum distance between two arrows
					temp = arguments.exclude(s); //
					if (temp == 0) //there are no more arrows in the matrix
						break;
					else {
					distance = (s.pop()+1)/2;  //distance that each arrow will cover
					arguments.update(col, row, distance, visited);
					}
				}
				System.out.println(visited.stepsCount());
			}
			catch (IOException e){
				e.printStackTrace();
			}
		}
		else
			System.out.println("Wrong input. Usage : Debate input.txt");

	}

}
