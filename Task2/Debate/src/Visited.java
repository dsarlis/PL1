/*
 * a boolean arrow that represents the whole matrix. If an element is marked as true
 * it has been covered by an arrow-argument as it moves into the matrix
 */

public class Visited {
	private boolean[] visited;
	
	public Visited (int size){
		visited = new boolean[size];
		for (int i=0; i<size; i++)
			visited[i] = false;
	}

	/*marks true the elements that an arrow covers as it moves along a row*/
	public void insertRowCell (int col,int x, int y, int step){
		for (int i=0; i<step; i++){
			visited[(y-1)*col+x+i-1] = true;
		}
	}
	
	/*marks as true the elements that an arrow covers as it moves along a column*/
	public void insertColCell (int col, int x, int y, int step){
		for (int i=0; i<step; i++){
			visited[(y+i-1)*col+x-1] = true;
		}
	}
	
	/*it sums the cells of the matrix that are marked as true*/
	public int stepsCount (){
		int count = 0;
		
		for (int i=0; i<visited.length; i++){
			if (visited[i]==true)
				count++;
		}		
		return count;
	}
	
}
