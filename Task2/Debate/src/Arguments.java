/*this class concerns the array that has as elements the arguments, represented by arrow objects*/
import java.util.Stack;

public class Arguments {
	
	public Arrow[] arrowsList;
	
	public Arguments(Arrow[] a){
		arrowsList = a;
	}
	
	/*It excludes all the elements that are about to crush according to the indexes
	 * that the Stack argument contains. Stack's first element is the minimum distance
	 * It returns the number of elements that are excluded */
	public int exclude (Stack<Integer> s){
		int excluded = 0;
		@SuppressWarnings("unchecked")
		Stack<Integer> stack = (Stack<Integer>) s.clone();
		
		int head = stack.pop();
		if (head == 123456789){
			for (int i = 0; i < arrowsList.length; i++)
				if (arrowsList[i].getState() == 0){
					arrowsList[i].xclude();
					excluded++;
				}
		}
		while (!(stack.isEmpty())){
			head = stack.pop();
			if (arrowsList[head].getState()==0){
				arrowsList[head].xclude();
				excluded++;
			}
		}
		
		return excluded;
	}
	
	/*finds the minimum distance between two Arrow objects of the Arguments object
	 * and pushes into a stack the indexes of all the arrows that are this distance far
	 * from each other (arrows that are about to crush)
	 */
	public Stack<Integer> min_dist () {
		Stack<Integer> stack = new Stack<Integer>();
		int size = arrowsList.length;
		int minimum = 123456789;
		
		for (int i=0; i<size; i++){
			if (arrowsList[i].getState() == 0){
				for (int j=i+1; j<size; j++){
					if (arrowsList[j].getState() == 0){
						int distance = arrowsList[i].crush(arrowsList[j]);
						
						if (distance != 123456789){
							if (distance == minimum){
								stack.push(i); 
								stack.push(j);
							}
							else if (distance < minimum){
								stack.clear();
								minimum = distance;
								stack.add(i); 
								stack.add(j);
							}
						}
					}
				}
			}
		}
		stack.push(minimum);
		return stack;
	}
	
	/*
	 * updates the coordinates of the arrows according to the distance that they cover
	 * and the elements of the Visited v argument that are covered are marked as true
	 */
	public void update (int col, int row, int dist, Visited v){
			
		for (int i=0; i<arrowsList.length; i++){
			if (arrowsList[i].getState() != 2){
					if (arrowsList[i].goUp()){
						if ((arrowsList[i].getY()-dist)>=1){
							v.insertColCell(col,arrowsList[i].getX(),arrowsList[i].getY()-dist,dist+1);
							arrowsList[i].moveUp(dist);
							if (arrowsList[i].getState() == 1)
								arrowsList[i].vanish();
						}
						else{
							v.insertColCell(col,arrowsList[i].getX(),1,arrowsList[i].getY());
							arrowsList[i].vanish();
						}
					}
					else if (arrowsList[i].goDown()){
						if ((arrowsList[i].getY()+dist)<=row){
							v.insertColCell(col,arrowsList[i].getX(),arrowsList[i].getY(),dist+1);
							arrowsList[i].moveDown(dist);
							if (arrowsList[i].getState() == 1)
								arrowsList[i].vanish();
						}
						else{
							v.insertColCell(col,arrowsList[i].getX(),arrowsList[i].getY(),row-arrowsList[i].getY()+1);
							arrowsList[i].vanish();
						}
					}
					else if (arrowsList[i].goLeft()){
						if ((arrowsList[i].getX()-dist)>=1){
							v.insertRowCell(col,arrowsList[i].getX()-dist,arrowsList[i].getY(),dist+1);
							arrowsList[i].moveLeft(dist);
							if (arrowsList[i].getState() == 1)
								arrowsList[i].vanish();
						}
						else{
							v.insertRowCell(col,1,arrowsList[i].getY(),arrowsList[i].getX());
							arrowsList[i].vanish();
						}
					}
					else {
						if ((arrowsList[i].getX()+dist)<=col){
							v.insertRowCell(col,arrowsList[i].getX(),arrowsList[i].getY(),dist+1);
							arrowsList[i].moveRight(dist);
							if (arrowsList[i].getState() == 1)
								arrowsList[i].vanish();
						}
						else{
							v.insertRowCell(col,arrowsList[i].getX(),arrowsList[i].getY(),col-arrowsList[i].getX()+1);
							arrowsList[i].vanish();
						}
					}
				}
				
		}
		
	}
	
}
