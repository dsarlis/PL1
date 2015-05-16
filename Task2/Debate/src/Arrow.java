/*each arrow has a xCoordinate, a yCoordinade, a direction and a state*
 * state 0: the arrow is in the matrix
 * state 1: the arrow is about to crush 
 * state 2: the arrow either has crushed or has exceeded the matrix' dimensions
 */

public class Arrow {
	private int x;
	private int y;
	private char d;
	private int state;
	
	public Arrow (int xcoor, int ycoor, char direction){
		x = xcoor;
		y = ycoor;
		d = direction;
		state = 0;
	}
	
	public int getX(){
		return x;
	}
	
	public int getY(){
		return y;
	}
	
	public int getDir(){
		return d;
	}
	
	public int getState(){
		return state;
	}

	public boolean goUp() {
		return d=='u';
	}
	
	public boolean goDown() {
		return d=='d';
	}
	
	public boolean goLeft(){
		return d=='l';
	}
	
	public boolean goRight(){
		return d=='r';
	}
	
	public void moveUp(int step){
		y -= step;
	}
	
	public void moveDown(int step){
		y += step;
	}
 
	public void moveLeft(int step){
		x -= step;
	}
	
	public void moveRight(int step){
		x += step;
	}
	
	public void xclude(){
		state = 1;
	}
	
	public void vanish(){
		state = 2;
	}
	
	/*if two arrows are possible to crush it returns the distance between them, else it returns 123456789*/
	public int crush (Arrow arrow){
		/*same row*/
			if (this.y == arrow.y){
				if (this.x < arrow.x){
					if (this.goRight() && arrow.goLeft())
						return arrow.x - this.x -1;
					else return 123456789; 
				}
				else /*x>arrow.x*/ {
					if (this.goLeft() && arrow.goRight())
						return this.x-arrow.x-1;
					else return 123456789;
				}
			}
			/*same column*/
			else if (this.x == arrow.x) {
				if (this.y<arrow.y){
					if (this.goDown() && arrow.goUp())
						return arrow.y-this.y-1;
					else return 123456789;
				}
				else /*y>arrow.y*/{
					if (this.goUp() && arrow.goDown())
						return this.y-arrow.y-1;
					else return 123456789;
				}
			}
			/*diagonal elements*/
			else if (Math.abs(this.x-arrow.x) == Math.abs(this.y-arrow.y)){
				if (this.x<arrow.x){
					if (this.y<arrow.y){
						if ((this.goDown() && arrow.goLeft()) || (this.goRight() && arrow.goUp()))
							return 2*Math.abs(this.y-arrow.y)-1;
						else return 123456789;
					}
					else {
						if ((this.goUp() && arrow.goLeft()) || (this.goRight() && arrow.goDown())) 
							return 2*Math.abs(this.y-arrow.y)-1;
						else return 123456789;
					}
				}
				else{
					if (this.y<arrow.y){
						if ((this.goLeft() && arrow.goUp()) || (this.goDown() && arrow.goRight())) 
							return 2*Math.abs(this.y-arrow.y)-1;
						else return 123456789;
					}
					else {
						if ((this.goUp() && arrow.goRight()) || (this.goLeft() && arrow.goDown()))
							return 2*Math.abs(this.y-arrow.y)-1;
						else return 123456789;
					}
				}
			}
			else return 123456789;
	}
}
