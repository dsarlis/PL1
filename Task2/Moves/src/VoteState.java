import java.util.*;


//A VoteState object is a state of voters implemented by an array of characters
public class VoteState implements IState {
	private char[] voters;
	private int len;
	
	
	public VoteState (char[] in){
		voters = in;
		len = in.length;
	}
	
	
	//Swaps two voters
	private void swap (int i, int j){
		char ch = voters[i];
		
		voters[i] = voters[j];
		voters[j] = ch;
		
	}
	
	
	//Checks if a "white" voter can go one place to the left
	private boolean canShiftLeft (int i){
		if (voters[i] == 'w'){
			if ((i>0)&&(voters[i-1]=='e'))
					return true;
			else return false;
		}
		else return false;
	}
	
	//Checks if a "black" voter can go one place to the right
	private boolean canShiftRight (int i){
		if (voters[i] == 'b'){
			if ((i < len-1)&&(voters[i+1]=='e'))
					return true;
			else return false;
		}
		else return false;
	}
	
	//Checks if a "white" voter can jump two places to the left
	private boolean canJumpLeft (int i){
		if (voters[i]=='w'){
			if ((i>1)&&(voters[i-1]!='e')&&(voters[i-2]=='e'))
				return true;
			else return false;
		}
		else return false;
	}
	
	//Checks if a "black" voter can jump two places to the right
	private boolean canJumpRight (int i){
		if (voters[i]=='b')
			if ((i<len-2)&&(voters[i+1]!='e')&&(voters[i+2]=='e'))
				return true;
			else return false;
		else return false;
	}
	
	//Moves a "white" voter one place to the left
	private VoteState shiftLeft(int i) throws CloneNotSupportedException{
		char[] v = voters.clone();
		VoteState newState = new VoteState(v);
		newState.swap(i, i-1);
		return newState;
	}
	
	//Moves a "black" voter one place to the right
	private VoteState shiftRight(int i) throws CloneNotSupportedException{
		char[] v = voters.clone();
		VoteState newState = new VoteState(v);
		newState.swap(i, i+1);
		return newState;
	}
	
	//Jumps a "white" voter two places to the left
	private VoteState jumpLeft(int i) throws CloneNotSupportedException{
		char[] v = voters.clone();
		VoteState newState = new VoteState(v);
		newState.swap(i, i-2);
		return newState;
	}
	
	//Jumps a "black" voter two places to the left
	private VoteState jumpRight(int i) throws CloneNotSupportedException{
		char[] v = voters.clone();
		VoteState newState = new VoteState(v);
		newState.swap(i, i+2);
		return newState;
	}
	
	//Method for finding all states derived from a parent state
	public Set<IState> nextStates() {
		//creates a HashSet so we don't add same states to the
		//ones to be checked afterwards
		Set<IState> next = new HashSet<IState>();
		
		try{
			for (int i=0; i<len; i++){
				if (canShiftLeft(i)){
					next.add(shiftLeft(i));
				}
				if (canShiftRight(i)){
					next.add(shiftRight(i));
				}
				if (canJumpLeft(i)){
					next.add(jumpLeft(i));
				}
				if (canJumpRight(i)){
					next.add(jumpRight(i));
				}
			}
		}
		catch (CloneNotSupportedException e){
			e.printStackTrace();
		}
		
		return next;
	}
	
	//Method for determining whether we have reached the final state
	public boolean isWinning (){
		int i = 0;
		
		while ((i<len) && (voters[i]=='w')){
			i++;
		}
		if (i>=len)
			return true;
		else {
			while ((i<len) && (voters[i] == 'e')){
				i++;
			}
			if (i>=len) 
				return true;
			else {
				while ((i<len) && (voters[i]=='b')){
					i++;
				}
				if (i>=len) 
					return true;
				else return false;
			}
		}
	}
	
	//Method for excluding states that have a certain pattern
	//which will not lead to a path to the final state
	public boolean exclude(){
		int i = 0, state = 0 ;
		char check;
		
		while (i < len){
			check = voters[i];
			if (state == 0){
				if (check == 'b')
					state = 1;
			}
			else if (state == 1){
				if (check == 'b')
					state = 2;
				else state = 0;
			}
			else if (state == 2){
				if (check == 'w')
					state=3;
				else if (check == 'e')
					state = 0;
			}
			else if (state == 3){
				if (check == 'b')
					state = 2;
				else if (check == 'e')
					state = 0;
				else return true;
			}
			i++;
		}
		
		if (state == 3) return true;
		else return false;
	}
	


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + len;
		result = prime * result + Arrays.hashCode(voters);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		VoteState other = (VoteState) obj;
		if (len != other.len)
			return false;
		if (!Arrays.equals(voters, other.voters))
			return false;
		return true;
	}

	
}
