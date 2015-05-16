

//Solver: Implementation of a graph search algorithm in the state space
public interface ISolver {
	
	// Runs the solver, starting from initialState. Returns the minimum
	//number of moves that are required so we can go from the initial state to
	//the final one
	public int solve(IState initialState);
}
