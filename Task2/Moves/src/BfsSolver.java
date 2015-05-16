import java.util.*;


public class BfsSolver implements ISolver{
		
		public BfsSolver (){}
				
		public int solve(IState initialState){
			//creates a map holding all the states we have already searched
			Map<IState,IState> visited = new HashMap<IState,IState>();
			//Structure holding the states of a certain level to be checked
			List<IState> fringe = new LinkedList<IState>();
			//Structures holding the states born by a parent state
			List<IState> nextFringe = new LinkedList<IState>();
			
			//add the initial state to the map
			visited.put(initialState, null);
			fringe.add(initialState);
			int moves = 0;
			
			//if the current first state in fringe is the final return moves
			//else produce all the new states from this parent state
			//if a new state is not visited add it to nextFringe and make it visited
			//if nextFringe is empty which means we don't have any other states to check 
			//return -1 else continue checking the states in nextFringe
			while (true){
				for (IState currState : fringe){
					if (currState.isWinning())
						return moves;
					Set <IState> nextStates = currState.nextStates();
					for (IState s : nextStates)
						if (!visited.containsKey(s)&&!s.exclude()){
							nextFringe.add(s);
							visited.put(s, currState);
						}
					
				}
				if (nextFringe.isEmpty())
					return -1;
				fringe = nextFringe;
				nextFringe = new LinkedList<IState>();
				moves++;
			}
		}
}
