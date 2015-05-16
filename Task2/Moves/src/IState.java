import java.util.Set;

//Implementation of a certain State
public interface IState {
	public Set<IState> nextStates();
	public boolean isWinning();
	public boolean exclude();
}
