package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

/**
 * A value which does not need any parameters, but has to be evaluated before used.
 * Used to build CAF's (Constant Applicative Forms).
 */
public class Apply0 extends Apply
{
    // private String name = Function.defaultNoName ;
    
	public Apply0( )
	{
	    super( null ) ;
	}
	
	// special case to be lookalike to Function, name is not used (yet)
	public Apply0( String nm )
	{
	    super( null ) ;
	    // name = nm ;
	}
	
	public Apply0( Object f )
	{
		super( f ) ;
	}
	
    public Object[] getBoundParams()
    {
	    return Utils.zeroArray ;
    }

    public int getNrBoundParams()
    {
        return 0 ;
    }

    protected void eraseRefs()
    {
    }
    
}
