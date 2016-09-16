
# KERRY RAYMOND'S SPANNING TREE ALGORITHM	FOR VIRTUAL TOPOLOGY NETWORKS #

Author:	Matthew Bennett
-- Date:		11-20-04 							Modified: 11-30-04
 

A nonproduction implementation of RAYMOND's algorithm which utilizes a logical hierarchy for process priority (holder values) as well as a virtual network topology for the process
"nodes". 
Algorithm is O(log N) where N is number of nodes, dependent on net topology. Each node communicates only with its neighbor in the spanning tree,--	and hold information only about those neighbors

RAYMOND implemented as described in	"A Tree-Based Algorithm for Mutual Exclusion", K. Raymond University of Queensland with additional revisions due to message passing across a virtual topology

Refactorings  11-20-04: (deNOTed @FIX@)
done 
	(1) use linked lists --DONE 11-28-04
	(2) fix integer overflow possibility
	(3) graceful termination (not implemented)
