----------------------------csc410/prog6/as6.adb----------------------------
-- Author:	Matthew Bennett
-- Class:		CSC410 Burgess
-- Date:		11-20-04 							Modified: 11-30-04
-- Due:			12-01-04
-- Desc:		Assignment 6: KERRY RAYMOND'S SPANNING TREE ALGORITHM
--												FOR VIRTUAL TOPOLOGY NETWORKS
--
--	a nonproduction implementation of
--	RAYMOND's algorithm which utilizes a logical hierarchy for process priority
--	(holder values) as well as a virtual network topology for the process
--	"nodes".
--	Algorithm is O(log N) where N is number of nodes, dependent on net topology.
--	Each node communicates only with its neighbor in the spanning tree,
--	and hold information only about those neighbors	
--
--	RAYMOND implemented as described in
--  	"A Tree-Based Algorithm for Mutual Exclusion", K. Raymond
--  	University of Queensland
--		with additional revisions due to message passing across a virtual topology
----------------------------------------------------------------------------

-- Refactorings  11-20-04: (deNOTed @FIX@)
--done(1) use linked lists --DONE 11-28-04
--		(2) fix integer overflow possibility
--		(3) graceful termination (not implemented)

----------------------------------------------------------------
-- dependencies

WITH ADA.TEXT_IO; 							USE ADA.TEXT_IO;
WITH ADA.INTEGER_TEXT_IO; 			USE ADA.INTEGER_TEXT_IO;
WITH ADA.NUMERICS.FLOAT_RANDOM;	USE ADA.NUMERICS.FLOAT_RANDOM;
WITH ADA.CALENDAR; 	-- (provides cast: natural -> time FOR input into delay)
WITH ADA.STRINGS; 							USE ADA.STRINGS;
WITH ADA.STRINGS.UNBOUNDED; 		USE ADA.STRINGS.UNBOUNDED;

PROCEDURE as6 IS

-- globals: randomPool, taskArray
	randomPool : ADA.NUMERICS.FLOAT_RANDOM.Generator;		--generate our random #s
	MAX_NEIGHBORS : CONSTANT := 50;											--for conserving in arrays

	DELAY_FACTOR 	: CONSTANT := 10.0;											--invariant longer delays

	TYPE RX;																						--recieve task
	TYPE RX_Ptr IS ACCESS RX;														--for spinning off receive

	TASKarray : ARRAY (0..MAX_NEIGHBORS) of RX_Ptr;		--keep up w tasks thrown off

	TYPE passableArray IS ARRAY (0..MAX_NEIGHBORS) of Integer;
		--this is needed bc anonymous types are not allowed in declarations

	TYPE MESG IS (REQ, PRV);
	PACKAGE MESG_IO IS NEW Enumeration_IO(MESG); USE MESG_IO;
	--so that we can natively output enumerated type

--linked list portion----------------------------------------------------------
	TYPE Node;					--fwd declaration needed for self referential structures
	TYPE Node_Ptr IS ACCESS Node;												--for linking

	TYPE Node IS RECORD																	--list building
		myValue : Integer;																--node's value
		nextNode : Node_Ptr := NULL;											--forward chaining
		PrevNode : Node_Ptr := NULL;											--backward chaining
	END RECORD;
----------------------------------
PROCEDURE Enqueue (																	--enqueue in linked list
		Head : IN OUT Node_Ptr;
		in_Value : IN Integer
) IS

tempNode : Node_Ptr;

BEGIN
	tempNode := new	Node;
	tempNode.myValue := in_Value;

	IF (Head = NULL) THEN	Head := tempNode;							--if empty, build list
	ELSE 																								--else insert at beginning
		tempNode.NextNode := Head;
		Head.PrevNode := tempNode;
		Head := tempNode;	
	END IF;

END Enqueue;
----------------------------------
PROCEDURE Dequeue(
	Head : IN OUT Node_Ptr;
	Value : OUT Integer
) IS 

	Curr : Node_Ptr;														--node iterator

BEGIN
	Curr := Head;

	IF (Head /= NULL) THEN -- nonempty queue
		WHILE (Curr.NextNode /= NULL) 						--iterate to end of list
		LOOP
			Curr := Curr.NextNode;
		END LOOP;

		IF (Curr.PrevNode = NULL) THEN Head := NULL;
			ELSE Curr.PrevNode.NextNode := NULL;
		END IF;

		Value := Curr.myValue;
	END IF;
END Dequeue;
----------------------------------
FUNCTION IsEmpty (Head : Node_Ptr)
	RETURN Boolean IS
BEGIN
	IF (Head = NULL) THEN RETURN TRUE;
	ELSE RETURN FALSE; END IF;
END IsEmpty;
--end linked list portion------------------------------------------------------

TASK TYPE RX IS
	ENTRY init( myid : Integer; hold : Integer; Neighbor : passableArray);
	ENTRY Send (mesgTYPE : MESG; FromId : Integer; ToId : Integer);
END RX;

-- BEGIN Receive TASK Definition -- 
TASK BODY RX IS

	NeighborArray 		: passableArray;			--array of possible holders

	HOLDER 						: Integer;						--which node is "higher" in the tree
	USING 						: Boolean := FALSE;		--is this node using CS?
	REQUEST_Q					: Node_Ptr := NULL;		--queue of 
	ASKED							: Boolean := FALSE;		--am i expceted to provide PRIV?
	SELF 							: Integer;						--own ID, for comparison
	--  (variable names agree with those given in RAYMOND)

  outp							: Unbounded_String := Null_Unbounded_String;
	--temporary string variable for buffered output

	TYPE Message;														--fwd declaration, access type
	TYPE Message_Ptr IS ACCESS Message;			--for self reference
	TYPE Message IS RECORD
		m_mesgTYPE : MESG;										--type of message received
		m_fromId : Integer;										--from process number
		m_toId : Integer;											--going to process number (NA)
		NextNode,PrevNode : Message_Ptr := NULL; --back,fwd chaining
	END RECORD;

	MESG_Q 						: Message_Ptr := NULL; --messages stored in a linked list
	
-- procedures for Enqueuing and Dequeuing messages
PROCEDURE Message_Enqueue(
	Head : IN OUT Message_Ptr;
	mesgTYPE : MESG;
	fromId : Integer;
	toId : Integer
) IS

Item : Message_Ptr;

BEGIN
	Item := new Message;
	Item.m_mesgTYPE := mesgTYPE;
	Item.m_fromId := fromId;
	Item.m_toId := toId;
  
	IF (Head = NULL) THEN -- We have an empty queue
		Head := Item;
	ELSE -- Insert at the beginning
		Item.NextNode := Head;
		Head.PrevNode := Item;
		Head := Item;
	END IF;
END Message_Enqueue;
----------------------------------
PROCEDURE Message_Dequeue(
	Head : IN out Message_Ptr;
	tempMesg : out Message)
IS Curr : Message_Ptr;

BEGIN
	Curr := Head;
	IF (Head /= NULL) THEN -- non-empty queue
		WHILE (Curr.NextNode /= NULL) LOOP Curr := Curr.NextNode; END LOOP;
    -- Curr should now point to the last element IN the list --
    IF (Curr.PrevNode = NULL)
			THEN Head := NULL;
    ELSE Curr.PrevNode.NextNode := NULL;
		END IF;

		tempMesg.m_mesgTYPE := Curr.m_mesgTYPE;
		tempMesg.m_fromId := Curr.m_fromId;
		tempMesg.m_toId := Curr.m_toId;
	END IF;
END Message_Dequeue;
----------------------------------
FUNCTION Message_IsEmpty (Head : Message_Ptr) RETURN Boolean IS
BEGIN
IF (Head = NULL) THEN RETURN TRUE;
	ELSE RETURN FALSE;
END IF;
END Message_IsEmpty;

-------------------------------------------------------------------------------	
-- Raymond's algorithm routines
-------------------------------------------------------------------------------	
-- because ada procedures provide serialization, no extra serialization needed

PROCEDURE ASSIGN_PRIVILEGE IS
BEGIN

IF (HOLDER = SELF) AND (NOT USING) AND (NOT IsEmpty(REQUEST_Q)) THEN

	Dequeue(REQUEST_Q, HOLDER);
	ASKED := FALSE;
	IF HOLDER = SELF THEN
		USING := TRUE;
		-- Critical Section
			outp := (((80/6)*SELF) * " ") & Integer'Image(SELF) & " in CS.";
			Put(To_String(outp)); New_line;
			delay (Standard.Duration(Random(randomPool) * DELAY_FACTOR));
			outp := (((80/6)*SELF) * " ") & Integer'Image(SELF) & " out CS.";
			Put(To_String(outp)); New_line;
	ELSE TASKarray(HOLDER).Send(PRV, SELF, HOLDER); 
	END IF;
END IF;

END ASSIGN_PRIVILEGE;

-------------------------------------------------------------------------------	

PROCEDURE MAKE_REQUEST IS
BEGIN
IF (HOLDER /= SELF) AND (NOT IsEmpty(REQUEST_Q)) AND (NOT ASKED) THEN
	taskArray(HOLDER).Send(REQ, SELF, HOLDER);
	ASKED := TRUE;
END IF;
END MAKE_REQUEST;

-------------------------------------------------------------------------------	
--ALGORITHM TASK---------------------------------------------------------------	
TASK TYPE AL IS END AL;
TASK BODY AL IS

currMessage : Message;

BEGIN
LOOP
	--50% chance of waiting, to let someone else try to get in CS
	--I'm still having trouble getting it right at 50%
	--simulate not entering by simply putting that process on the backburner
	IF ((Random(randomPool) * 10.0) > 4.0) THEN--normalize and compare. 50% chance
		Delay (Standard.Duration(Random(randomPool) * DELAY_FACTOR));
		--spare us the busy waits, save processor time

  	-- Node now wishes to enter CS
		Enqueue(REQUEST_Q, SELF);

		ASSIGN_PRIVILEGE;
		MAKE_REQUEST;
--!NOTE! peterson's or semaphores NOT needed because procedures are SERIAL calls

		--node done CS
  	USING := FALSE;
		ASSIGN_PRIVILEGE;
		MAKE_REQUEST;
	END IF;
  
	-- Process any messages in the message queue
	IF (NOT Message_IsEmpty(MESG_Q)) THEN
		Message_Dequeue(MESG_Q, currMessage);
		case currMessage.m_mesgTYPE IS
			WHEN PRV =>
			BEGIN
			--outp := (((80/6)*SELF) * " ") &Integer'Image(SELF) & " Holder to self";
			--put(To_String(outp)); New_line;
				HOLDER := SELF;
				ASSIGN_PRIVILEGE;
				MAKE_REQUEST;
			END;
			
			WHEN REQ =>
			BEGIN
				Enqueue(REQUEST_Q, currMessage.m_FromId);
				ASSIGN_PRIVILEGE;
				MAKE_REQUEST;
			END;
			
			WHEN Others => NULL; --required by ada standards -- not called

		END Case;
	  
	ELSE Delay (Standard.Duration(Random(randomPool) * DELAY_FACTOR ));
	-- delay to let others in/finish
END IF;
		
END LOOP;
END AL;

TYPE AL_Ptr IS ACCESS AL;
aPtr : AL_Ptr; --spin off single ALgorithm task for each eexternal RX task

-------------------------------------------------------------------------------	
-- BEGIN RX
BEGIN 

ACCEPT Init( --simple initializations
		myid : Integer;
		hold : Integer;
		Neighbor : passableArray
) DO

	NeighborArray := Neighbor;

	HOLDER := hold;
	SELF := myid;	

END Init;

	aPtr := new AL; -- spin off algorithm task

-------------------------------------------------------------------------------
--start receiving messages
LOOP
SELECT
--just plain easier this way, for enqueing and dequeing w/o translation
ACCEPT Send (mesgTYPE : MESG; FromId : Integer; ToId : Integer)
DO 
	Message_Enqueue (MESG_Q, mesgTYPE, fromId, ToId);
	outp := (((80/6)*SELF) * " ") &Integer'Image(SELF) & " " & MESG'Image(MESGTYPE)
				& Integer'Image(FromId) & "," & Integer'Image(ToID);
	Put(To_String(outp)); New_line;
END Send;

END SELECT;
END LOOP; -- RX LOOP

END RX;
-------------------------------------------------------------------------------
PROCEDURE Driver IS
	infile 	: FILE_TYPE; --ada.standard.textIO type for reading ascii and iso-8XXX

	taskId 									: Integer;				--temporary, for task intialization
	holder 									: Integer;				--temporary, for task intialization
	neighborArray						: passableArray;	--temporary, for task intialization

	neighborCount						: Integer := 0;		--temp, for filling NeighborArray

	seedUser			: Integer; --user input random seed FOR random number generator

	filename		: string(1..5);	--what file should we read from?

BEGIN
	put_line("Raymond's Tree-based Algorithm");

	put("# random seed:       ");
	get(seedUser); --to  ensure a significantly random series, a seed is needed
									-- to generate pseudo-random numbers
 	Ada.Numerics.Float_Random.Reset(randomPool,seedUser);
		--seed the random number pool

	put("Filename:            ");
	get(filename);
	--first lets read in the 
	Open (File=> inFile, Mode => IN_FILE, Name => filename);
	--open as read only ascii and use reference infile
	--file format is:	nodeID <initial holder> neighbor ...neighbor [MAX_NEIGHBORS]

	WHILE NOT END_OF_FILE(infile) 
	LOOP
		Get(infile, TASKId);
		Get(infile, Holder);
		WHILE NOT END_OF_LINE(infile)
		LOOP
			Get( infile, neighborArray(neighborCount) );
			neighborCount := neighborCount + 1;
		END LOOP;
		TASKarray(TASKId) := new RX;						--spin task, initialize
		TASKarray(TASKId).init(	taskId, holder, neighborArray);
	END LOOP;

  Close(infile);

	EXCEPTION
   WHEN Name_Error =>
      Put(Item => "File not found.");
   WHEN Status_Error =>
      Put(Item => "File already open.");
   WHEN Use_Error =>
      Put(Item => "You lack permission to open file");
   WHEN constraint_Error =>
      Put(Item => "problem IN code! constraint error thrown");

END Driver;

BEGIN Driver; END as6; --seperation of globals
