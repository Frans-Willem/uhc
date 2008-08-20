%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hardcoded / configured sizes + types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Requirement: pointers should fit into a Word

%%[8
#if USE_64_BITS
typedef uint64_t Word  ;
typedef  int64_t SWord ;

#define Word_SizeInBits			64
#define Word_SizeInBits_Log		6

#define Word_SizeInBytes		8
#define Word_SizeInBytes_Log	3

#else

typedef uint32_t Word  ;
typedef  int32_t SWord ;

#define Word_SizeInBits			32
#define Word_SizeInBits_Log		5

#define Word_SizeInBytes		4
#define Word_SizeInBytes_Log	2

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef	void*		Ptr  ;
typedef	uint8_t*	BPtr ;
%%]