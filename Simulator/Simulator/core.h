#pragma once

#include <string>
#include <iostream>

#define RAMSIZE 65535
#define ARGSTACKSIZE 32
#define RETSTACKSIZE 32
#define FRAMESIZE 8

#define PAT_ALU			1
#define PAT_JMPCLOS		0
#define PAT_LDI			4
#define PAT_END			3

#define LIT_MASK		0x7FFFFFFF
#define PTRN_MASK		0xE0000000
#define PTRN_SHIFT		29
#define A_MASK			0x1C000000
#define A_SHIFT			26
#define ARGS_MASK		0x02000000
#define ARGS_SHIFT		25
#define ALLOC_MASK		0x01000000
#define ALLOC_SHIFT		24
#define PC_MASK			0x00C00000
#define PC_SHIFT		22
#define RETS_MASK		0x00300000
#define RETS_SHIFT		20
#define ARGS_INC_MASK	0x000C0000
#define ARGS_INC_SHIFT  18
#define RETS_INC_MASK	0x00030000
#define RETS_INC_SHIFT  16
#define ALU_MASK		0x0000F800
#define ALU_SHIFT		11
#define E_MASK			0x00000600
#define E_SHIFT			9
#define F_MASK			0x00000180
#define F_SHIFT			7
#define E_INC_MASK		0x00000060
#define E_INC_SHIFT		5
#define DB_MASK			0x0000001F

#define FRAME_MASK		0xFFFFFFF8
#define CELL_MASK		0x00000007
#define A_ENV_MASK		0xFFFF0000
#define A_ENV_SHIFT		16
#define A_CODE_MASK		0x0000FFFF

//---------------------------------------------------------
//------ Instructions -------------------------------
					
#define 	LDI		0x80000000	    
#define		ACC		0x24000000
#define		APPT	0x204C02A0			
#define		APP		0x205D02A0		
#define		PUSH	0x22040000	
#define		MARK	0x3E04F800	
#define		CLOS	0x10000000
#define		GRAB	0x288E04C0		
#define		RET		0x20CE06C0	
#define		LET		0x20000120		
#define		ELET	0x20000060			
#define		TEMP	0x2000F920	
#define		UPDT	0x20000100	
#define		END		0x60000000
//#define	JMP			
//#define	IF			
//#define	EQ			
//#define	NE			
//#define	LT			
//#define	GT			
//#define	LTE			
//#define	GTE			
//#define	ABV			
//#define	BEL			
#define 	ADD		0x3C0C1100		
//#define	SUB		
//#define	AND     
//#define	OR		
//#define	XOR		
//#define	NOT		
//#define	XNOR	
//#define	ASHR	
//#define	LSHR	
//#define	LSHL	

#define		INITVAL 0
#define 	MARK_VAL		0x80000000

struct Closure
{
	unsigned short code;
	unsigned short env;
};

//union Val
//{
//	int m; //stack marker
//	int i; //int value
//	Closure c;	
//}u = {0x80000000};//default init to mark


class Core
{
public:

	Core();

	void					Reset();
	void					LoadRAM(std::string file);
	void					Run(bool printState);
	bool					Step(bool printState);

	void					Disassemble();
	


	int RAM[RAMSIZE];		
	int argS[ARGSTACKSIZE];
	int retS[RETSTACKSIZE];
	int F[FRAMESIZE];

	int A;					// accumulator
	unsigned short PC;		// program counter
	unsigned short _PC;      
	unsigned short E;		// environment 
	unsigned short _E;
	unsigned short N;		// next allocation address
	unsigned char arg_TOS;  // top of arg stack 
	unsigned char ret_TOS;  // top of ret stack 



	char DB;		// debrujin index
	short FA;		// frame address
	short _FA;

	bool RAM_R;

	bool A_stall;		//The processor must stall when searching through envs
	bool F_stall;		//The processor must stall when setting a new env

	unsigned int link; 
	

};