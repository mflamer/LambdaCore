#pragma once

#include <string>
#include <iostream>

#define RAMSIZE 262143
#define ARGSTACKSIZE 32
#define RETSTACKSIZE 32
#define FRAMESIZE 8


//#define LIT_MASK		0x7FFFFFFF
//#define PTRN_MASK		0xC0000000
//#define PTRN_SHIFT		30
//#define A_MASK			0x38000000
//#define A_SHIFT			27
//#define ARGS_MASK		0x04000000
//#define ARGS_SHIFT		26
//#define ALLOC_MASK		0x02000000
//#define ALLOC_SHIFT		25
//#define PC_MASK			0x01C00000
//#define PC_SHIFT		22
//#define RETS_MASK		0x00300000
//#define RETS_SHIFT		20
//#define ARGS_INC_MASK	0x000C0000
//#define ARGS_INC_SHIFT  18
//#define RETS_INC_MASK	0x00030000
//#define RETS_INC_SHIFT  16
//#define ALU_MASK		0x0000F800
//#define ALU_SHIFT		11
//#define E_MASK			0x00000600
//#define E_SHIFT			9
//#define F_MASK			0x00000180
//#define F_SHIFT			7
//#define E_INC_MASK		0x00000060
//#define E_INC_SHIFT		5
//#define DB_MASK			0x0000001F

#define FRAME_MASK		0xFFFFFFF8
#define CELL_MASK		0x00000007
#define A_ENV_MASK		0xFFFF0000
#define A_ENV_SHIFT		16
#define A_CODE_MASK		0x0000FFFF

#define F_CNT_MASK		0xFFFF0000
#define F_CNT_INC		0x00010000
#define F_CNT_SHIFT		16
#define F_LINK_MASK		0x0000FFFF


#define OP_MASK			0xFC
#define DATA_CNT_MASK   0x03


//---------------------------------------------------------
//------ Instructions -------------------------------
					
#define NOP  0x00
#define LDI	 0x04
#define LDIP 0x08
#define CLOS 0x0C
#define IF	 0x10
#define CALL 0x14
#define JUMP 0x18
#define RET	 0x1C
#define ACC  0x20
#define ACCP 0x24
#define APPT 0x28
#define APP	 0x2C
#define PUSH 0x30
#define MARK 0x34
#define GRAB 0x38
#define RETC 0x3C
#define LET	 0x40
#define ELET 0x44
#define TEMP 0x48
#define UPDT 0x4C
#define END	 0x50

#define ADD  (char)0xA0
#define	SUB	 (char)0xA4
#define	MUL	 (char)0xA8
#define	AND	 (char)0xAC
#define	OR	 (char)0xB0
#define	NOT	 (char)0xB4
#define	XOR	 (char)0xB8
#define	XNOR (char)0xBC
#define	ASHR (char)0xC0
#define	LSHR (char)0xC4
#define	LSHL (char)0xC8
#define	EQ	 (char)0xCC
#define	NE	 (char)0xD0
#define	GT	 (char)0xD4
#define	LT	 (char)0xD8
#define	GTE	 (char)0xDC
#define	LTE	 (char)0xE0
#define	ABV	 (char)0xE4
#define	BEL	 (char)0xE8



#define		INITVAL		0x00000000
#define 	MARK_VAL	0x80000000

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
	size_t					LoadRAM(std::string file);
	void					Run(bool printState);
	bool					Step(bool printState);

	void					Disassemble(size_t cnt);
	


	unsigned char RAM[RAMSIZE];		
	int argS[ARGSTACKSIZE];
	int retS[RETSTACKSIZE];
	int F[FRAMESIZE];

	int A;					// accumulator
	unsigned int PC;		// program counter
	//unsigned int _PC;      
	unsigned int E;		// environment 
	//unsigned short _E;
	unsigned int N;		// next allocation address
	unsigned char arg_TOS;  // top of arg stack 
	//unsigned char _arg_TOS;
	unsigned char ret_TOS;  // top of ret stack 
	//unsigned char _ret_TOS;


	
	char DB;		// debrujin index
	unsigned int FA;		// frame address
	//short _FA;

	bool RAM_R;
	bool RAM_W;

	bool A_stall;		//The processor must stall when searching through envs
	bool F_stall;		//The processor must stall when setting a new env

	bool late_write_F;  //We are loading a frame and need to add a value to it on the next cycle
	bool newFrame;      //We are linking to the next available frame
	bool loadWord;
	unsigned int late_write_val; 

	void	NewFrame(int firstParam, bool incRef = true);



	//Stats
	int newFrames;
	int disposedFrames;
	

};








	