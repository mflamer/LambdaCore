#include "core.h"

int PrintInst(char inst[], unsigned int PC)
{
	int result = 1;
	int skip = 1;
	char op = inst[0] & OP_MASK;
	char n = inst[0] & DATA_CNT_MASK;
	int data = 0;
	switch(n)
	{
	case 0:
		data = (int)(inst[1] | (inst[2]<<8) | (inst[3]<<16) | (inst[4]<<24));
		skip = 5;
		break;
	case 1:
		data = (unsigned char)inst[1];
		//data = ((int)(inst[1] << 24) >> 24);
		skip = 2;
		break;
	case 2: 
		data = ((unsigned char)inst[1] | ((unsigned char)inst[2]<<8));
		//data = ((int)(inst[1] << 16)|((int)(inst[2] << 24)) >> 16);
		skip = 3;
		break;		   
	case 3:		
		data = ((unsigned char)inst[1] | ((unsigned char)inst[2]<<8) | ((unsigned char)inst[3]<<16));
		//data = ((int)(inst[1]<<8)|(int)(inst[2]<<16)|((int)(inst[3]<<24))>>8);
		skip = 4;
		break;		   
	}
	switch((int)op)
	{
		case NOP:
			std::cout << PC << ": \t" << "NOP \n";			
			break;
		case LDI:
			std::cout << PC << ": \t" << "LDI " << data << "\n";			
			result = skip;
			break;
		case LDIP:
			std::cout << PC << ": \t" << "LDIP " << data << "\n";		
			result = skip;
			break;
		case CLOS:
			std::cout << PC << ": \t" << "CLOS " << data << "\n";			
			result = skip;
			break;	
		case IF:
			std::cout << PC << ": \t" << "IF " << data << "\n";			
			result = skip;
			break;
		case CALL:
			std::cout << PC << ": \t" << "CALL " << data << "\n";		
			result = skip;
			break;
		case JUMP:
			std::cout << PC << ": \t" << "JUMP " << data << "\n";		
			result = skip;
			break;
		case RET:
			std::cout << PC << ": \t" << "RET \n";			
			break;
		case ACC:
			std::cout << PC << ": \t" << "ACC " << data << "\n";
			result = skip;
			break;
		case ACCP:
			std::cout << PC << ": \t" << "ACCP " << data << "\n";
			result = skip;
			break;
		case APPT:
			std::cout << PC << ": \t" << "APPT \n";
			break;
		case APP:
			std::cout << PC << ": \t" << "APP \n";
			break;
		case PUSH:
			std::cout << PC << ": \t" << "PUSH \n";
			break;
		case MARK:
			std::cout << PC << ": \t" << "MARK \n";
			break;
		case GRAB:
			std::cout << PC << ": \t" << "GRAB \n";
			break;
		case RETC:
			std::cout << PC << ": \t" << "RETC \n";
			break;
		case LET:
			std::cout << PC << ": \t" << "LET \n";
			break;
		case ELET:
			std::cout << PC << ": \t" << "ELET \n";
			break;
		case TEMP:
			std::cout << PC << ": \t" << "TEMP \n";
			break;
		case UPDT:
			std::cout << PC << ": \t" << "UPDT \n";
			break;
		case END:
			std::cout << PC << ": \t" << "END \n";
			break;
		case ADD:
			std::cout << PC << ": \t" << "ADD \n";
			break;
		case SUB:
			std::cout << PC << ": \t" << "SUB \n";
			break;
		case MUL:
			std::cout << PC << ": \t" << "MUL \n";
			break;
		case AND:
			std::cout << PC << ": \t" << "AND \n";
			break;
		case OR:
			std::cout << PC << ": \t" << "OR \n";
			break;
		case NOT:
			std::cout << PC << ": \t" << "NOT \n";
			break;
		case XOR:
			std::cout << PC << ": \t" << "XOR \n";
			break;
		case XNOR:
			std::cout << PC << ": \t" << "XNOR \n";
			break;
		case ASHR:
			std::cout << PC << ": \t" << "ASHR \n";
			break;
		case LSHR:
			std::cout << PC << ": \t" << "LSHR \n";
			break;
		case LSHL:
			std::cout << PC << ": \t" << "LSHL \n";
			break;
		case EQ:
			std::cout << PC << ": \t" << "EQ \n";
			break;
		case NE:
			std::cout << PC << ": \t" << "NE \n";
			break;
		case GT:
			std::cout << PC << ": \t" << "GT \n";
			break;
		case LT:
			std::cout << PC << ": \t" << "LT \n";
			break;
		case GTE:
			std::cout << PC << ": \t" << "GTE \n";
			break;
		case LTE:
			std::cout << PC << ": \t" << "LTE \n";
			break;
		case ABV:
			std::cout << PC << ": \t" << "ABV \n";
			break;
		case BEL:
			std::cout << PC << ": \t" << "BEL \n";
			break;
		default:
			std::cout << "Bad instruction encoding! \n";
			break;		

	}
	return result;
}

Core::Core()
{
	Reset();
}


void Core::Reset()
{
	//initalize data
	for (unsigned i = 0; i < RAMSIZE; ++i) RAM[i] = INITVAL;
	for (unsigned i = 0; i < ARGSTACKSIZE; ++i) argS[i] = INITVAL;
	for (unsigned i = 0; i < RETSTACKSIZE; ++i) retS[i] = INITVAL;
	for (unsigned i = 0; i < FRAMESIZE; ++i) F[i] = INITVAL;

	A = 0;
	PC = 0;
	E = 0;
	N = 0;
	arg_TOS = 0xFF;
	ret_TOS = 0xFF;
	DB = 0;
	FA = 0;
	RAM_R = false;
	RAM_W = false;
	A_stall = false;
	F_stall = false;

	late_write_F = false;
	late_write_val = 0;

	loadWord = false; 

}


size_t Core::LoadRAM(std::string fileName)
{
	FILE* pFile;
	fopen_s(&pFile, fileName.c_str(), "rb");
	if (pFile!=NULL)
	{
		// get length of file:		  
		fseek (pFile , 0 , SEEK_END);
		size_t size = ftell (pFile) -8;
		rewind (pFile);
		fseek (pFile , 8 , SEEK_SET);// stupid header in bin file
		if(size >= RAMSIZE )		
			std::cout << "error: file length is invalid, abort boot \n";		
		else if(fread ( (void*)RAM, 1, size, pFile ) == size)
		{
			std::cout << "Loaded " << size << " instructions... \n";
			E = (size & 0xFFFFFFE0) + 32;		
			for(int i = E; i < RAMSIZE - 32; i+=32)
			{
				*((int*)&RAM[i]) = (i + 32)>>2;
			}
			memcpy((void*)F, (void*)(&RAM[E]), 32); 
			E = E >> 2;
			FA = E;
			N = E + 8;
		}
		fclose (pFile);	
		return size;
	}
	else 
		return 0;
}


void Core::Run(bool printState)
{
	bool cont = true;
	size_t cycles = 0;
	while(cont)
	{
		cont = Step(printState);
		cycles++;
	}
	std::cout << "------------------------------------------------------------------ \n";
	std::cout << "Result = " << A << "\n";
	std::cout << "Ran for = " << cycles << "cycles \n";
}

 //bool Core::Step(bool printState)
//{
//	int ALU = 0;
//	int _A = A;		
//	unsigned short PCPlusOne = PC + 1;
//	
//	unsigned short _N = N;	
//	/*int _F[FRAMESIZE];
//	std::copy(F, F + 8, _F);*/
//
//	unsigned char _arg_TOS = arg_TOS;
//	unsigned char _ret_TOS = ret_TOS;
//
//	char _DB = 0;
//	
//	//Current Env
//	unsigned short E_frame = E & FRAME_MASK;
//	unsigned char  E_cell = E & CELL_MASK;
//
//	//Current Frame Address 
//	unsigned char FA_cell = FA & CELL_MASK;
//	unsigned char FA_frame = FA & FRAME_MASK;
//	
//	unsigned int inst;
//	bool RAM_W = false;
//	
//	/////////////////////////////////////////////////////////
//	//Decode
//
//	
//	inst = RAM[PC];
//	char pattern = (inst & PTRN_MASK)>>PTRN_SHIFT;
//
//
//	if(printState)
//	{
//		printf("A = %X \t argS = %X \t retS = %X \t [E] = %X \n", A, argS[arg_TOS], retS[ret_TOS], F[FA_cell]);
//		printf("argTOS = %i \t retTOS = %i \t E = %i \n", arg_TOS, ret_TOS, E);
//		std::cout << "\n";
//		PrintInst(inst, PC);		
//	}
//
//
//
//	if(!A_stall && !F_stall)
//	{				
//		DB = inst & DB_MASK; // index of env var we need to fetch
//	}
//	
//	//pre compute in case we need to access a var from env
//	//DB = DB - FA_cell; // this would be the next DB if we need the next frame 
//	char Fidx = FA_cell - DB; // This is the index of the slot in F we are seeking 	
//	
//
//
//	if(link)
//	{
//		F[0] = F[0] | (link & F_LINK_MASK);
//		//F[0] = F[0] + F_CNT_INC;
//		link = 0;
//	}
//
//	if(!RAM_R && late_write_F)
//	{
//		F[FA_cell] = late_write_val;
//		late_write_F = false;
//	}
//	
//	
//	if(inst == END)
//		return false;
//	else if(pattern == PAT_LDI)// LDI
//	{
//		_A = inst & LIT_MASK;
//		_PC = PCPlusOne;
//	}
//	else 
//	{
//		if(pattern == PAT_ALU)
//		{
//			if(!A_stall && !F_stall)
//			{
//				// ALU //////////////////////////////////////////////////////////////////////
//				switch((inst & ALU_MASK)>>ALU_SHIFT)
//				{
//				case 0:// A
//					ALU = A;
//					break;
//				case 1:// argS
//					ALU = argS[arg_TOS];
//					break;
//				case 2:// ADD
//					ALU = A + argS[arg_TOS];
//					break;
//				case 3:// SUB
//					ALU = A - argS[arg_TOS];
//					break;
//				case 4:// MUL
//					ALU = A * argS[arg_TOS];
//					break;
//				case 5:// AND
//					ALU = A & argS[arg_TOS];
//					break;
//				case 6:// OR
//					ALU = A | argS[arg_TOS];
//					break;
//				case 7:// NOT
//					ALU = ~A;
//					break;
//				case 8:// XOR
//					ALU = A ^ argS[arg_TOS];
//					break;
//				case 9:// XNOR
//					ALU = ~(A ^ argS[arg_TOS]);
//					break;
//				case 10:// ASHR
//					ALU = (int)A >> argS[arg_TOS];
//					break;
//				case 11:// LSHR
//					ALU = (unsigned int)A >> argS[arg_TOS];
//					break;
//				case 12:// LSHL
//					ALU = A << argS[arg_TOS];
//					break;
//				case 13:// EQ
//					ALU = A == argS[arg_TOS];
//					break;
//				case 14:// NE
//					ALU = A != argS[arg_TOS];
//					break;
//				case 15:// GT
//					ALU = A > argS[arg_TOS];
//					break;		
//				case 16:// LT
//					ALU = A < argS[arg_TOS];
//					break;		
//				case 17:// GTE
//					ALU = A >= argS[arg_TOS];
//					break;		
//				case 18:// LTE
//					ALU = A <= argS[arg_TOS];
//					break;		
//				case 19:// ABV
//					ALU = ((unsigned int)A) > ((unsigned int)argS[arg_TOS]);
//					break;		
//				case 20:// BEL
//					ALU = ((unsigned int)A) < ((unsigned int)argS[arg_TOS]);
//					break;		
//				case 31:
//					ALU = MARK_VAL;
//					break;
//				default:
//					std::cout << "Bad instruction encoding in ALU \n";
//					break;
//				}////////////////////////////////////////////////////////////////////////////	
//			}
//		}
//
//		
//		switch(pattern)
//		{
//		case PAT_JMPCLOS:
//		case PAT_ALU:				
//			
//			if(!F_stall && !RAM_R)
//			{
//				// _A /////////////////////////////////////////////////////////////////////
//				switch((inst & A_MASK)>>A_SHIFT)
//				{
//				case 0:// no change
//					_A = ALU;
//					break;
//				case 1:// [E-n] -> A
//					if(Fidx <= 0)// need to fetch the next frame down the chain
//					{
//						if(!A_stall)
//							RAM_W = true;
//						RAM_R = true;
//						A_stall = true;
//						_FA = F[0] & F_LINK_MASK; // Set the next frame to jump to
//						DB = DB - FA_cell; 
//					}
//					else
//					{
//						_A = F[Fidx];	
//						A_stall = false;
//					}
//					break;
//				case 2:// pc -> A.c, E -> A.e
//					_A = argS[arg_TOS] == MARK_VAL ? (E << A_ENV_SHIFT) | PCPlusOne : A;
//					F[0] = F[0] + F_CNT_INC;
//					break;
//				case 3:
//					_A = inst & LIT_MASK;
//					break;
//				case 4:
//					_A = (E << A_ENV_SHIFT) | (inst & A_CODE_MASK);	
//					F[0] = F[0] + F_CNT_INC;
//					break;
//				case 5:
//					_A = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] : A;
//					break;
//				case 6:
//					_A = inst & A_CODE_MASK;
//					break;
//				case 7:
//					_A = ALU;//E_frame;
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in _A \n";
//					break;
//				}/////////////////////////////////////////////////////////////////////////////
//			}
//
//			if(!A_stall && !F_stall)
//			{
//
//				// _PC ///////////////////////////////////////////////////////////////////////
//				switch((inst & PC_MASK) >> PC_SHIFT)
//				{
//				case 0:// inc above	
//					_PC = PCPlusOne;
//					break;
//				case 1://
//					_PC = _A & A_CODE_MASK;
//					break;
//				case 2://
//					_PC = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] & A_CODE_MASK : PCPlusOne;
//					break;
//				case 3:
//					_PC = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] & A_CODE_MASK : _A & A_CODE_MASK;
//					break;	
//				case 4:
//					_PC = A != 0 ? inst & A_CODE_MASK : PCPlusOne;
//					break;
//				case 5:
//					_PC = retS[ret_TOS] & A_CODE_MASK;
//					break;
//				case 6:
//					_PC = inst & A_CODE_MASK;
//					break;
//				case 7:
//					_PC = PCPlusOne & (inst & DB_MASK);//??
//					break;
//				default:
//					std::cout << "Bad instruction encoding in _PC \n";
//					break;
//				}//////////////////////////////////////////////////////////////////////////////					
//				
//
//				if(inst & ALLOC_MASK)// _A -> N
//				{
//					_N = _A;
//				}
//				else if((N & FRAME_MASK) <= E_frame) 
//					_N = (E_frame) + 8; 
//
//
//
//			}
//			break;
//		
//		}
//
//		if(pattern == PAT_ALU)
//		{
//			if(!A_stall && !F_stall)
//			{			
//				// _E ///////////////////////////////////////////////////////////////////////
//				switch((inst & E_MASK) >> E_SHIFT)
//				{
//				case 0:// inc above		
//					_E = E;
//					break;
//				case 1://					
//					_E = N;
//					RAM_W = true;					
//					link = (_A & A_ENV_MASK) >> A_ENV_SHIFT;
//					break;
//				case 2://
//					if(argS[arg_TOS] == MARK_VAL)
//					{
//						_E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
//						_FA = _E;
//						RAM_W = true;
//						F_stall = true;
//						RAM_R = true;
//					}
//					else 
//						_E = E;
//					break;
//				case 3:
//					if(argS[arg_TOS] == MARK_VAL)
//					{
//						_E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
//						_FA = _E;
//						RAM_W = true;
//						F_stall = true;
//						RAM_R = true;
//					}
//					else
//					{
//						_E = (_A & A_ENV_MASK) >> A_ENV_SHIFT;
//						_FA = _E;
//						RAM_W = true;
//						F_stall = true;
//						RAM_R = true;
//						late_write_val = argS[arg_TOS];//should be able to use arg_TOS - 1 here instead
//						late_write_F = true; 
//
//						if(F[0] & F_CNT_MASK == 0) //drop current env
//							_N = E_frame;
//					}
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in _E \n";
//					break;
//				}////////////////////////////////////////////////////////////////////////////				
//			
//			}
//
//			if(RAM_W) 
//			{
//				std::copy(F, F + 8, RAM + (FA & FRAME_MASK));
//			}
//			
//				
//			if(!A_stall && (!RAM_R || !RAM_W))
//			{			
//				// E inc /////////////////////////////////////////////////////////////////////
//				switch((inst & E_INC_MASK) >> E_INC_SHIFT)
//				{
//				case 0:// no change				
//					break;
//				case 1://
//					if((_E & CELL_MASK) == 7)
//					{						
//						RAM_W = true;
//						_E = N;
//						link = E;
//					}
//					else _E = _E + 1;
//					break;
//				case 2: // conditionaly inc E 
//					if(argS[arg_TOS] != MARK_VAL)
//					{
//						if((_E & CELL_MASK) == 7)
//						{							
//							RAM_W = true;							
//							_E = N;							
//							link = E;
//						}
//						_E = _E + 1;
//					}
//					break;
//				case 3:
//					_E = _E - 1;
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in retSinc \n";
//					break;
//				}//////////////////////////////////////////////////////////////////////////////
//			}
//
//			if(!A_stall && !F_stall)
//			{
//
//				// F ////////////////////////////////////////////////////////////////////////
//				switch((inst & F_MASK) >> F_SHIFT)
//				{
//				case 0:// no change				
//					break;
//				case 1://
//					if(argS[arg_TOS] != MARK_VAL) 
//						F[_E & CELL_MASK] = argS[arg_TOS];
//					break;
//				case 2://
//					F[_E & CELL_MASK] = _A;
//					break;
//				case 3:
//					//
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in F \n";
//					break;
//				}/////////////////////////////////////////////////////////////////////////////
//			}
//		}
//		
//
//			if(!A_stall && !(RAM_R && RAM_W))
//			{
//				// argS inc ///////////////////////////////////////////////////////////////////////
//				switch((inst & ARGS_INC_MASK) >> ARGS_INC_SHIFT)
//				{
//				case 0:// no change				
//					_arg_TOS = arg_TOS;
//					break;
//				case 1://
//					_arg_TOS = arg_TOS + 1;
//					break;
//				case 2:	
//					//
//					break;
//				case 3:
//					_arg_TOS = arg_TOS -1;
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in argSinc \n";
//					break;
//				}//////////////////////////////////////////////////////////////////////////////////
//				_arg_TOS &= 0x1F; //we need the stacks to wrap
//	
//
//				if(inst & ARGS_MASK)// _A -> argS
//				{
//					argS[_arg_TOS] = _A;
//				}
//
//				// retS inc ///////////////////////////////////////////////////////////////////////
//				switch((inst & RETS_INC_MASK) >> RETS_INC_SHIFT)
//				{
//				case 0:// no change	
//					_ret_TOS = ret_TOS;
//					break;
//				case 1://
//					_ret_TOS = ret_TOS + 1;
//					break;
//				case 2:	
//					if(argS[arg_TOS] == MARK_VAL)
//						_ret_TOS = ret_TOS - 1;
//					break;
//				case 3:
//					_ret_TOS = ret_TOS -1;
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in retSinc \n";
//					break;
//				}//////////////////////////////////////////////////////////////////////////////////
//				_ret_TOS &= 0x1F; //we need the stacks to wrap
//
//				// retS ///////////////////////////////////////////////////////////////////////
//				switch((inst & RETS_MASK) >> RETS_SHIFT)
//				{
//				case 0:// no change				
//					break;
//				case 1://
//					retS[_ret_TOS] = (E << A_ENV_SHIFT) | PCPlusOne;
//					break;
//				case 2://
//					retS[_ret_TOS] = PCPlusOne;
//					break;
//				case 3:
//					retS[_ret_TOS] = _A;
//					break;			
//				default:
//					std::cout << "Bad instruction encoding in _retS \n";
//					break;
//				}///////////////////////////////////////////////////////////////////////////////
//
//			}
//		
//		}
//	
//
//
//	
//	if(RAM_R && ! RAM_W) 
//	{
//		std::copy(RAM + (_FA & FRAME_MASK), RAM + (_FA & FRAME_MASK) + 8, F);
//		FA = _FA;
//		RAM_R = false;	
//		F_stall = false;
//	}
//
//	if(!A_stall && !F_stall)			
//	{
//		FA = _E;
//		A = _A;
//		PC = _PC;
//		E = _E;	
//		N = _N;
//		arg_TOS = _arg_TOS;
//		ret_TOS = _ret_TOS;
//	}
//
//	
//
//
//
//
//	return true;
//}

bool Core::Step(bool printState)
{
		
	if(!A_stall && !F_stall)			
	{				
		DB = RAM[PC+1];  // index of env var we need to fetch
		FA = E;
	}
	
	short PCPlusOne = PC + 1;	
	//Current Env
	short E_frame = E & FRAME_MASK;
	char  E_cell = E & CELL_MASK;
	//Current Frame Address 
	short FA_frame = FA & FRAME_MASK;
	char FA_cell = FA & CELL_MASK;
	
	
	char inst = RAM[PC];
	
	
	if(RAM_R && !RAM_W) 
	{
		memcpy((void*)F, (void*)(&RAM[(FA & FRAME_MASK)<<2]), 32); 			
		RAM_R = false;	
		F_stall = false;
	}
	else if(RAM_W)
		RAM_W = false;



	arg_TOS &= 0x1F;
	ret_TOS &= 0x1F;

	
	if(printState)
	{
		printf("A = %X \t argS = %X \t retS = %X \t [E] = %X \n", A, argS[arg_TOS], retS[ret_TOS], F[FA_cell]);
		printf("argTOS = %i \t retTOS = %i \t E = %i \n", arg_TOS, ret_TOS, E);
		std::cout << "\n";
		PrintInst(&RAM[PC], PC);		
	}

	
	//a value we couldnt write to F untill we loaded it from ram
	if(!RAM_R && late_write_F)
	{
		F[FA_cell] = late_write_val;
		late_write_F = false;
	}
	
	//stall to load a full 4byte immedate
	if(loadWord)
	{
		A = *((int*)(&RAM[PC]));
		PC = PC+4;
		loadWord = false;
	}
	else
	{
		char op = inst & OP_MASK;
		switch(op)
		{
			case NOP://////////////////////////////////////////////////////////////////////////
				PC = PCPlusOne;			
				break;
			case LDI:
			case LDIP://////////////////////////////////////////////////////////////////////////
			{
				char cnt = inst & DATA_CNT_MASK;
				switch(cnt)
				{
				case 0:
					loadWord = true;
					PC = PCPlusOne;
					break;
				case 1:
					//A = ((int)(RAM[PC+1] << 24) >> 24);
					A = (unsigned char)RAM[PC+1];
					PC = PC+2;
					break;			
				case 2:
					//A = ((int)(RAM[PC+1] << 16)|((int)(RAM[PC+2] << 24)) >> 16);
					A = ((unsigned char)RAM[PC+1] | ((unsigned char)RAM[PC+2]<<8));
					PC = PC+3;
					break;
				case 3:
					//A = ((int)(RAM[PC+1]<<8)|(int)(RAM[PC+2]<<16)|((int)(RAM[PC+3]<<24))>>8);
					A = ((unsigned char)RAM[PC+1] | ((unsigned char)RAM[PC+2]<<8) | ((unsigned char)RAM[PC+3]<<16));
					PC = PC+4;
					break;
				}

				if(op == LDIP)//PUSH
				{
					arg_TOS = (arg_TOS + 1) & 0x1F;
					argS[arg_TOS] = A;										
				}
				
				break;				
			}
			case CLOS://////////////////////////////////////////////////////////////////////////
				A = (E << A_ENV_SHIFT) | RAM[PC+1] | (RAM[PC+2]<<8);
				PC = PC + 3;
				break;	
			case IF://////////////////////////////////////////////////////////////////////////
				if(A)
					PC = RAM[PC+1] | (RAM[PC+2]<<8);
				else
					PC = PC + 3;
				break;
			case CALL://////////////////////////////////////////////////////////////////////////
				PC = RAM[PC+1] | (RAM[PC+2]<<8);
				ret_TOS = (ret_TOS + 1) & 0x1F;
				retS[ret_TOS] = PCPlusOne;
				break;
			case JUMP://////////////////////////////////////////////////////////////////////////
				PC = RAM[PC+1] | (RAM[PC+2]<<8);			
				break;
			case RET://////////////////////////////////////////////////////////////////////////
				PC = retS[ret_TOS];
				ret_TOS--;
				break;
			case ACC:
			case ACCP:{
				char Fidx = FA_cell - DB; // This is the index of the slot in F we are seeking
				if(Fidx <= 0)// need to fetch the next frame down the chain
				{
					if(!A_stall)
					{
						memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32); 						
						RAM_W = true;
					}
					RAM_R = true;
					A_stall = true;
					FA = F[0] & F_LINK_MASK; // Set the next frame to jump to
					DB = DB - FA_cell; 
				}
				else
				{
					A = F[Fidx];	
					A_stall = false;
					PC = PC + 2;
					if(op == ACCP)//PUSH
					{
						arg_TOS = (arg_TOS + 1) & 0x1F;
						argS[arg_TOS] = A;
					}
				}
				
				break;	
					  }
			case APPT:
			case APP:
				if(op == APP)
				{
					ret_TOS = (ret_TOS + 1) & 0x1F;
					retS[ret_TOS] = (E << 16) | PCPlusOne;					
				}
				PC = A & A_CODE_MASK;								
				E = N;
				E++;
				memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);
				RAM_W = true;
				F[0] = (A & A_ENV_MASK) >> A_ENV_SHIFT;	
				F[E & CELL_MASK] = argS[arg_TOS];
				arg_TOS--;							
				break;				
			case PUSH:
				arg_TOS = (arg_TOS + 1) & 0x1F;
				argS[arg_TOS] = A;				
				PC = PCPlusOne;
				break;
			case MARK:
				arg_TOS = (arg_TOS + 1) & 0x1F;
				argS[arg_TOS] = MARK_VAL;
				PC = PCPlusOne;
				break;
			case GRAB:
				if(argS[arg_TOS] == MARK_VAL)//build clos and pop return stack
				{
					A = (E << A_ENV_SHIFT) | PCPlusOne;
					F[0] = F[0] + F_CNT_INC;//refcnt+
					PC =  retS[ret_TOS] & A_CODE_MASK;
					ret_TOS--;
					memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
					RAM_W = true;
					E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;					
					FA = E;					
					F_stall = true;
					RAM_R = true;
				}
				else //grab an argument off stack and add to env 
				{					
					if((E & CELL_MASK) == 7)
					{
						memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);;//write
						RAM_W = true;
						F[0] = F[0] | E & F_LINK_MASK;
						E = N;						
					}
					E++;
					F[E & CELL_MASK] = argS[arg_TOS];
					PC = PCPlusOne;
				}
				arg_TOS--;
				break;
			case RETC:
				//if there are no refs to the current clos, we dont need to save
				if((F[0] & F_CNT_MASK) == 0) 
				{
					N = E_frame;
				}
				else//otherwise save back to ram
				{
					memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
					RAM_W = true;
				}

				if(argS[arg_TOS] == MARK_VAL)
				{
					PC = retS[ret_TOS] & A_CODE_MASK;
					E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
					ret_TOS--;
					FA = E;
					memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
					RAM_W = true;
					F_stall = true;
					RAM_R = true;
				}
				else
				{					
					//pop clos
					PC = A & A_CODE_MASK;
					E = (A & A_ENV_MASK) >> A_ENV_SHIFT;
					if((E & CELL_MASK) == 7)
					{
						F[0] = F[0] | E & F_LINK_MASK;
						E = N;						
						E++;
						F[E & CELL_MASK] = argS[arg_TOS];
					}
					else
					{
						E++;
						FA = E;					
						F_stall = true;
						RAM_R = true;
						late_write_val = argS[arg_TOS];//should be able to use arg_TOS - 1 here instead
						late_write_F = true; 
					}
				}				
				arg_TOS--;
				break;
			case LET:
				if((E & CELL_MASK) == 7)
				{
					F[0] = F[0] | E & F_LINK_MASK;
					E = N;										
				}
				E++;
				F[E & CELL_MASK] = A;
				PC = PCPlusOne;
				break;
			case ELET:
				E--;
				PC = PCPlusOne;
				break;
			case TEMP:
				if((E & CELL_MASK) == 7)
				{
					F[0] = F[0] | E & F_LINK_MASK;
					E = N;										
				}
				E++;
				F[E & CELL_MASK] = MARK_VAL;
				PC = PCPlusOne;
				break;
			case UPDT:
				F[E & CELL_MASK] = A;				
				PC = PCPlusOne;
				break;
			case END:
				return false;
				break;
			case ADD:
				A = A + argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case SUB:
				A = A - argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case MUL:
				A = A * argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case AND:
				A = A & argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case OR:
				A = A | argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case NOT:
				A = ~A;
				PC = PCPlusOne;
				break;
			case XOR:
				A = A ^ argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case XNOR:
				A = ~(A ^ argS[arg_TOS]);
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case ASHR:
				A = (int)A >> argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case LSHR:
				A = (unsigned int)A >> argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case LSHL:
				A = A << argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case EQ:
				A = A == argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case NE:
				A = A != argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case GT:
				A = A > argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case LT:
				A = A < argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case GTE:
				A = A >= argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case LTE:
				A = A <= argS[arg_TOS];
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case ABV:
				A = ((unsigned int)A) > ((unsigned int)argS[arg_TOS]);
				PC = PCPlusOne;
				arg_TOS--;
				break;
			case BEL:
				A = ((unsigned int)A) < ((unsigned int)argS[arg_TOS]);
				PC = PCPlusOne;
				arg_TOS--;
				break;
			default:
				std::cout << "Bad instruction encoding! \n";
				break;		

		}	




	}
	return true;
}






void Core::Disassemble(size_t cnt)
{
	std::cout << "================= RAM Dissassembly ================ \n";
	for(size_t i = 0; i < cnt;)
	{		
		i += PrintInst(&RAM[i], i);		
	}
}