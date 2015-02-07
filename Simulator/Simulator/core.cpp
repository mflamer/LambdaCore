#include "core.h"

int PrintInst(unsigned char inst[], unsigned int PC)
{
	int result = 1;
	int skip = 1;
	char op = inst[0] & OP_MASK;
	char n = inst[0] & DATA_CNT_MASK;
	int data = 0;
	switch(n)
	{
	case 0:
		data = inst[1];		
		skip = 2;
		break;
	case 1: 
		data = (inst[1] | (inst[2]<<8));		
		skip = 3;
		break;		   
	case 2:		
		data = (inst[1] | (inst[2]<<8) | (inst[3]<<16));		
		skip = 4;
		break;	
	case 3:
		data = (inst[1] | (inst[2]<<8) | (inst[3]<<16) | (inst[4]<<24));
		skip = 5;
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
	newFrame = false;
	loadWord = false; 


	//Stats
	newFrames = 0;
	disposedFrames = 0;

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
			//init root frame
			N = F[0] & F_LINK_MASK;
			F[0] = 0;
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
	std::cout << "SpawnedFrames = " << newFrames << "\n";
	std::cout << "DisposedFrames = " << disposedFrames << "\n";
}



bool Core::Step(bool printState)
{
	
		
	if(!F_stall && !A_stall)
		FA = E;
	
	short PCPlusOne = PC + 1;
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

	//setup a fetched frame
	//a value we couldnt write to F untill we loaded it from ram
	if(!RAM_R && late_write_F)
	{
		if(newFrame)
		{
			N = F[0] & F_LINK_MASK;
			F[0] = E & F_LINK_MASK;
			newFrame = false;
		}
		FA++;
		E = FA;
		F[FA & CELL_MASK] = late_write_val;
		late_write_F = false;
	}

	if(!A_stall && !F_stall)			
	{		
		DB = RAM[PC+1];  // index of env var we need to fetch		
	}

	//Current Env
	short E_frame = E & FRAME_MASK;
	char  E_cell = E & CELL_MASK;
	//Current Frame Address 
	short FA_frame = FA & FRAME_MASK;
	char FA_cell = FA & CELL_MASK;

	
	if(printState)
	{
		printf("A=%i  A.e=%i  A.c=%i \t argS_TOS=%i  argS=%i  argS.e=%i  argS.c=%i \n", A, (A & A_ENV_MASK)>>A_ENV_SHIFT , A & A_CODE_MASK, arg_TOS, argS[arg_TOS], (argS[arg_TOS] & A_ENV_MASK)>>A_ENV_SHIFT, argS[arg_TOS] & A_CODE_MASK);
		printf("argS_TOS=%i retS=%i  retS.e=%i  retS.c=%i \n", ret_TOS, retS[ret_TOS], (retS[ret_TOS] & A_ENV_MASK)>>A_ENV_SHIFT, (retS[ret_TOS] & A_CODE_MASK));
		printf("FA=%i  E=%i  E_cell=%i  N=%i  DB=%i \n", FA, E, E_cell, N, DB);
		printf("F = [cnt = %i link = %i, %i, %i, %i, %i, %i, %i, %i] \n", (F[0] & F_CNT_MASK)>>F_CNT_SHIFT, F[0] & F_LINK_MASK, F[1], F[2], F[3], F[4], F[5], F[6], F[7]);
		
		std::cout << "\n";
		if(F_stall) 
			std::cout << "F_stall \n";
		else if(A_stall) 
			std::cout << "A_stall \n";
		else	
			PrintInst(&RAM[PC], PC);		
	}




	
	//stall to load a full 4byte immedate
	if(loadWord)
	{
		char op = inst & OP_MASK;
		A = *((int*)(&RAM[PC+1]));
		if(op == LDIP)//PUSH
		{
			arg_TOS = (arg_TOS + 1) & 0x1F;
			argS[arg_TOS] = A;										
		}
		PC = PC+5;
		loadWord = false;
	}
	else if(!F_stall)
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
					A = RAM[PC+1];
					PC = PC+2;
					break;			
				case 1:					
					A = (RAM[PC+1] | (RAM[PC+2]<<8));
					PC = PC+3;
					break;
				case 2:					
					A = (RAM[PC+1] | (RAM[PC+2]<<8) | (RAM[PC+3]<<16));
					PC = PC+4;
					break;
				case 3:
					loadWord = true;
					break;
				}

				if(op == LDIP && cnt != 3)//PUSH
				{
					arg_TOS = (arg_TOS + 1) & 0x1F;
					argS[arg_TOS] = A;										
				}
				
				break;				
			}
			case CLOS://////////////////////////////////////////////////////////////////////////
				A = (E << A_ENV_SHIFT) | (RAM[PC+1]) | (RAM[PC+2]<<8);
				F[0] = F[0] + F_CNT_INC;//refcnt+
				PC = PC + 3;
				break;	
			case IF://////////////////////////////////////////////////////////////////////////
				if(A)
					PC = (RAM[PC+1]) | (RAM[PC+2]<<8);
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
				char Fidx = (FA & CELL_MASK) - DB; // This is the index of the slot in F we are seeking
				if(Fidx <= 0)// need to fetch the next frame down the chain
				{
					if(!A_stall)
					{
						memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32); 						
						RAM_W = true;
						F_stall = true;
					}
					RAM_R = true;
					A_stall = true;	
					DB = DB - (FA & CELL_MASK);
					FA = F[0] & F_LINK_MASK; // Set the next frame to jump to					 
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
					//we are done, load the current frame
					if(FA != E)
					{
						FA = E;
						F_stall = true;
						RAM_R = true;
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
					NewFrame(argS[arg_TOS], false);
				}
				else //grab an argument off stack and add to env 
				{					
					if((E & CELL_MASK) == 7)//need to get the next available frame
						NewFrame(argS[arg_TOS]);
					else//still space in this frame
					{
						E++;
						F[E & CELL_MASK] = argS[arg_TOS];						
					}
				}					
				PC = A & A_CODE_MASK;				
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
					if((E & CELL_MASK) == 7)//need to get the next available frame
						NewFrame(argS[arg_TOS]);
					else//still space in this frame
					{
						E++;
						F[E & CELL_MASK] = argS[arg_TOS];
						PC = PCPlusOne;
					}
				}
				arg_TOS--;
				break;
			case RETC:
			{
				//if there are no refs to the current clos, we dont need to save
				//add this frame to the head of the avail chain
				bool saveThisFrame = true;
				if((F[0] & F_CNT_MASK) == 0) /// CHECK if nothing in E, (E_cell == 0)? I think we still might need to keep it
				{
					F[0] = N;
					N = E_frame;
					saveThisFrame = false;
					memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
					RAM_W = true;
					F_stall = true;

					//Stats
					disposedFrames++;
				}			

				if(argS[arg_TOS] == MARK_VAL)
				{
					if(saveThisFrame)
					{
						memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
						RAM_W = true;
						F_stall = true;
					}
					PC = retS[ret_TOS] & A_CODE_MASK;
					E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
					ret_TOS--;
					FA = E;					
					RAM_R = true;
				}
				else
				{					
					//pop clos
					PC = A & A_CODE_MASK;
					E = (A & A_ENV_MASK) >> A_ENV_SHIFT;
					NewFrame(argS[arg_TOS]);
				}	

				arg_TOS--;
				break;
			}
			case LET:
				if((E & CELL_MASK) == 7)				
					NewFrame(A);				
				else
				{
					E++;
					F[E & CELL_MASK] = A;
				}
				PC = PCPlusOne;
				break;
			case ELET:
				//check if we need to pop back to prior frame? 
				if(E_cell == 1)
				{
					E = F[0] & F_LINK_MASK;
					bool saveThisFrame = true;
					if((F[0] & F_CNT_MASK) == 0) /// CHECK if nothing in E, (E_cell == 0)? I think we still might need to keep it
					{
						F[0] = N;
						N = E_frame;
						saveThisFrame = false;

						//Stats
						disposedFrames++;
					}

					if(saveThisFrame)
					{
						memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
						RAM_W = true;
						F_stall = true;				
					}
					
					FA = E;					
					RAM_R = true;
				}
				else
				{
					E--;
				}
				PC = PCPlusOne;
				break;
			case TEMP:
				if((E & CELL_MASK) == 7)
					NewFrame(MARK_VAL);
				else
				{
					E++;
					F[E & CELL_MASK] = MARK_VAL;
				}
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


void	Core::NewFrame(int firstParam, bool incRef)
{
	if(incRef)
		F[0] = F[0] + F_CNT_INC;//refcnt+
	memcpy((void*)(&RAM[(FA & FRAME_MASK)<<2]), (void*)F, 32);//write
	RAM_W = true;						
	FA = N;						
	F_stall = true;
	RAM_R = true;
	late_write_F = true;
	late_write_val = firstParam;
	newFrame = true;

	//////Stats
	newFrames++;
}




void Core::Disassemble(size_t cnt)
{
	std::cout << "================= RAM Dissassembly ================ \n";
	for(size_t i = 0; i < cnt;)
	{		
		i += PrintInst(&RAM[i], i);		
	}
}