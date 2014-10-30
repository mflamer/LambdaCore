#include "core.h"

void PrintInst(unsigned int inst, unsigned short PC)
{
	char pattern = (inst & PTRN_MASK)>>PTRN_SHIFT;
	switch(pattern)
	{
	case PAT_LDI:
		std::cout << PC << ": \t" << "LDI " << (inst & LIT_MASK) << "\n";
		break;
	case PAT_JMPCLOS:
		switch(inst & 0xFFFF0000)
		{
			case CLOS:
				std::cout << PC << ": \t" << "CLOS " << (inst & A_CODE_MASK) << "\n";			
				break;	
			case IF:
				std::cout << PC << ": \t" << "IF " << (inst & A_CODE_MASK) << "\n";			
				break;
			default:
				std::cout << "Bad instruction encoding! \n";
				break;
		}
		break;
	case PAT_ALU:
		if(inst == END)
				std::cout << PC << ": \t" << "END \n";
		else
		{
			switch(inst & 0xFFFFFFE0)
			{
				case ACC:
					std::cout << PC << ": \t" << "ACC " << (inst & DB_MASK) << "\n";
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
				case RET:
					std::cout << PC << ": \t" << "RET \n";
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
				case ADD:
					std::cout << PC << ": \t" << "ADD \n";
					break;
				case SUB:
					std::cout << PC << ": \t" << "SUB \n";
					break;
				default:
					std::cout << "Bad instruction encoding! \n";
					break;
			}
		}
		break;
		default:
			std::cout << "Bad instruction encoding! \n";
			break;
	}
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
	_PC = 0;
	E = 0;
	_E = 0;
	N = 0;
	arg_TOS = 0xFF;
	ret_TOS = 0xFF;
	DB = 0;
	FA = 0;
	_FA = 0;
	RAM_R = false;
	A_stall = false;
	F_stall = false;

	link = 0;
	late_write_F = false;
	late_write_val = 0;


}


void Core::LoadRAM(std::string fileName)
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
		if(size >= RAMSIZE * 4 || size % 4 != 0)		
			std::cout << "error: file length is invalid, abort boot \n";		
		else if(fread ( (void*)RAM, 4, size / 4, pFile ) == size / 4)
		{
			std::cout << "Loaded " << size / 4 << " instructions... \n";
			N = ((size / 4) & 0xFFFFFFF8) + 8;
			E = N;
			FA = E;
			N += 8;
		}
		fclose (pFile);		
	}
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

bool Core::Step(bool printState)
{
	int ALU = 0;
	int _A = A;		
	unsigned short PCPlusOne = PC + 1;
	
	unsigned short _N = N;	
	/*int _F[FRAMESIZE];
	std::copy(F, F + 8, _F);*/

	unsigned char _arg_TOS = arg_TOS;
	unsigned char _ret_TOS = ret_TOS;

	char _DB = 0;
	
	//Current Env
	unsigned short E_frame = E & FRAME_MASK;
	unsigned char  E_cell = E & CELL_MASK;

	//Current Frame Address 
	unsigned char FA_cell = FA & CELL_MASK;
	unsigned char FA_frame = FA & FRAME_MASK;
	
	unsigned int inst;
	bool RAM_W = false;
	
	/////////////////////////////////////////////////////////
	//Decode

	
	inst = RAM[PC];
	char pattern = (inst & PTRN_MASK)>>PTRN_SHIFT;


	if(printState)
	{
		printf("A = %X \t argS = %X \t retS = %X \t [E] = %X \n", A, argS[arg_TOS], retS[ret_TOS], F[FA_cell]);
		printf("argTOS = %i \t retTOS = %i \t E = %i \n", arg_TOS, ret_TOS, E);
		std::cout << "\n";
		PrintInst(inst, PC);		
	}



	if(!A_stall && !F_stall)
	{				
		DB = inst & DB_MASK; // index of env var we need to fetch
	}
	
	//pre compute in case we need to access a var from env
	//DB = DB - FA_cell; // this would be the next DB if we need the next frame 
	char Fidx = FA_cell - DB; // This is the index of the slot in F we are seeking 	
	


	if(link)
	{
		F[0] = link;
		link = 0;
	}

	if(!RAM_R && late_write_F)
	{
		F[FA_cell] = late_write_val;
		late_write_F = false;
	}
	
	
	if(inst == END)
		return false;
	else if(pattern == PAT_LDI)// LDI
	{
		_A = inst & LIT_MASK;
		_PC = PCPlusOne;
	}
	else 
	{
		if(pattern == PAT_ALU)
		{
			if(!A_stall && !F_stall)
			{
				// ALU //////////////////////////////////////////////////////////////////////
				switch((inst & ALU_MASK)>>ALU_SHIFT)
				{
				case 0:// A
					ALU = A;
					break;
				case 1:// argS
					ALU = argS[arg_TOS];
					break;
				case 2:// ADD
					ALU = A + argS[arg_TOS];
					break;
				case 3:// SUB
					ALU = A - argS[arg_TOS];
					break;
				case 4:// AND
					ALU = A & argS[arg_TOS];
					break;
				case 5:// OR
					ALU = A | argS[arg_TOS];
					break;
				case 6:// XOR
					ALU = A ^ argS[arg_TOS];
					break;
				case 7:// NOT
					ALU = ~A;
					break;
				case 31:
					ALU = MARK_VAL;
					break;

				default:
					std::cout << "Bad instruction encoding in ALU \n";
					break;
				}////////////////////////////////////////////////////////////////////////////	
			}
		}

		
		switch(pattern)
		{
		case PAT_JMPCLOS:
		case PAT_ALU:				
			
			if(!F_stall && !RAM_R)
			{
				// _A /////////////////////////////////////////////////////////////////////
				switch((inst & A_MASK)>>A_SHIFT)
				{
				case 0:// no change
					_A = A;
					break;
				case 1:// [E-n] -> A
					if(Fidx <= 0)// need to fetch the next frame down the chain
					{
						if(!A_stall)
							RAM_W = true;
						RAM_R = true;
						A_stall = true;
						_FA = F[0]; // Set the next frame to jump to
						DB = DB - FA_cell; 
					}
					else
					{
						_A = F[Fidx];	
						A_stall = false;
					}
					break;
				case 2:// pc -> A.c, E -> A.e
					_A = argS[arg_TOS] == MARK_VAL ? (E << A_ENV_SHIFT) | PCPlusOne : A;
					break;
				case 3:
					_A = inst & LIT_MASK;
					break;
				case 4:
					_A = (E << A_ENV_SHIFT) | (inst & A_CODE_MASK);						
					break;
				case 5:
					_A = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] : A;
					break;
				case 6:
					_A = inst & A_CODE_MASK;
					break;
				case 7:
					_A = ALU;
					break;			
				default:
					std::cout << "Bad instruction encoding in _A \n";
					break;
				}/////////////////////////////////////////////////////////////////////////////
			}

			if(!A_stall && !F_stall)
			{

				// _PC ///////////////////////////////////////////////////////////////////////
				switch((inst & PC_MASK) >> PC_SHIFT)
				{
				case 0:// inc above	
					_PC = PCPlusOne;
					break;
				case 1://
					_PC = _A & A_CODE_MASK;
					break;
				case 2://
					_PC = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] & A_CODE_MASK : PCPlusOne;
					break;
				case 3:
					_PC = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] & A_CODE_MASK : _A & A_CODE_MASK;
					break;	
				case 4:
					_PC = A != 0 ? inst & A_CODE_MASK : PCPlusOne;
					break;
				default:
					std::cout << "Bad instruction encoding in _PC \n";
					break;
				}//////////////////////////////////////////////////////////////////////////////					
				

				if(inst & ALLOC_MASK)// _A -> N
				{
					_N = _A;
				}
				else if((N & FRAME_MASK) <= E_frame) 
					_N = (E_frame) + 8; 



			}
			break;
		
		}

		if(!A_stall && !F_stall)
		{
			
			if(pattern == PAT_ALU)
			{
			// _E ///////////////////////////////////////////////////////////////////////
				switch((inst & E_MASK) >> E_SHIFT)
				{
				case 0:// inc above		
					_E = E;
					break;
				case 1://
					_E = N;						
					RAM_W = true;						
					link = (_A & A_ENV_MASK) >> A_ENV_SHIFT;
					break;
				case 2://
					if(argS[arg_TOS] == MARK_VAL)
					{
						_E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
						_FA = _E;
						RAM_W = true;
						F_stall = true;
						RAM_R = true;
					}
					else 
						_E = E;
					break;
				case 3:
					if(argS[arg_TOS] == MARK_VAL)
					{
						_E = (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT;
						_FA = _E;
						RAM_W = true;
						F_stall = true;
						RAM_R = true;
					}
					else
					{
						_E = (_A & A_ENV_MASK) >> A_ENV_SHIFT;
						_FA = _E;
						RAM_W = true;
						F_stall = true;
						RAM_R = true;
						late_write_val = argS[arg_TOS];//should be able to use arg_TOS - 1 here instead
						late_write_F = true; 
					}
					break;			
				default:
					std::cout << "Bad instruction encoding in _E \n";
					break;
				}////////////////////////////////////////////////////////////////////////////	
			}
			
		}

		if(RAM_W) 
		{
			std::copy(F, F + 8, RAM + (FA & FRAME_MASK));
		}

			
				
		if(!A_stall && (!RAM_R || !RAM_W))
		{					
			
			// E inc /////////////////////////////////////////////////////////////////////
			switch((inst & E_INC_MASK) >> E_INC_SHIFT)
			{
			case 0:// no change				
				break;
			case 1://
				if((_E & CELL_MASK) == 7)
				{						
					RAM_W = true;
					_E = N;
					link = E;
				}
				else _E = _E + 1;
				break;
			case 2: // conditionaly inc E 
				if(argS[arg_TOS] != MARK_VAL)
				{
					if((_E & CELL_MASK) == 7)
					{							
						RAM_W = true;							
						_E = N;							
						link = E;
					}
					_E = _E + 1;
				}
				break;
			case 3:
				_E = _E - 1;
				break;			
			default:
				std::cout << "Bad instruction encoding in retSinc \n";
				break;
			}//////////////////////////////////////////////////////////////////////////////
		}

		if(!A_stall && !F_stall)
		{

			// F ////////////////////////////////////////////////////////////////////////
			switch((inst & F_MASK) >> F_SHIFT)
			{
			case 0:// no change				
				break;
			case 1://
				if(argS[arg_TOS] != MARK_VAL) 
					F[_E & CELL_MASK] = argS[arg_TOS];
				break;
			case 2://
				F[_E & CELL_MASK] = _A;
				break;
			case 3:
				//
				break;			
			default:
				std::cout << "Bad instruction encoding in F \n";
				break;
			}/////////////////////////////////////////////////////////////////////////////
		}

		if(!A_stall && !(RAM_R && RAM_W))
		{
			// argS inc ///////////////////////////////////////////////////////////////////////
			switch((inst & ARGS_INC_MASK) >> ARGS_INC_SHIFT)
			{
			case 0:// no change				
				_arg_TOS = arg_TOS;
				break;
			case 1://
				_arg_TOS = arg_TOS + 1;
				break;
			case 2:	
				//
				break;
			case 3:
				_arg_TOS = arg_TOS -1;
				break;			
			default:
				std::cout << "Bad instruction encoding in argSinc \n";
				break;
			}//////////////////////////////////////////////////////////////////////////////////
			_arg_TOS &= 0x1F; //we need the stacks to wrap
	

			if(inst & ARGS_MASK)// _A -> argS
			{
				argS[_arg_TOS] = _A;
			}

			// retS inc ///////////////////////////////////////////////////////////////////////
			switch((inst & RETS_INC_MASK) >> RETS_INC_SHIFT)
			{
			case 0:// no change	
				_ret_TOS = ret_TOS;
				break;
			case 1://
				_ret_TOS = ret_TOS + 1;
				break;
			case 2:	
				if(argS[arg_TOS] == MARK_VAL)
					_ret_TOS = ret_TOS - 1;
				break;
			case 3:
				_ret_TOS = ret_TOS -1;
				break;			
			default:
				std::cout << "Bad instruction encoding in retSinc \n";
				break;
			}//////////////////////////////////////////////////////////////////////////////////
			_ret_TOS &= 0x1F; //we need the stacks to wrap

			// retS ///////////////////////////////////////////////////////////////////////
			switch((inst & RETS_MASK) >> RETS_SHIFT)
			{
			case 0:// no change				
				break;
			case 1://
				retS[_ret_TOS] = (E << A_ENV_SHIFT) | PCPlusOne;
				break;
			case 2://
				retS[_ret_TOS] = PCPlusOne;
				break;
			case 3:
				retS[_ret_TOS] = _A;
				break;			
			default:
				std::cout << "Bad instruction encoding in _retS \n";
				break;
			}///////////////////////////////////////////////////////////////////////////////

		}
			
		
		
	}


	
	if(RAM_R && ! RAM_W) 
	{
		std::copy(RAM + (_FA & FRAME_MASK), RAM + (_FA & FRAME_MASK) + 8, F);
		FA = _FA;
		RAM_R = false;	
		F_stall = false;
	}

	if(!A_stall && !F_stall)			
	{
		FA = _E;
		A = _A;
		PC = _PC;
		E = _E;	
		N = _N;
		arg_TOS = _arg_TOS;
		ret_TOS = _ret_TOS;
	}

	




	return true;
}








void Core::Disassemble()
{
	std::cout << "================= RAM Dissassembly ================ \n";
	while(RAM[PC] != INITVAL)
	{
		unsigned int inst = RAM[PC];
		PrintInst(inst, PC);
		PC++;
	}
}