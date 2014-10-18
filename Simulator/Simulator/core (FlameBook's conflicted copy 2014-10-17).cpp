#include "core.h"



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
	RAMReading = false;
	A_stall = false;
	F_stall = false;

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


void Core::Run()
{
	size_t cycles = 0;
	while(RAM[PC] != INITVAL)
	{
		Step();
		cycles++;
	}
	std::cout << "------------------------------------------------------------------ \n";
	std::cout << "Result = " << A << "\n";
	std::cout << "Ran for = " << cycles << "cycles \n";
}

void Core::Step()
{
	int ALU = 0;
	int _A = A;					
	unsigned short _PC;		
	unsigned short _E = E;		
	/*unsigned short _N = 0;*/	
	char _DB = 0;
	short _FA = 0;

	//Current Env
	unsigned short E_frame = E & FRAME_MASK;
	unsigned char  E_cell = E & CELL_MASK;

	//Current Frame Address 
	unsigned char FA_cell = FA & CELL_MASK;
	unsigned char FA_frame = FA & FRAME_MASK;
	
	unsigned int inst;

	


	/////////////////////////////////////////////////////////
	//Decode

	inst = RAM[PC];
	char pattern = (inst & PTRN_MASK)>>PTRN_SHIFT;


	if(!A_stall && !F_stall)
	{		
		PC++; //we need this to happen right away by default
		_PC = PC;
		DB = inst & DB_MASK; // index of env var we need to fetch
	}
	
	//pre compute in case we need to access a var from env
	_DB = DB - FA_cell; // this would be the next DB if we need the next frame 
	char Fidx = FA_cell - DB; // This is the index of the slot in F we are seeking 	
	

	
	
	if(pattern == PAT_LDI)// LDI
	{
		_A = inst & LIT_MASK;
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
			
			if(!F_stall)
			{
				// _A /////////////////////////////////////////////////////////////////////
				switch((inst & A_MASK)>>A_SHIFT)
				{
				case 0:// no change
					_A = A;
					break;
				case 1:// [E-n] -> A
					if(_DB > 0)// need to fetch the next frame down the chain
					{
						RAMReading = true;
						A_stall = true;
						_FA = F[0]; // Set the next frame to jump to
					}
					else
					{
						_A = F[Fidx];	
						A_stall = false;
					}
					break;
				case 2:// pc -> A.c, E -> A.e
					_A = argS[arg_TOS] == MARK_VAL ? (E << A_ENV_SHIFT) | PC : A;
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
					break;
				case 1://
					_PC = _A & A_CODE_MASK;
					break;
				case 2://
					_PC = argS[arg_TOS] == MARK_VAL ? retS[ret_TOS] & A_CODE_MASK : PC;
					break;
				case 3:
					_PC = inst & A_CODE_MASK;
					break;			
				default:
					std::cout << "Bad instruction encoding in _PC \n";
					break;
				}//////////////////////////////////////////////////////////////////////////////					
				

				if(inst & ALLOC_MASK)// _A -> N
				{
					N = _A;
				}
				else if((N & FRAME_MASK) < E_frame) 
					N = (E_frame) + 8; 



			}
			break;
		
		}

		if(!A_stall)
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
					_E = (_A & A_ENV_MASK) >> A_ENV_SHIFT;
					break;
				case 2://
					_E = argS[arg_TOS] == MARK_VAL ? (retS[ret_TOS] & A_ENV_MASK) >> A_ENV_SHIFT : E;
					break;
				case 3:
					_E = _A;
					break;			
				default:
					std::cout << "Bad instruction encoding in _E \n";
					break;
				}////////////////////////////////////////////////////////////////////////////

				//If we set a new active env, we must be sure to save this frame before loading the new one
				bool RAMWriting = false;
				if(inst & E_MASK)
				{
					if((_E & FRAME_MASK) != FA_frame)
					{						
						std::copy(F, F + 8, RAM + E_frame);
						RAMWriting = true;
					}
				}

				if(inst & F_MASK)
				{
					//if we are going to write to F, we must be sure it matches E
					if((_E & FRAME_MASK) != FA_frame)
					{
						if(!RAMWriting)
						{
							//load _E 
							F_stall = true;
							RAMReading = true;
						}
						else//we are going to be writing for the next cycle so must wait to read
						{
							F_stall = true;
						}
					}
					else 
						F_stall = false;
				}
				
				if(!F_stall)
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
			
					// E inc /////////////////////////////////////////////////////////////////////
					switch((inst & E_INC_MASK) >> E_INC_SHIFT)
					{
					case 0:// no change				
						break;
					case 1://
						if((_E & CELL_MASK) == 7)
						{						
							std::copy(F, F + 8, RAM + E_frame);
							_E = N;
						}
						else _E = E + 1;
						break;
					case 2: // conditionaly inc E 
						if(argS[arg_TOS] != MARK_VAL)
						{
							if((_E & CELL_MASK) == 7)
							{							
								std::copy(F, F + 8, RAM + E_frame);
								_E = N;
							}
							_E = E + 1;
						}
						break;
					case 3:
						_E = E - 1;
						break;			
					default:
						std::cout << "Bad instruction encoding in retSinc \n";
						break;
					}//////////////////////////////////////////////////////////////////////////////
				}

				if(!A_stall && !F_stall)
				{
					

					// retS inc ///////////////////////////////////////////////////////////////////////
					switch((inst & RETS_INC_MASK) >> RETS_INC_SHIFT)
					{
					case 0:// no change				
						break;
					case 1://
						ret_TOS++;
						break;
					case 2:	
						if(argS[arg_TOS] == MARK_VAL)
							ret_TOS--;
						break;
					case 3:
						ret_TOS--;
						break;			
					default:
						std::cout << "Bad instruction encoding in retSinc \n";
						break;
					}//////////////////////////////////////////////////////////////////////////////////
					ret_TOS &= 0x1F; //we need the stacks to wrap

					// retS ///////////////////////////////////////////////////////////////////////
					switch((inst & RETS_MASK) >> RETS_SHIFT)
					{
					case 0:// no change				
						break;
					case 1://
						retS[ret_TOS] = (E << A_ENV_SHIFT) | PC;
						break;
					case 2://
						retS[ret_TOS] = PC;
						break;
					case 3:
						retS[ret_TOS] = _A;
						break;			
					default:
						std::cout << "Bad instruction encoding in _retS \n";
						break;
					}///////////////////////////////////////////////////////////////////////////////

					// argS inc ///////////////////////////////////////////////////////////////////////
					switch((inst & ARGS_INC_MASK) >> ARGS_INC_SHIFT)
					{
					case 0:// no change				
						break;
					case 1://
						arg_TOS++;
						break;
					case 2:	
						//
						break;
					case 3:
						arg_TOS--;
						break;			
					default:
						std::cout << "Bad instruction encoding in argSinc \n";
						break;
					}//////////////////////////////////////////////////////////////////////////////////
					arg_TOS &= 0x1F; //we need the stacks to wrap
	

					if(inst & ARGS_MASK)// _A -> argS
					{
						argS[arg_TOS] = _A;
					}

				}



			}
		}
		
	}


	if(RAMReading) 
	{
		std::copy(RAM + (_FA & FRAME_MASK), RAM + (_FA & FRAME_MASK) + 8, F);
		FA = _FA;
		RAMReading = false;	
	}
	else
	{

	}

	if(!A_stall & !F_stall)			
	{
		_FA = _E;
		A = _A;
		PC = _PC;
		E = _E;				
	}

	

}






void Core::DisAssemble()
{
	std::cout << "================= RAM Dissassembly ================ \n";
	while(RAM[PC] != INITVAL)
	{
		unsigned int inst = RAM[PC];
		char pattern = (inst & PTRN_MASK)>>PTRN_SHIFT;
		switch(pattern)
		{
		case PAT_LDI:
			std::cout << PC << ": \t" << "LDI " << (inst & LIT_MASK) << "\n";
			break;
		case PAT_JMPCLOS:
			std::cout << PC << ": \t" << "CLOS " << (inst & A_CODE_MASK) << "\n";			
			break;
		case PAT_ALU:
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
				default:
					std::cout << "Bad instruction encoding! \n";
					break;
			}
			break;
			default:
				std::cout << "Bad instruction encoding! \n";
				break;
		}
		PC++;
	}
}