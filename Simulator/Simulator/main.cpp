#include <stdio.h>

#include "core.h"




int main(int argc, char **argv)
{
	Core core;
	size_t cnt = core.LoadRAM(argv[1]);
	if(cnt)
	{		
		if((int)argv[2][0] == 'r')
			core.Run(false);
		if((int)argv[2][0] == 's')
			core.Run(true);
		else if((int)argv[2][0] == 'd')
			core.Disassemble(cnt);
	}
	else
		std::cout << "error: could not open file \n";
}

