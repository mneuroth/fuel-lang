
#include "Utils.h"
#include "Lisp.h"

namespace CsLisp
{

	/*static*/ void ShowVersion(std::shared_ptr<TextWriter> output)
	{
		output->WriteLine();
		output->WriteLine(Lisp::Name + " " + Lisp::Version + " (for " + Lisp::Platform + ") from " + Lisp::Date + ", " + Lisp::Copyright);
		output->WriteLine();
	}

	/*static*/ void ShowAbout(std::shared_ptr<TextWriter> output)
	{
		ShowVersion(output);
		output->WriteLine(Lisp::Info);
		output->WriteLine();
	}

}
