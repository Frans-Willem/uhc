// Adjusted from https://gist.github.com/ArildF/911288
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace TailCalls
{
	class Program
	{
		static int Main(string[] args)
		{
			var filename = args.FirstOrDefault();

			if (filename == null)
			{
				Usage();
				return 0;
			}
			try
			{
				ProcessModule(filename);
			}
			catch (Exception ex)
			{
				Console.WriteLine(ex);
				return 1;
			}

			return 0;
		}

		private static void ProcessModule(string filename)
		{
			var moduleDefinition = ModuleDefinition.ReadModule(filename);
			var methods = from type in moduleDefinition.Types
			              from method in type.Methods
			              select method;

			foreach (var methodDefinition in methods)
			{
				ProcessMethod(methodDefinition);
			}

			moduleDefinition.Write(filename);
		}

		private static void ProcessMethod(MethodDefinition methodDefinition)
		{
			var body = methodDefinition.Body;
			if (body == null) {
				return;
			}
			var processor = body.GetILProcessor();

			bool seenRet = false;
			foreach(var instruction in body.Instructions.Reverse())
			{
				if (instruction.OpCode == OpCodes.Ret)
				{
					seenRet = true;
				}
				else if (seenRet && (instruction.OpCode == OpCodes.Call || instruction.OpCode == OpCodes.Calli || instruction.OpCode == OpCodes.Callvirt))
				{
					var tailCall = processor.Create(OpCodes.Tail);
					processor.InsertBefore(instruction, tailCall);
					seenRet = false;
				}
				else if (instruction.OpCode != OpCodes.Nop)
				{
					seenRet = false;
				}
			}
		}


		private static void Usage()
		{
			Console.WriteLine(@"Usage: TailCall Path\To\Assembly.dll");
		}
	}
}
