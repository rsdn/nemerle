using System;
using System.Collections.Generic;
using System.Text;

using Tests;

namespace ConsoleTest
{
	class Program
	{
		static void Main()
		{
			Test1 test = new Test1();
			test.Init();

			test.Complete_in_lambda();
			test.Overload1();
			test.GetMethodTip();
			test.QuickTips();
			test.QuickTip();
			test.Property_location();
			test.Complete_in_return_type_1();
			test.Complete_in_return_type_2();
			test.Complete_in_return_type_3();
			test.Complete_in_return_type_4();
			test.QuickTip_ForMacro();
			test.Complete_in_match_variant_6();
			test.Complete_in_match_variant_5();
			test.Complete_in_match_variant_4();
			test.Complete_System_Collections_Generic_List__Collections();
			test.Complete_namespace_2();
			test.Complete_2();
			test.Complete_empty();
			test.Complete_Complete_expr();
			test.Complete_type_escalation_3();
			test.Complete_Complete_aliased_type();
			test.Complete_ExtensionMethod_1();
			test.QuickTip_Imperative();
			//test.QuickTip_StackOverflow();
			test.QuickTip_ArgPattern();
			test.QuickTip_CtorArg();
			//test.Check_partial_region();
			//test.Check_region_location();
			test.QuickTip_TupleProp();
			test.QuickTip_TupleMethod();
			test.Complete_enum();
			test.SimpleSourceTextManager_GetLine();
			test.SimpleSourceTextManager_GetLine_EOF();
			test.SimpleSourceTextManager_GetLineAndColumn();
			test.SimpleSourceTextManager_GetRegion_block_1();
			test.SimpleSourceTextManager_GetRegion_block_2();
			test.SimpleSourceTextManager_GetRegion_block_3();
			test.Hint_on_return();
			test.Hint_in_body_of_implicit_match();
			test.QuickTip_ClassExtension();
			test.Complete_generic_type_cast();
			test.Complete_type_cast();
			test.Complete_namespace_1();
			test.Complete_vars_with_same_name();
			test.Complete_in_match_variant_3();
			test.Complete_in_match_variant_2();
			test.Complete_in_match_variant_1();
			test.Complete_type_escalation_2();
			test.Complete_type_escalation();
			test.Complete_qualidend();
			test.Complete_GlabalNs_in_NestedNs();
			test.CompleteInType_1();
			test.CompleteInUsing_1();
			test.FindByLocation_Method_Main();
		}
	}
}
