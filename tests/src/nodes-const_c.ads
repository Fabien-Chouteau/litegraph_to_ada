with Testsuite; use Testsuite;
with Nodes.Gen_Constant;

package Nodes.Const_C is new Nodes.Gen_Constant (Cat_K     => Cat_C,
                                                 Port_K    => Port_C,
                                                 Node_Name => "const_c",
                                                 Value     => (Val => 3));
