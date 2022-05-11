with Testsuite; use Testsuite;
with Nodes.Gen_Constant;

package Nodes.Const_A is new Nodes.Gen_Constant (Cat_K     => Cat_A,
                                                 Port_K    => Port_A,
                                                 Node_Name => "const_a",
                                                 Value     => (Val => 1));
