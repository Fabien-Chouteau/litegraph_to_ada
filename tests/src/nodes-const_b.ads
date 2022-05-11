with Testsuite; use Testsuite;
with Nodes.Gen_Constant;

package Nodes.Const_B is new Nodes.Gen_Constant (Cat_K     => Cat_B,
                                                 Port_K    => Port_B,
                                                 Node_Name => "const_b",
                                                 Value     => (Val => 2));
