with Testsuite; use Testsuite;
with Nodes.Gen_Print;

package Nodes.Print_B is new Nodes.Gen_Print (Cat_K     => Cat_B,
                                              Port_K    => Port_B,
                                              Node_Name => "print_b");
