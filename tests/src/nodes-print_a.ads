with Testsuite; use Testsuite;
with Nodes.Gen_Print;

package Nodes.Print_A is new Nodes.Gen_Print (Cat_K     => Cat_A,
                                              Port_K    => Port_A,
                                              Node_Name => "print_a");
