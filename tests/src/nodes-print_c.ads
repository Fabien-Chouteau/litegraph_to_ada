with Testsuite; use Testsuite;
with Nodes.Gen_Print;

package Nodes.Print_C is new Nodes.Gen_Print (Cat_K     => Cat_C,
                                              Port_K    => Port_C,
                                              Node_Name => "print_c");
