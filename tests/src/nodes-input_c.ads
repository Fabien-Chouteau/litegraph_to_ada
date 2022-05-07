with Testsuite; use Testsuite;
with Nodes.Gen_Input;

package Nodes.Input_C is new Nodes.Gen_Input (Cat_K     => Cat_C,
                                              Port_K    => Port_C,
                                              Node_Name => "input_c");
