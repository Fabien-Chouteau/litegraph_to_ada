with Testsuite; use Testsuite;
with Nodes.Gen_Input;

package Nodes.Input_A is new Nodes.Gen_Input (Cat_K     => Cat_A,
                                              Port_K    => Port_A,
                                              Node_Name => "input_a");
