with Testsuite; use Testsuite;
with Nodes.Gen_Input;

package Nodes.Input_B is new Nodes.Gen_Input (Cat_K     => Cat_B,
                                              Port_K    => Port_B,
                                              Node_Name => "input_b");
