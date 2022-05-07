with Testsuite; use Testsuite;
with Nodes.Gen_Bridge;

package Nodes.Bridge_B_C is new Nodes.Gen_Bridge (Cat_K      => Cat_B,
                                                  In_Port_K  => Port_B,
                                                  Out_Port_K => Port_C,
                                                  Node_Name  => "bridge_b_c");
