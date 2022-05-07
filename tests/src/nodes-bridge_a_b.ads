with Testsuite; use Testsuite;
with Nodes.Gen_Bridge;

package Nodes.Bridge_A_B is new Nodes.Gen_Bridge (Cat_K      => Cat_A,
                                                  In_Port_K  => Port_A,
                                                  Out_Port_K => Port_B,
                                                  Node_Name  => "bridge_a_b");
