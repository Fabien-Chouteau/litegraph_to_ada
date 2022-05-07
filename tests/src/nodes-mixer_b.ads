with Testsuite; use Testsuite;
with Nodes.Gen_Mixer;

package Nodes.Mixer_B is new Nodes.Gen_Mixer (Cat_K     => Cat_B,
                                              Port_K    => Port_B,
                                              Node_Name => "mixer_b");
